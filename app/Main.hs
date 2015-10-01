{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai (responseLBS, Application, responseFile, Request (..))
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status302)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Method (methodGet)
import Network.HTTP.ReverseProxy
import Network.HTTP.Conduit (newManager, tlsManagerSettings)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad.IO.Class (liftIO)
import System.Environment
import System.Exit
import System.Process (shell, readCreateProcessWithExitCode)
import Data.List
import Data.Char (isDigit)

data Configuration = Configuration {
    cPort :: Int,
    cPattern :: String,
    cCommand :: String,
    cUrl :: String,
    cErrorScriptUrl :: Maybe String
} deriving Show

main = do
    arguments <- getArgs
    configuration <- case arguments of
        [port, pattern, command, url] -> return Configuration { 
            cPort = read port, 
            cPattern = pattern, 
            cCommand = command, 
            cUrl = url, 
            cErrorScriptUrl = Nothing 
        }
        [port, pattern, command, url, errorScriptUrl] -> return Configuration { 
            cPort = read port, 
            cPattern = pattern, 
            cCommand = command, 
            cUrl = url, 
            cErrorScriptUrl = Just errorScriptUrl
        }
        _ -> do
            putStrLn "USAGE:"
            putStrLn ""
            putStrLn "  f5 port pattern command url [errorScriptUrl]"
            putStrLn ""
            putStrLn "            port - The port that the f5 web server will listen on."
            putStrLn "             url - The root URL or file that is redirected to."
            putStrLn "         pattern - Paths that match this regular expression causes the command"
            putStrLn "                   to be run before redirecting."
            putStrLn "         command - The command to be run. If it returns a non-zero status code,"
            putStrLn "                   a HTML error is served instead of redirecting."
            putStrLn "  errorScriptUrl - If specified, this script will be embedded in the HTML error,"
            putStrLn "                   and can be used to parse and present the output of the"
            putStrLn "                   command in a friendly way."
            putStrLn ""
            putStrLn "EXAMPLE:"
            putStrLn ""
            putStrLn "  f5 8080 / \"sbt fastOptJS\" index.html sbt-error-f5.js"
            putStrLn ""
            die "ERROR: Wrong number of arguments."
    print configuration
    start configuration

start configuration = do
    putStrLn $ "Listening on port " ++ show (cPort configuration)
    run (cPort configuration) (app configuration)

-- Check if it's a directory and serve the appropriate subpath instead
fileHandler fileName request = do
    return $ WPRResponse $ responseFile status200 [(hContentType, "text/html")] fileName Nothing

destinationHandler host port request = do
    let host' = host ++ (if port == 80 then "" else ":" ++ show port)
    let headers' = filter (\(k, v) -> k /= "Host") (requestHeaders request)
    let header = ("Host", S.pack host')
    let headers'' = header : headers'
    let request' = request { requestHeaders = headers'' }
    return (WPRModifiedRequest request' (ProxyDest (S.pack host) port))

delegateHandler configuration request = do
    let url = cUrl configuration
    let clean n url = filter (/= '/') (drop n url)
    let (http, hostPort) = 
            if "http://" `isPrefixOf` url then (Just "http", clean 7 url)
            else if "https://" `isPrefixOf` url then (Just "https", clean 8 url)
            else if ":" `isInfixOf` url then (Just "http", clean 0 url)
            else (Nothing, url)
    let host = takeWhile (/= ':') hostPort
    let port = reverse (takeWhile (/= ':') (reverse hostPort))
    let port' = if ":" `isInfixOf` hostPort && all isDigit port then read port else 80
    case http of
        Just _ -> destinationHandler host port' request
        Nothing -> fileHandler url request
    
exitHandler configuration exitCode stdout stderr = do
    -- TODO: Escaping, read script from URL
    script <- case cErrorScriptUrl configuration of
        Nothing -> return ""
        Just f -> do
            s <- readFile f
            return ("<script>" ++ s ++ "</script>")
    let html = "<html><head><title>Error while running command</title></head><body><code id=\"errorcode\">Error code: <span>" ++ (show (exitCode :: Int)) ++ "</span></code><br /><br /><pre id=\"stdout\">" ++ stdout ++ "</pre><br /><br /><pre id=\"stderr\" style=\"color: #a00000\">" ++ stderr ++ "</pre>" ++ script ++ "</html>"
    return $ WPRResponse $ responseLBS status200 [(hContentType, "text/html")] (L.pack html)
    
handler configuration request = do
    -- TODO: Better check of the pattern
    if requestMethod request == methodGet && rawPathInfo request == S.pack (cPattern configuration)
        then do
            putStrLn ("Running command: " ++ (cCommand configuration))
            let process = shell (cCommand configuration)
            (exitCode, stdout, stderr) <- readCreateProcessWithExitCode process ""
            case exitCode of
                ExitFailure code -> exitHandler configuration code stdout stderr
                ExitSuccess -> delegateHandler configuration request
        else delegateHandler configuration request

app configuration req sendResponse = do
    manager <- liftIO $ newManager tlsManagerSettings
    waiProxyToSettings
        (handler configuration)
        def
        manager 
        req
        sendResponse 
