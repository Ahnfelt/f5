{-# LANGUAGE OverloadedStrings #-}
module Main where

-- Consider using: https://github.com/fpco/http-reverse-proxy

import Network.Wai (responseLBS, Application, Request (..))
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status302)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.ReverseProxy
import Network.HTTP.Conduit (newManager, tlsManagerSettings)
import qualified Data.ByteString.Char8 as S
import Control.Monad.IO.Class (liftIO)
import System.Environment
import System.Exit

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
    start (cPort configuration)

start port = do
    putStrLn $ "Listening on port " ++ show port
    let destination = ProxyDest "www.example.com" 80
    run port (proxy2 destination)

handler destination@(ProxyDest host port) request = do
    -- WPRResponse WAI.Response
    let host' = S.pack (S.unpack host ++ (if port == 80 then "" else ":" ++ show port))
    let headers' = filter (\(k, v) -> k /= "Host") (requestHeaders request)
    let header = ("Host", host')
    let headers'' = header : headers'
    let request' = request { requestHeaders = headers'' }
    return (WPRModifiedRequest request' destination)
   
--proxy2 :: Request -> ResourceT IO Response
proxy2 destination req sendResponse = do
    manager <- liftIO $ newManager tlsManagerSettings
    waiProxyToSettings
        (handler destination)
        def -- { wpsOnExc = onExc, wpsTimeout = Nothing}
        manager 
        req
        sendResponse 
    where
        onExc _ _ = return $ responseLBS
            status200
            [ ("content-type", "text/html")
            , ("Refresh", "1")
            ]
            "<h1>App not ready, please refresh</h1>"    
    
--app :: Application
--app request respond =
    --respond $ do 
        --responseLBS status200 [(hContentType, "text/plain")] "Hello world!"
      --  responseLBS status302 [("Location", "http://www.google.com/"), (hContentType, "text/plain")] "Foo!"
