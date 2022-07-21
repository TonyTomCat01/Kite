module Main where

import Control.Concurrent
import Control.Exception (AsyncException (UserInterrupt))
import Log
import Parser
import Query
import Util

-- TODO: Concurrency and Optimizations

manageConnections :: Socket -> IO ()
manageConnections sock =
    handle
        ( \UserInterrupt -> do
            close sock
            putStrLn "\nUser Interrupt. Stopping Now"
        )
        loop
  where
    loop =
        forever $
            do
                (conn, addr) <- accept sock
                logVal ("Connection from " <> show addr)
                r <- recv conn 1024
                _ <- forkIO (managequeries (returnFstParseUnpack r) conn)
                return ()
    returnFstParseUnpack = return . fst . parseHttp . unpackStr

main :: IO ()
main = do
    logVal "\n\nStarting"
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 1337 $ tupleToHostAddress (0, 0, 0, 0))
    listen sock maxConns
    manageConnections sock
