module Main where

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
                managequeries ((return . fst . parseH . unpackStr) r) conn
                close conn

safeHead c list =
    if (not . null) list
        then head list
        else c

main :: IO ()
main = do
    logVal "Starting"
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 1337 $ tupleToHostAddress (0, 0, 0, 0))
    listen sock 4
    manageConnections sock
