module Main where

import Control.Exception (AsyncException (UserInterrupt))
import Parser
import Query
import Util

-- TODO: Concurrency

manageConnections :: Socket -> IO ()
manageConnections sock =
    handle
        ( \UserInterrupt -> do
            seq id close sock
            putStrLn "\nUser Interrupt. Stopping Now"
        )
        loop
  where
    loop =
        forever $
            do
                (conn, addr) <- accept sock
                r <- recv conn 1024
                -- managequeries ((head . parseH) (b r)) conn
                close conn

main :: IO ()
main = do
    putStrLn "Starting"
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 1337 $ tupleToHostAddress (0, 0, 0, 0))
    listen sock 4
    manageConnections sock
