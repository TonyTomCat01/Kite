{-# LANGUAGE ScopedTypeVariables #-}

module Query where

import Control.Exception
import Control.Monad
import Data.Bits (Bits (xor))
import Data.Either
import Data.Time
import Util

-- NOTE: I will refactor the mess below. Feel free to refactor, and submit a pr.

data Method
    = GET
    | HEAD
    | NotImpleMented
    deriving (Show, Eq)

method x
    | x == "HEAD" = HEAD
    | x == "GET" = GET
    | otherwise = NotImpleMented

data Response = Response String String String deriving (Show)

sendStr conn = sendAll conn . packStr . con
con (Response status headers content) = status <> headers <> content

safeRead :: String -> IO (String, String, String, String, String)
safeRead file = do
    time <- getCurrentTime
    state <- try ((readFile . tail) file) :: IO (Either SomeException String)
    if file == "/"
        then do
            t <- readFile rootFile
            return ("200 OK", show time, "html", showlen t, t)
        else
            if null state
                then do
                    t <- readFile notFound
                    return ("404 Not Found", show time, "html", showlen t, t)
                else return ("200 OK", show time, showext file, (showlen . fromRight "") state, fromRight "" state)
  where
    showlen = show . length
    showext = tail . takeExtension

query NotImpleMented f = do
    time <- getCurrentTime
    return $
        Response
            "HTTP/1.1 501 NotImpleMented"
            ("\nDate: " <> show time <> "\nContent-Length: 0\n")
            "\n\n"
query HEAD f = do
    (status, time, extension, length, _) <- safeRead f
    return $
        Response
            ("HTTP/1.1 " <> status)
            ("\nDate: " <> time <> "\nContent-Type: text/" <> extension <> "\nContent-Length: " <> length)
            "\n\n"
query GET f = do
    (status, time, extension, length, content) <- safeRead f
    return $
        Response
            ("HTTP/1.1 " <> status)
            ("\nDate: " <> time <> "\nContent-Type: text/" <> extension <> "\nContent-Length: " <> length)
            ("\n\n" <> content)

managequeries i conn = do
    (a, b, c) <- i
    if null a
        then print "Invalid Request"
        else do
            r <- query (method a) b
            sendStr conn r
