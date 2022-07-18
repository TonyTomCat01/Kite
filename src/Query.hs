{-# LANGUAGE ScopedTypeVariables #-}

module Query where

import Control.Exception
import Control.Monad
import Data.Either
import Data.Time
import Util

-- NOTE: I will refactor the mess below. Feel free to refactor, and submit a pr.

data Method
    = GET
    | HEAD
    | NotImpleMented
    deriving (Show, Eq)

p :: String -> Method
p "GET" = GET
p "HEAD" = HEAD
p _ = NotImpleMented

data Response = Response String String String deriving (Show)

type Error = String

data Result = Either Error Response
    deriving (Show)

sendStr conn = sendAll conn . packStr . con
con (Response status headers content) = status <> headers <> content

safeRead :: String -> IO String
safeRead f = do
    t <- try (readFile f) :: IO (Either IOException String)
    return $ fromRight "" t

noSuchFile method (e :: SomeException) = do
    t <- safeRead notFound
    time <- getCurrentTime
    if method == HEAD
        then
            return $
                Response
                    "HTTP/1.1 404 Not Found\n"
                    ( "Date: "
                        <> show time
                        <> "\nContent-Type: text/html\n"
                        <> "Content-Length: "
                        <> show (length t)
                    )
                    "\n\n"
        else
            if method == GET
                then
                    return $
                        Response
                            "HTTP/1.1 404 Not Found\n"
                            ( "Date: "
                                <> show time
                                <> "\nContent-Type: text/html\n"
                                <> "Content-Length: "
                                <> show (length t)
                                <> "\n\n"
                            )
                            t
                else
                    return $
                        Response
                            "HTTP/1.1 501 Not Implemented\n"
                            ( "Date: "
                                <> show time
                            )
                            "\n\n"

query GET f = do
    time <- getCurrentTime
    root <- readFile rootFile
    if f == "/"
        then
            return $
                Response
                    msg
                    ( "Date: "
                        <> show time
                        <> "\nContent-Type: text/html\nContent-Length: "
                        <> show (length root)
                        <> "\n\n"
                    )
                    root
        else
            handle
                (noSuchFile GET)
                ( do
                    t <- (readFile . tail) f
                    return $
                        Response
                            "HTTP/1.1 200 OK\n"
                            ( "Date: "
                                <> show time
                                <> "\nContent-Type: text/"
                                <> (tail . takeExtension) f
                                <> "\nContent-Length: "
                                <> show
                                    (length t)
                            )
                            ("\n\n" <> t)
                )
query HEAD f = do
    time <- getCurrentTime
    root <- readFile rootFile
    if f == "/"
        then
            return $
                Response
                    msg
                    ( "Date: "
                        <> show time
                        <> "\nContent-Type: text/html\nContent-Length: "
                        <> (show . length) root
                    )
                    "\n\n"
        else
            handle
                (noSuchFile HEAD)
                ( do
                    t <- (readFile . tail) f
                    return $
                        Response
                            "HTTP/1.1 200 OK\n"
                            ( "Date: "
                                <> show time
                                <> "\nContent-Type: text/"
                                <> (tail . takeExtension) f
                                <> "\nContent-Length: "
                                <> show (length t)
                            )
                            "\n\n"
                )
query NotImpleMented f = do
    time <- getCurrentTime
    return $
        Response
            "HTTP/1.1 501 Not Implemented\n"
            ( "Date: "
                <> show time
                <> "\nContent-Length: 0\n"
            )
            "\n"

managequeries i conn = do
    (a, b, c) <- i
    r <- query (p a) b
    sendStr conn r
