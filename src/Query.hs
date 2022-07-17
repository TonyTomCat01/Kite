{-# LANGUAGE ScopedTypeVariables #-}

module Query where

import Control.Exception
import Control.Monad
import Util

-- NOTE: I will refactor the mess below. Feel free to refactor, and submit a pr.

data Query
    = GET
    | HEAD
    | NotImpleMented
    deriving (Show)

p :: String -> Query
p "GET" = GET
p "HEAD" = HEAD
p _ = NotImpleMented

data Response = Response
    { statuscode :: String
    , headers :: String
    , content :: String
    }
    deriving (Show)

con t = statuscode t <> headers t <> content t

sendStr conn = sendAll conn . packStr

query _ "/" conn = sendStr conn (msg ++ "Content-Type: text/html\n\n" ++ d)
query NotImpleMented f conn = do
    (sendAll conn . packStr)
        ( con
            Response
                { statuscode = "HTTP/1.1 501 Not Implemented\n"
                , headers = "\n"
                , content = "\n"
                }
        )
query GET f conn = do
    handle
        fileError
        ( do
            t <- (readFile . tail) f
            (sendAll conn . packStr)
                ( con
                    Response
                        { statuscode = "HTTP/1.1 200 OK\n"
                        , headers = "Content-Type: text/" <> (tail . takeExtension) f
                        , content = "\n\n" <> t
                        }
                )
        )
  where
    fileError (e :: SomeException) = do
        t <- readFile "notFound.html"
        sendStr
            conn
            ( con
                Response
                    { statuscode = "HTTP/1.1 404 Not Found\n"
                    , headers = "Content-Type: text/html\n\n"
                    , content = t
                    }
            )
query HEAD f conn = do
    handle
        fileError
        ( do
            t <- (readFile . tail) f
            sendStr
                conn
                ( con
                    Response
                        { statuscode = "HTTP/1.1 200 OK\n"
                        , headers = "Content-Type: text/html\n\n"
                        , content = ""
                        }
                )
        )
  where
    fileError (e :: SomeException) = do
        t <- readFile "notFound.html"
        sendStr
            conn
            ( con
                Response
                    { statuscode = "HTTP/1.1 404 Not Found\n"
                    , headers = "Content-Type: text/html\n\n"
                    , content = ""
                    }
            )

managequeries i conn =
    let (a, b, c) = i
     in query a b conn
