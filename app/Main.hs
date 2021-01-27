{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import Lib
import ApiType
import Streamly
import Streamly.Prelude
import Servant
-- import Network.Wai
import Network.Wai.Handler.Warp

data Coords = Coords {
    x :: Int,
    y :: Int
} deriving Show

data Object optType = Object {
    pos :: Coords,
    opt :: optType
} deriving Show

data Vehicle optType = Vehicle {
    pos :: Coords,
    opt :: optType
} deriving Show

class Element e where -- type class
    moveUp :: e -> e
    moveDn :: e -> e

instance Element (Object a) where -- instance
    moveUp e = Object { pos = Coords { x = x ((pos :: Object a -> Coords) e), y = y ((pos :: Object a -> Coords) e) + 1 }, opt = (opt :: Object a -> a) e }
    moveDn e = Object { pos = Coords { x = x ((pos :: Object a -> Coords) e), y = y ((pos :: Object a -> Coords) e) - 1 }, opt = (opt :: Object a -> a) e }

instance Element (Vehicle a) where -- instance #2
    moveUp e = Vehicle { pos = Coords { x = x ((pos :: Vehicle a -> Coords) e), y = y ((pos :: Vehicle a -> Coords) e) + 1 }, opt = (opt :: Vehicle a -> a) e }
    moveDn e = Vehicle { pos = Coords { x = x ((pos :: Vehicle a -> Coords) e), y = y ((pos :: Vehicle a -> Coords) e) - 1 }, opt = (opt :: Vehicle a -> a) e }

-- (a -> b) -> m a -> m b
instance Functor Object where
    fmap f (Object a b) = Object a (f b)

-- (m a -> b) -> m a -> m b
instance Applicative Object where
    pure a = Object (Coords { x = 0, y = 0}) a
    (Object _ f) <*> (Object a b) = Object a (f b)

-- (a -> b) -> m a -> b
instance Monad Object where
    (Object _ a) >>= f = f a

server :: Server InfoAPI
server = ApiType.getInfo

proxy :: Proxy InfoAPI
proxy = Proxy

app :: Application
app = serve proxy server

port :: Int
port = 8081

main :: IO ()
main = do
    run port app
