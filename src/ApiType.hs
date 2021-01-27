{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
module ApiType ( 
    InfoAPI,
    Info (..),
    getInfo
) where

import GHC.Generics
import Servant.API
import Data.Aeson

type InfoAPI = "info" :> Get '[JSON] Info

data Info = Info {
    version :: String,
    lang :: String
} deriving (Eq, Show, Generic)

instance ToJSON Info


dummyData :: Info
dummyData = Info { version = "0.0.1", lang = "Haskell" }

getInfo :: Monad m => m Info
getInfo = return dummyData
