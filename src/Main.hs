{-# LANGUAGE OverloadedStrings #-}

-- module Main where

-- import           Control.Applicative
-- import           Control.Monad
-- import qualified Data.ByteString      as B
-- import qualified Data.ByteString.Lazy as LB


import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Language.Liquid.Server.Types 
import           Data.Aeson           hiding (Result)
import           Data.Maybe

main      :: IO ()
main      = quickHttpServe site

site      :: Snap ()
site      = route [ ("static", serveDirectory "resources/static/")
                  , ("check" , method POST queryH)
                  , (""      , defaultH)
                  ]

defaultH  :: Snap ()
defaultH  = writeLBS "Liquid Demo Server"

queryH        :: Snap ()
queryH        = writeLBS . encode . getResult =<< readBody
  where
    readBody  = readRequestBody maxLen
    maxLen    = 1000000
    getResult = fromMaybe dummyResult . fmap queryResult . decode

-- echoHandler = do
--     param <- getParam "echoparam"
--     maybe (writeBS "must specify echo/param in URL")
--           writeBS param
