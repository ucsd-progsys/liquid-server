{-# LANGUAGE OverloadedStrings #-}

module Language.Liquid.Server.Types (
  -- * Type of the Query
    Query   
   
  -- * Type of the Result
  , Result
  
  -- * Dummy Result Returned Upon Failure
  , dummyResult
  
  -- * Function to Compute Query Results
  , queryResult 
  ) where

import           Control.Monad          (mzero)
import           Control.Applicative    ((<$>))
import           Data.Maybe
import           Data.Aeson                 hiding (Result)
-- import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict  as M

data Query  = Q { program :: LB.ByteString } 
type Result = Value

instance FromJSON Query where
  parseJSON (Object v) = Q <$> v .: "program" 
  parseJSON _          = mzero

instance ToJSON Query where
  toJSON (Q p) = object ["program" .= p]

dummyResult :: Result 
dummyResult = fromJust $ decode "{\"status\" : \"crash\" }"

echoResult  :: (ToJSON a) => a -> Result
echoResult x = Object $ M.fromList [ ("status", String "crash")
                                   , ("stuff" , toJSON x)       ]

queryResult :: Query -> Result
queryResult = echoResult  


