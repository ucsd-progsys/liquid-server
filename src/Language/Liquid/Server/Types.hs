{-# LANGUAGE OverloadedStrings #-}

module Language.Liquid.Server.Types (
    Query  (..) 
  , Result 
  , Files  (..)
  , Config (..)
  , dummyResult
  ) where

-- import           System.Exit            (ExitCode)
-- import           System.Directory       (doesFileExist)
-- import           System.FilePath        ((</>), addExtension)
-- import           System.Process         (system)
-- import           Control.Monad.IO.Class (liftIO)
-- import           Data.List              (intercalate)
-- import qualified Data.ByteString      as B


import           Control.Monad          (mzero)
import           Control.Applicative    ((<$>))
import           Data.Maybe
import           Data.Aeson                 hiding (Result)
import qualified Data.ByteString.Lazy as LB

-----------------------------------------------------------------
-- Core Data Types ----------------------------------------------
-----------------------------------------------------------------

data Config = C { srcSuffix   :: String
                , srcChecker  :: FilePath
                , cmdPrefix   :: String
                , sandboxPath :: FilePath
                }

data Query  = Q { program :: LB.ByteString } 

data Files  = F { srcFile  :: FilePath
                , jsonFile :: FilePath
                }

type Result = Value

----------------------------------------------------------------
-- JSON Serialization ------------------------------------------
----------------------------------------------------------------

instance FromJSON Query where
  parseJSON (Object v) = Q <$> v .: "program" 
  parseJSON _          = mzero

instance ToJSON Query where
  toJSON (Q p) = object ["program" .= p]

dummyResult :: Result 
dummyResult = fromJust $ decode "{\"status\" : \"crash\" }"


------------------------------------------------------------------------
-- Random stuff for debugging ------------------------------------------
------------------------------------------------------------------------

-- Can instead use exit code from liquid...
-- resultExit Safe        = ExitSuccess
-- resultExit (Unsafe _)  = ExitFailure 1
-- resultExit _           = ExitFailure 2

-- echoResult  :: (ToJSON a) => a -> Result
-- echoResult x = Object $ M.fromList [ ("status", String "crash")
--                                    , ("stuff" , toJSON x)       ]
