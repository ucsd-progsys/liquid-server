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

import           System.FilePath        (addExtension)
import           System.Process         (system)
import           Control.Monad          (mzero)
import           Control.Applicative    ((<$>))
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe
import           Data.List              (intercalate)
import           Data.Aeson                 hiding (Result)
-- import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict  as M
import           Data.Time.Clock.POSIX

-----------------------------------------------------------------
-- Configuration Parameters -------------------------------------
-----------------------------------------------------------------

config :: Config
config = C { srcSuffix  = "hs" 
           , srcChecker = "liquid"
           , cmdPrefix  = "GHC_PACKAGE_PATH=/home/rjhala/research/liquid/.hsenv_liquid/ghc/lib/ghc-7.6.3/package.conf.d"
           , logFile    = "resources/sandbox/log"
           }

-----------------------------------------------------------------
-- Core Data Types ----------------------------------------------
-----------------------------------------------------------------

data Config = C { srcSuffix  :: String
                , srcChecker :: FilePath
                , cmdPrefix  :: String
                , logFile    :: FilePath 
                }

data Query  = Q { program :: LB.ByteString } 

data Files  = F { srcFile  :: FilePath
                , jsonFile :: FilePath
                }

type Result = Value

instance FromJSON Query where
  parseJSON (Object v) = Q <$> v .: "program" 
  parseJSON _          = mzero

instance ToJSON Query where
  toJSON (Q p) = object ["program" .= p]

dummyResult :: Result 
dummyResult = fromJust $ decode "{\"status\" : \"crash\" }"

-- | Executing the Binary

------------------------------------------------------------------------
queryResult :: Query -> IO Result
------------------------------------------------------------------------
queryResult q 
  = do f <- queryFiles q 
       LB.writeFile (srcFile f) (program q)
       let cmd = makeCommand config (srcFile f)
       -- logError $ "EXEC: " ++ cmd
       system cmd
       byteStringResult <$> LB.readFile (jsonFile f)

byteStringResult = fromMaybe dummyResult . decode

queryFiles   :: Query -> IO Files
queryFiles q 
  = do t     <- show <$> getPOSIXTime 
       return $ F (sourceName t) (jsonName t)

sourceName = (`addExtension` (srcSuffix config))
jsonName   = (`addExtension` "json") . sourceName 

-- executeShellCommand phase cmd 
--   = do whenLoud $ putStrLn $ "EXEC: " ++ cmd 
--        Ex.bracket_ (startPhase Loud phase) (donePhase Loud phase) $ system cmd
-- 
-- import qualified Control.Exception     as Ex
-- queryResult = echoResult  



echoResult  :: (ToJSON a) => a -> Result
echoResult x = Object $ M.fromList [ ("status", String "crash")
                                   , ("stuff" , toJSON x)       ]

------------------------------------------------------------------------
makeCommand :: Config -> FilePath -> String
------------------------------------------------------------------------
makeCommand config target 
  = intercalate " " 
    [ "LANG=en_US.UTF-8"
    , cmdPrefix  config
    , srcChecker config
    , target 
    , ">"
    , logFile    config 
    , "2>&1" 
    ]

