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

import           System.Process           (system)
import           Control.Monad          (mzero)
import           Control.Applicative    ((<$>))
import           Data.Maybe
import           Data.Aeson                 hiding (Result)
-- import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict  as M

-----------------------------------------------------------------
-- Configuration Parameters -------------------------------------
-----------------------------------------------------------------

config :: Config
config = C { srcSuffix  = "hs" 
           , srcChecker = "liquid"
           , cmdPrefix  = "GHC_PACKAGE_PATH=/home/rjhala/research/liquid/.hsenv_liquid/ghc/lib/ghc-7.6.3/package.conf.d"
           , log        = "resources/sandbox/log"
           }

-----------------------------------------------------------------
-- Core Data Types ----------------------------------------------
-----------------------------------------------------------------

data Config = C { srcSuffix  :: String
                , srcChecker :: FilePath
                , cmdPrefix  :: String
                , log        :: FilePath 
                }

data Query  = Q { program :: LB.ByteString } 

data Files  = F { src  :: FilePath
                , json :: FilePath
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
queryResult :: Query -> Snap Result
------------------------------------------------------------------------

-- import qualified Control.Exception     as Ex
-- queryResult = echoResult  

queryResult q 
  = do f <- srcFile q 
       LB.writeFile (src f) (program q)
       let cmd = makeCommand config (src f)
       logError $ "EXEC: " ++ cmd
       system cmd
       HEREHEREHEREHERE

queryFiles   :: Query -> IO Files
queryFiles q = do t     <- getTimeStamp
                  return $ F (srcFile t) (jsonFile t)

srcFile      = (`addExtension` (config srcSuffix))
jsonFile     = (`addExtension` "json" . srcFile) 

-- executeShellCommand phase cmd 
--   = do whenLoud $ putStrLn $ "EXEC: " ++ cmd 
--        Ex.bracket_ (startPhase Loud phase) (donePhase Loud phase) $ system cmd


echoResult  :: (ToJSON a) => a -> Result
echoResult x = Object $ M.fromList [ ("status", String "crash")
                                   , ("stuff" , toJSON x)       ]

------------------------------------------------------------------------
makeCommand :: Config -> FilePath -> String
------------------------------------------------------------------------
makeCommand config target 
  = intercalate " " 
    [ "LANG=en_US.UTF-8"
    , config cmdPrefix
    , config checker
    , target 
    , ">"
    , config log 
    , "2>&1" 
    ]

