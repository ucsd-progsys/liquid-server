{-# LANGUAGE OverloadedStrings #-}

module Language.Liquid.Server.Types (
    -- * Configuration
    Config (..)
   , Files  (..)

    -- * Query Type 
  , Query (..) 
  
    -- * Response Type
  , Result

    -- * Canned Responses
  , dummyResult, okResult, errResult
  ) where

import           Control.Monad          (mzero)
import           Control.Applicative    ((<$>), (<*>))
-- import           Data.Maybe
import           Data.Aeson                 hiding (Result)
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as M
-----------------------------------------------------------------
-- Core Data Types ----------------------------------------------
-----------------------------------------------------------------

data Config = Config { 
    toolName    :: String     -- used to lookup resources/custom/toolName 
  , srcSuffix   :: String     -- hs, js etc.
  , srcChecker  :: FilePath   -- checker binary; must be in your $PATH 
  , cmdPrefix   :: String     -- extra command line params to be passed to `srcChecker`
  , themeFile   :: FilePath   -- /ace/js/theme-THEMEFILE.js
  , modeFile    :: FilePath   -- /ace/js/mode-MODEFILE.js
  }

data Files   = Files { 
    srcFile  :: FilePath
  , jsonFile :: FilePath
  }

-----------------------------------------------------------------
-- "REST" Queries ----------------------------------------------- 
-----------------------------------------------------------------

data Query  = Check   { program :: LB.ByteString } 
            | Recheck { program :: LB.ByteString
                      , path    :: FilePath      }
            | Save    { program :: LB.ByteString 
                      , path    :: FilePath      } 
            | Load    { path    :: FilePath      } 
            | Perma   { program :: LB.ByteString }
            | Junk

type Result = Value

----------------------------------------------------------------
-- JSON Serialization ------------------------------------------
----------------------------------------------------------------

instance FromJSON Query where
  parseJSON (Object v) = objectQuery v
  parseJSON _          = mzero 

-- objectQuery    :: Object -> Query
objectQuery v 
  = do ty <- v .: "type" 
       case ty :: String of 
         "check"   -> Check   <$> v .: "program"  
         "recheck" -> Recheck <$> v .: "program" <*> v.: "path" 
         "perma"   -> Perma   <$> v .: "program" 
         "save"    -> Save    <$> v .: "program" <*> v.: "path" 
         "load"    -> Load    <$> v .: "path"
         _         -> mzero 

instance ToJSON Query where
  toJSON q@(Check prg)       = object ["type" .= jsonType q, "program" .= prg]
  toJSON q@(Recheck prg pth) = object ["type" .= jsonType q, "program" .= prg, "path" .= pth]
  toJSON q@(Perma prg)       = object ["type" .= jsonType q, "program" .= prg]
  toJSON q@(Save  prg pth)   = object ["type" .= jsonType q, "program" .= prg, "path" .= pth]
  toJSON q@(Load  pth)       = object ["type" .= jsonType q,                   "path" .= pth]
  toJSON q@(Junk)            = object ["type" .= jsonType q]

jsonType              :: Query -> String 
jsonType (Check {})   = "check"
jsonType (Recheck {}) = "recheck"
jsonType (Perma {})   = "perma"
jsonType (Save  {})   = "save"
jsonType (Load  {})   = "load"
jsonType (Junk  {})   = "junk"

----------------------------------------------------------------
----------------------------------------------------------------
----------------------------------------------------------------

dummyResult :: Result 
dummyResult = mkResult [("status", "crash")] -- fromJust $ decode "{\"status\" : \"crash\" }"

okResult :: Result
okResult    = mkResult [("status", "ok")] 

errResult :: LB.ByteString -> Result
errResult s = mkResult [("status", "crash"), ("error",  s)] 

mkResult :: [(LB.ByteString, LB.ByteString)] -> Result
mkResult = toJSON . M.fromList
