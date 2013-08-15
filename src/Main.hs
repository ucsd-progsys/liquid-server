{-# LANGUAGE OverloadedStrings #-}

import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server     hiding (Config)
import           Language.Liquid.Server.Types 
-- import           Data.Aeson           hiding (Result)
-- import           Data.Maybe
import           System.Exit            (ExitCode)
import           System.Directory       (doesFileExist)
import           System.FilePath        ((</>), addExtension)
import           System.Process         (system)
import           Control.Applicative    ((<$>))
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe
import           Data.List              (intercalate)
import           Data.Aeson                 hiding (Result)
-- import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as LB
-- import qualified Data.HashMap.Strict  as M
import           Data.Time.Clock.POSIX
main      :: IO ()
main      = quickHttpServe site

site      :: Snap ()
site      = route [ ("index.html" , serveFile      "resources/static/index.html") 
                  , ("log"        , serveFileAs    "text/plain" logFile) 
                  , ("js/"        , serveDirectory "resources/static/js")
                  , ("css/"       , serveDirectory "resources/static/css")
                  , ("demos/"     , serveDirectory "resources/static/demos")
                  , ("check/"     , method POST queryH)
                  , (""           , defaultH)
                  ]

defaultH       :: Snap ()
defaultH       = writeLBS "Liquid Demo Server: Oops, there's nothing here!"

queryH         :: Snap ()
queryH         = writeLBS . encode =<< bodyResult =<< readBody
  where
    readBody   = readRequestBody maxLen
    maxLen     = 1000000
    bodyResult = maybe (return dummyResult) (liftIO . queryResult) . decode 

---------------------------------------------------------------
queryResult :: Query -> IO Result
---------------------------------------------------------------
queryResult q 
  = do f <- queryFiles q
       writeQuery q f
       runCommand f
       readResult f

writeQuery     :: Query -> Files -> IO ()
writeQuery q f = LB.writeFile (srcFile f) (program q)

runCommand    :: Files -> IO ExitCode 
runCommand    = systemD . makeCommand . srcFile
  where 
    systemD c = {- putStrLn ("EXEC: " ++ c) >> -} system c  

readResult   :: Files -> IO Result
readResult f = do b <- doesFileExist file
                  if b then decodeResult <$> LB.readFile file
                       else return dummyResult
  where 
    file     = jsonFile f

------------------------------------------------------------------------
decodeResult :: LB.ByteString -> Result
------------------------------------------------------------------------
decodeResult = fromMaybe dummyResult . decode

------------------------------------------------------------------------
queryFiles   :: Query -> IO Files
------------------------------------------------------------------------
queryFiles _ 
  = do t        <- (takeWhile (/= '.') . show) <$> getPOSIXTime 
       return    $ F (sourceName t) (jsonName t)
    where 
      jsonName   = (`addExtension` "json") . sourceName 
      sourceName = (sandbox </>) . (`addExtension` ext) 
      ext        = srcSuffix   config
      sandbox    = sandboxPath config

------------------------------------------------------------------------
makeCommand :: FilePath -> String
------------------------------------------------------------------------
makeCommand target 
  = intercalate " " 
    [ -- "LANG=en_US.UTF-8", 
      cmdPrefix  config
    , srcChecker config
    , target 
    , ">"
    , logFile
    , "2>&1" 
    ]

-----------------------------------------------------------------
-- Configuration Parameters -------------------------------------
-----------------------------------------------------------------

config :: Config
config = C { srcSuffix   = "hs" 
           , srcChecker  = "liquid"
           , cmdPrefix   = "" -- "GHC_PACKAGE_PATH=/home/rjhala/research/liquid/.hsenv_liquid/ghc/lib/ghc-7.6.3/package.conf.d"
           , sandboxPath = "resources/sandbox/"
           }

logFile :: FilePath
logFile = (</> "log") $ sandboxPath config


