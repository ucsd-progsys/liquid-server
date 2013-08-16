{-# LANGUAGE OverloadedStrings #-}

import           Snap.Core hiding (path)
import           Snap.Util.FileServe
import           Snap.Http.Server     hiding (Config)
import           Language.Liquid.Server.Types 
import           System.IO.Error        (catchIOError)
import           System.Exit            (ExitCode)
import           System.Directory       (doesFileExist)
import           System.FilePath        ((</>), addExtension)
import           System.Process         (system)
import           Control.Applicative    ((<$>))
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe
import           Data.List              (intercalate)
import           Data.Aeson                 hiding (Result)
import qualified Data.ByteString.Lazy as LB
import           Data.Time.Clock.POSIX
import           Data.ByteString.Lazy.Char8 (pack, unpack)

-- import           Data.Aeson           hiding (Result)
-- import           Data.Maybe
-- import qualified Data.HashMap.Strict  as M
-- import qualified Data.ByteString      as B

main      :: IO ()
main      = quickHttpServe site

site      :: Snap ()
site      = route [ ("index.html" , serveFile      "resources/static/index.html") 
                  , ("log"        , serveFileAs    "text/plain" logFile) 
                  , ("js/"        , serveDirectory "resources/static/js")
                  , ("css/"       , serveDirectory "resources/static/css")
                  , ("img/"       , serveDirectory "resources/static/img")
                  , ("demos/"     , serveDirectory "resources/static/demos")
                  , ("act/"       , method POST    queryH)
                  , (""           , defaultH)
                  ]

defaultH :: Snap ()
defaultH = writeLBS "Liquid Demo Server: Oops, there's nothing here!"

queryH   :: Snap ()
queryH   = writeLBS . encode =<< liftIO . queryResult =<< getQuery  

getQuery :: Snap Query 
getQuery = fromMaybe Junk . decode <$> readRequestBody 1000000

---------------------------------------------------------------
loadResult :: Query -> IO Result
---------------------------------------------------------------
loadResult q = doRead `catchIOError` err
  where 
    doRead   = LB.readFile (path q) >>= return . ok
    err e    = return $ errResult $ pack $ "Load Error: " ++ show e
    ok pgm   = toJSON $ Save pgm (path q) 

---------------------------------------------------------------
saveResult :: Query -> IO Result
---------------------------------------------------------------
saveResult q = doWrite `catchIOError` err
  where 
    doWrite  = LB.writeFile (path q) (program q) >> return ok
    err e    = return $ errResult $ pack $ "Save Error: " ++ show e
    ok       = toJSON $ Load (path q) 

---------------------------------------------------------------
queryResult :: Query -> IO Result
---------------------------------------------------------------
queryResult q@(Check {}) = checkResult q
queryResult q@(Load {})  = loadResult  q
queryResult q@(Save {})  = saveResult  q
queryResult q@(Junk )    = return $ errResult "junk query" 

---------------------------------------------------------------
checkResult :: Query -> IO Result
---------------------------------------------------------------
checkResult q 
  = do f <- checkFiles
       writeQuery q f
       runCommand f
       readResult f

checkFiles   :: IO Files
checkFiles 
  = do t        <- (takeWhile (/= '.') . show) <$> getPOSIXTime 
       return    $ Files (sourceName t) (jsonName t)
    where 
      jsonName   = (`addExtension` "json") . sourceName 
      sourceName = (sandbox </>) . (`addExtension` ext) 
      ext        = srcSuffix   config
      sandbox    = sandboxPath config

writeQuery     :: Query -> Files -> IO ()
writeQuery q f = LB.writeFile (srcFile f) (program q)

runCommand    :: Files -> IO ExitCode 
runCommand    = systemD . makeCommand . srcFile
  where 
    systemD c = {- putStrLn ("EXEC: " ++ c) >> -} system c  

readResult   :: Files -> IO Result
readResult f = do b <- doesFileExist file
                  if b then decodeRes <$> LB.readFile file
                       else return dummyResult
  where 
    file      = jsonFile f
    decodeRes = fromMaybe dummyResult . decode

makeCommand :: FilePath -> String
makeCommand t = intercalate " " 
    [ cmdPrefix  config
    , srcChecker config
    , t
    , ">"
    , logFile
    , "2>&1" 
    ]




-----------------------------------------------------------------
-- Configuration Parameters -------------------------------------
-----------------------------------------------------------------

config :: Config
config = Config { srcSuffix   = "hs" 
                , srcChecker  = "liquid"
                , cmdPrefix   = "" -- "GHC_PACKAGE_PATH=/home/rjhala/research/liquid/.hsenv_liquid/ghc/lib/ghc-7.6.3/package.conf.d"
                , sandboxPath = "resources/sandbox/"
                }

logFile :: FilePath
logFile = (</> "log") $ sandboxPath config

---------------------------------------------------------------
-- | Generic helpers, could be in a misc ----------------------
---------------------------------------------------------------

---------------------------------------------------------------
readFile'    :: FilePath -> IO (Maybe LB.ByteString)
---------------------------------------------------------------
readFile' fp = do b <- doesFileExist fp
                  if b then Just <$> LB.readFile fp
                       else return Nothing


