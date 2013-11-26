{-# LANGUAGE OverloadedStrings #-}

import           Snap.Core hiding (path)
import           Snap.Util.FileServe
import           Snap.Http.Server     hiding (Config)

import           System.IO.Error        (catchIOError)
import           System.Exit            (ExitCode)
import           System.Directory       (doesFileExist)
import           System.FilePath        ((</>), addExtension, splitFileName)
import           System.Process         (system)
import           Control.Applicative    ((<$>))
import           Control.Exception      (throw)
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe
import           Data.List              (isPrefixOf, intercalate)
import           Data.Aeson                 hiding (Result)
import qualified Data.ByteString.Lazy as LB
import           Data.Time.Clock.POSIX
import           Data.ByteString.Lazy.Char8 (pack)
import qualified Data.HashMap.Strict  as M
import           Data.ByteString (unpack)
import           Language.Liquid.Server.Types 
import           Language.Liquid.Server.Config


main      :: IO ()
main      = quickHttpServe $ site config 

site      :: Config -> Snap ()
site cfg  = route [ ("index.html"    , serveFile      $ staticPath </> "index.html"   ) 
                  , ("fullpage.html" , serveFile      $ staticPath </> "fullpage.html")
                  , ("js/"           , serveDirectory $ staticPath </> "js"           )
                  , ("css/"          , serveDirectory $ staticPath </> "css"          )
                  , ("img/"          , serveDirectory $ staticPath </> "img"          )
                  , ("demos/"        , serveDirectory $ demoPath cfg                  )
                  , ("permalink/"    , serveDirectory $ sandboxPath cfg               )
                  , ("custom/:js"    , customHandler cfg                              ) 
                  , ("query"         , method POST    $ queryH cfg                    )
                  , ("log"           , serveFileAs    "text/plain" $ logFile cfg      ) 
                  , (""              , defaultH                                       )
                  ]

defaultH :: Snap ()
defaultH = writeLBS "Liquid Demo Server: Oops, there's nothing here!"

queryH    :: Config -> Snap ()
queryH c  = writeLBS . encode =<< liftIO . queryResult c =<< getQuery  
  -- where
    -- queryResult' q = dumpQuery q >> queryResult q
    -- dumpQuery      = putStrLn . show . toJSON 

getQuery :: Snap Query 
getQuery = fromMaybe Junk . decode <$> readRequestBody 1000000

---------------------------------------------------------------
queryResult :: Config -> Query -> IO Result
---------------------------------------------------------------
queryResult c q@(Check {})   = checkResult   c q
queryResult c q@(Recheck {}) = recheckResult c q
queryResult _ q@(Load  {})   = loadResult      q
queryResult _ q@(Save  {})   = saveResult      q
queryResult c q@(Perma {})   = permaResult   c q
queryResult _ Junk           = return $ errResult "junk query" 

---------------------------------------------------------------
permaResult :: Config -> Query -> IO Result
---------------------------------------------------------------
permaResult config q
  = do f <- writeQuery q =<< genFiles config
       return $ toJSON $ Load $ permalink $ srcFile f

permalink :: FilePath -> FilePath
permalink = ("permalink" </>) . snd . splitFileName

-- m >>== f = m >>= (\x -> f x >> return x)

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
recheckResult :: Config -> Query -> IO Result
---------------------------------------------------------------
recheckResult c q = queryFiles c q >>= writeQuery q >>= execCheck c

queryFiles    :: Config -> Query -> IO Files
queryFiles config q
  = do b <- validRecheck config src
       if b then return $ Files src jsn 
            else throw  $ userError $ "Invalid Recheck Path: " ++ src   
    where 
      src        = path q
      jsn        = src `addExtension` "json"

validRecheck :: Config -> FilePath -> IO Bool 
validRecheck config src 
  = do b1    <- doesFileExist src
       let b2 = sandboxPath config `isPrefixOf` src
       return $ b1 && b2

---------------------------------------------------------------
checkResult :: Config -> Query -> IO Result
---------------------------------------------------------------
checkResult c q  = genFiles c >>= writeQuery q >>= execCheck c 

genFiles         :: Config -> IO Files
genFiles config 
  = do t        <- (takeWhile (/= '.') . show) <$> getPOSIXTime 
       return    $ Files (sourceName t) (jsonName t)
    where 
      jsonName   = (`addExtension` "json") . sourceName 
      sourceName = (sandbox </>) . (`addExtension` ext) 
      ext        = srcSuffix   config
      sandbox    = sandboxPath config

---------------------------------------------------------------
execCheck :: Config -> Files -> IO Result
---------------------------------------------------------------
execCheck c f
  = do runCommand c f
       r <- readResult f
       return $ r += ("path", toJSON $ srcFile f) 

---------------------------------------------------------------
writeQuery     :: Query -> Files -> IO Files
---------------------------------------------------------------
writeQuery q f = LB.writeFile (srcFile f) (program q) >> return f

---------------------------------------------------------------
runCommand     :: Config -> Files -> IO ExitCode 
---------------------------------------------------------------
runCommand cfg = systemD . makeCommand cfg . srcFile
  where 
    systemD c  = {- putStrLn ("EXEC: " ++ c) >> -} system c  

---------------------------------------------------------------
readResult   :: Files -> IO Result
---------------------------------------------------------------
readResult f = do b <- doesFileExist file
                  if b then decodeRes <$> LB.readFile file
                       else return dummyResult
  where 
    file      = jsonFile f
    decodeRes = fromMaybe dummyResult . decode

---------------------------------------------------------------
makeCommand :: Config -> FilePath -> String
---------------------------------------------------------------
makeCommand config t = intercalate " " 
    [ cmdPrefix  config
    , srcChecker config
    , t
    , ">"
    , logFile config
    , "2>&1" 
    ]

-----------------------------------------------------------------
-- Configuration Parameters -------------------------------------
-----------------------------------------------------------------


-- nanojs         = Config { }

logFile :: Config -> FilePath
logFile = (</> "log") . sandboxPath 

---------------------------------------------------------------
-- | Global Paths ---------------------------------------------
---------------------------------------------------------------

staticPath      = "resources/static"
editorPath      = staticPath </> "js/ace" 
customPath t n  = "resources/custom" </> n </> t  

---------------------------------------------------------------
-- | Extracting Paths from Config -----------------------------
---------------------------------------------------------------

demoPath, sandboxPath, configPath :: Config -> FilePath
demoPath        = customPath "demos"     . toolName
sandboxPath     = customPath "sandbox"   . toolName
configPath      = customPath "config.js" . toolName

customHandler :: Config -> Snap ()
customHandler cfg 
  = do js <- getParam "js"
       case fromMaybe "junk" js of
         "theme.js"  -> serveFile $ editorPath </> themeFile  cfg
         "mode.js"   -> serveFile $ editorPath </> modeFile   cfg
         "config.js" -> serveFile $ configPath cfg
         other       -> writeLBS  $ LB.concat ["Liquid Demo Server: Unexpected custom file ", b2lb other] 

b2lb = LB.pack . unpack 
---------------------------------------------------------------
-- | Generic helpers, could be in a misc ----------------------
---------------------------------------------------------------

---------------------------------------------------------------
-- readFile'    :: FilePath -> IO (Maybe LB.ByteString)
-- ---------------------------------------------------------------
-- readFile' fp = do b <- doesFileExist fp
--                   if b then Just <$> LB.readFile fp
--                        else return Nothing

---------------------------------------------------------------
-- (+=) :: Value -> (Text, Value) -> Value 
{-@ (+=) :: {v:Value | (Object v)} -> Pair -> Value @-} 
---------------------------------------------------------------
(Object o) += (k, v) = Object $ M.insert k v o
_          +=  _     = throw  $ userError "invalid addition to value"


