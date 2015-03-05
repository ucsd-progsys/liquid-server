{-# LANGUAGE OverloadedStrings #-}

module Language.Liquid.Server.Query (queryResult) where


import           System.IO.Error        (catchIOError)
import           System.Exit            (ExitCode)
import           System.Directory       (doesFileExist)
import           System.FilePath        ((</>), addExtension, splitFileName)
import           System.Process         (system)
import           Control.Applicative    ((<$>))
import           Control.Exception      (throw)
import           Data.Maybe
import           Data.List              (isPrefixOf, intercalate)
import           Data.Aeson                 hiding (Result)
import qualified Data.Text.Lazy       as T
import qualified Data.Text.Lazy.IO    as TIO 
import qualified Data.ByteString.Lazy as LB
import           Data.Time.Clock.POSIX
import qualified Data.HashMap.Strict  as M

import           Language.Liquid.Server.Types 
import           Language.Liquid.Server.Paths

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
    doRead   = TIO.readFile (path q) >>= return . ok
    err e    = return $ errResult $ T.pack $ "Load Error: " ++ show e
    ok pgm   = toJSON $ Save pgm (path q) 

---------------------------------------------------------------
saveResult :: Query -> IO Result
---------------------------------------------------------------
saveResult q = doWrite `catchIOError` err
  where 
    doWrite  = TIO.writeFile (path q) (program q) >> return ok
    err e    = return $ errResult $ T.pack $ "Save Error: " ++ show e
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
       return    $ Files (srcF t) (jsonF t)
    where 
      jsonF t    = sandbox </> tmpDir config </> jsonName t
      jsonName t = srcName t `addExtension` "json"
      srcF  t    = sandbox </> srcName t 
      srcName  t = t `addExtension` ext
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
writeQuery q f = TIO.writeFile (srcFile f) (program q) >> return f

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

---------------------------------------------------------------
-- | Redirecting Custom Files ---------------------------------
---------------------------------------------------------------

-- b2lb = LB.pack . unpack 

---------------------------------------------------------------
-- (+=) :: Value -> (T.Text, Value) -> Value 
{-@ (+=) :: {v:Value | (Object v)} -> Pair -> Value @-} 
---------------------------------------------------------------
(Object o) += (k, v) = Object $ M.insert k v o
_          +=  _     = throw  $ userError "invalid addition to value"


