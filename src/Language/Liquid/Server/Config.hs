{-# LANGUAGE OverloadedStrings #-}

module Language.Liquid.Server.Config (
  getConfig 
  ) where

import           System.Environment     (getArgs)
import           Control.Applicative    ((<$>))
import           Data.List              (isSuffixOf)
import           Data.Aeson                 hiding (Result)
import qualified Data.ByteString.Lazy as LB
import           Language.Liquid.Server.Types 
import           Language.Liquid.Server.Paths

---------------------------------------------------------------
getConfig :: IO (Either FilePath Config) 
---------------------------------------------------------------
getConfig 
  = do f <- getConfigFile 
       c <- decode <$> LB.readFile f
       putStrLn $ "Config: " ++ show c
       case c of
         Just cfg -> return $ Right cfg
         Nothing  -> return $ Left f

getConfigFile :: IO FilePath
getConfigFile 
  = do args <- getArgs
       case [p | p <- args, ".json" `isSuffixOf` p] of
         f:_ -> do putStrLn $ "Using config file: " ++ f 
                   return f 
         []  -> do putStrLn $ "No config file specified, using: " ++ defaultConfigFile
                   return defaultConfigFile

defaultConfigFile :: FilePath
defaultConfigFile = customPath ["liquidhaskell", "config.json"]





