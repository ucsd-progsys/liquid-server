{-# LANGUAGE OverloadedStrings #-}
module Language.Liquid.Server.Snap (start) where


  
import           Snap.Core hiding (path)
import           Snap.Util.FileServe
import           Snap.Http.Server hiding (Config)

import           System.FilePath        ((</>)) -- , joinPath, addExtension, splitFileName)
import           Control.Applicative    ((<$>))
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe
import           Data.Aeson       hiding (Result)
  
import           Language.Liquid.Server.Types 
import           Language.Liquid.Server.Config
import           Language.Liquid.Server.Paths
import           Language.Liquid.Server.Query

start :: IO ()
start = do c <- getConfig
           case c of
             Right cfg -> quickHttpServe $ site cfg
             Left f    -> error $ "Malformed configuration file: " ++ f

site      :: Config -> Snap ()
site cfg  = route [ ("index.html"    , serveFile      $ staticPath </> "index.html"   ) 
                  , ("fullpage.html" , serveFile      $ staticPath </> "fullpage.html")
                  , ("config.js"     , serveFile   $ configPath cfg      )
                  , ("theme.js"      , serveFile   $ themePath cfg       )
                  , ("mode.js"       , serveFile   $ modePath  cfg       )
                  , ("js/"           , serveDirectory $ staticPath </> "js"           )
                  , ("css/"          , serveDirectory $ staticPath </> "css"          )
                  , ("img/"          , serveDirectory $ staticPath </> "img"          )
                  , ("demos/"        , serveDirectory $ demoPath cfg                  )
                  , ("permalink/"    , serveDirectory $ sandboxPath cfg               )
                  , ("query"         , method POST    $ queryH cfg                    )
                  , ("log"           , serveFileAs    "text/plain" $ logFile cfg      ) 
                  , (""              , defaultH                                       )
                  ]

defaultH :: Snap ()
defaultH = writeLBS "Liquid Demo Server: Oops, there's nothing here!"

queryH    :: Config -> Snap ()
queryH c  = writeLBS . encode =<< liftIO . queryResult c =<< getQuery  

getQuery :: Snap Query 
getQuery = fromMaybe Junk . decode <$> readRequestBody 1000000
