{-# LANGUAGE OverloadedStrings #-}

import           Snap.Core hiding (path)
import           Snap.Util.FileServe
import           Snap.Http.Server     hiding (Config)

import           System.IO.Error        (catchIOError)
import           System.Exit            (ExitCode)
import           System.Directory       (doesFileExist)
import           System.FilePath        ((</>), joinPath, addExtension, splitFileName)
import           System.Process         (system)
import           System.Environment     (getArgs)
import           Control.Applicative    ((<$>))
import           Control.Exception      (throw)
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe
import           Data.List              (isSuffixOf, isPrefixOf, intercalate)
import           Data.Aeson                 hiding (Result)
import qualified Data.Text.Lazy       as T
import qualified Data.Text.Lazy.IO    as TIO 
import qualified Data.ByteString.Lazy as LB
import           Data.Time.Clock.POSIX
import           Data.ByteString.Lazy.Char8 (pack)
import qualified Data.HashMap.Strict  as M
import           Data.ByteString (unpack)

  
import           Language.Liquid.Server.Types 
import           Language.Liquid.Server.Config
import           Language.Liquid.Server.Paths
import           Language.Liquid.Server.Query



main      :: IO ()
main      = do c <- getConfig
               case c of
                 Right cfg -> quickHttpServe $ site cfg
                 Left f    -> error $ "Malformed configuration file: " ++ f

site      :: Config -> Snap ()
site cfg  = route [ ("index.html"    , serveFile      $ staticPath </> "index.html"   ) 
                  , ("fullpage.html" , serveFile      $ staticPath </> "fullpage.html")
                  , ("config.js"     , logServeFile "config.js" $ configPath cfg      )
                  , ("theme.js"      , logServeFile "theme.js"  $ themePath cfg       )
                  , ("mode.js"       , logServeFile "mode.js"   $ modePath  cfg       )
                  , ("js/"           , serveDirectory $ staticPath </> "js"           )
                  , ("css/"          , serveDirectory $ staticPath </> "css"          )
                  , ("img/"          , serveDirectory $ staticPath </> "img"          )
                  , ("demos/"        , serveDirectory $ demoPath cfg                  )
                  , ("permalink/"    , serveDirectory $ sandboxPath cfg               )
                  , ("query"         , method POST    $ queryH cfg                    )
                  , ("log"           , serveFileAs    "text/plain" $ logFile cfg      ) 
                  , (""              , defaultH                                       )
                  ]

logServeFile f p = serveFile p

defaultH :: Snap ()
defaultH = writeLBS "Liquid Demo Server: Oops, there's nothing here!"

queryH    :: Config -> Snap ()
queryH c  = writeLBS . encode =<< liftIO . queryResult c =<< getQuery  

getQuery :: Snap Query 
getQuery = fromMaybe Junk . decode <$> readRequestBody 1000000
