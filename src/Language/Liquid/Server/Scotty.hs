{-# LANGUAGE OverloadedStrings #-}

module Language.Liquid.Server.Scotty (start) where

import Web.Scotty
import System.FilePath        ((</>)) -- , joinPath, addExtension, splitFileName)
-- import Control.Applicative    ((<$>))
import Control.Monad.IO.Class (liftIO)
import Data.Maybe
import Data.Aeson       hiding (Result, json)
-- import Network.Wai

-- import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.Cors

import Language.Liquid.Server.Types
import Language.Liquid.Server.Config
import Language.Liquid.Server.Paths
import Language.Liquid.Server.Query
import Language.Liquid.Server.Ticket


start :: IO ()
start = do
  c <- getConfig
  t <- newTicket
  case c of
    Left f    -> error $ "Malformed configuration file: " ++ f
    Right cfg -> serve (port cfg) (site cfg t)

serve :: Int -> ScottyM () -> IO ()
serve p act = scotty p $ do
  -- middleware logStdoutDev
  middleware $ staticPolicy (noDots >-> addBase staticPath)
  -- middleware $ simpleCors
  middleware $ cors (const $ Just myCorsPolicy)
  act

myCorsPolicy :: CorsResourcePolicy
myCorsPolicy = CorsResourcePolicy
    { corsOrigins        = Nothing
    , corsMethods        = simpleMethods
    , corsRequestHeaders = ["x-requested-with", "content-type"]
    , corsExposedHeaders = Nothing
    , corsMaxAge         = Nothing
    , corsVaryOrigin     = True
    , corsRequireOrigin  = False
    , corsIgnoreFailures = False
    }

site   :: Config -> Ticket -> ScottyM ()
site cfg t = route
   [ (get "/index.html"      , serveFile $ staticPath </> "index.html"   )
   , (get "/fullpage.html"   , serveFile $ staticPath </> "fullpage.html")
   , (get "/config.js"       , serveFile $ configPath cfg                )
   , (get "/theme.js"        , serveFile $ themePath cfg                 )
   , (get "/mode.js"         , serveFile $ modePath  cfg                 )
   , (post "/query"          , queryH cfg t                               )
   , (get "/log"             , serveFileAsText $ logFile cfg             )
   , (get "/demos/:path"     , serveFileAt $ demoPath cfg                )
   , (get "/permalink/:path" , serveFileAt $ sandboxPath cfg             )
   -- handled by static
   --    , (get "/js/:path"        , serveDirectory $ staticPath </> "js" )
   --    , (get "/css/:path"       , serveDirectory $ staticPath </> "css")
   --    , (get "/img/:path"       , serveDirectory $ staticPath </> "img")
   , (get "/"                , defaultH                                  )
   ]
  where
    route pas = sequence_  [p a | (p, a) <- pas]

serveFile  :: FilePath -> ActionM ()
serveFile f = do liftIO (putStrLn $ "serveFile: " ++ f)
                 file f

serveFileAt :: FilePath -> ActionM ()
serveFileAt p = do f <- param "path"
                   serveFile $ p </> f

-- serveDirectory   :: FilePath -> ActionM ()
-- serveDirectory p = do f <- param "path"
--                       serveFile $ p </> f

serveFileAsText   :: FilePath -> ActionM ()
serveFileAsText f = do setHeader "Content-Type" "text/plain"
                       serveFile f

defaultH :: ActionM ()
defaultH = text "Liquid Demo Server: Oops, there's nothing here! Perhaps try /index.html"

queryH    :: Config -> Ticket -> ActionM ()
queryH c t = {- writeLBS . encode -} json =<< liftIO . queryResult c t =<< getQuery

getQuery :: ActionM Query
getQuery = fromMaybe Junk . decode <$> body {- readRequestBody 1000000 -}
