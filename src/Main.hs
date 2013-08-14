{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.IO.Class (liftIO)
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Language.Liquid.Server.Types 
import           Data.Aeson           hiding (Result)
import           Data.Maybe

main      :: IO ()
main      = quickHttpServe site

site      :: Snap ()
site      = route [ ("index.html" , serveFile      "resources/static/index.html") 
                  , ("log"        , serveFileAs    "text/plain" "resources/sandbox/log") 
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


-- bodyResult bs = case decode bs of
--                   Nothing -> return dummyResult
--                   Just q  -> queryResult q 
-- 
-- getResult = readBody >>= bodyResult 

-- echoHandler = do
--     param <- getParam "echoparam"
--     maybe (writeBS "must specify echo/param in URL")
--           writeBS param
