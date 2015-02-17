
module Language.Liquid.Server.Paths  where

import           System.FilePath        ((</>), joinPath)
import           Language.Liquid.Server.Types 

---------------------------------------------------------------
-- | Global Paths ---------------------------------------------
---------------------------------------------------------------
logFile :: Config -> FilePath
logFile = (</> "log") . sandboxPath 


themePath      :: Config -> FilePath
themePath      = (editorPath </>) . themeFile

modePath :: Config -> FilePath
modePath       = (editorPath </>) . modeFile 

editorPath     :: FilePath
editorPath     = staticPath </> "js/ace" 

demoPath       :: Config -> FilePath
demoPath c     = customPath [toolName c, "demos"]

sandboxPath    :: Config -> FilePath
sandboxPath c  = customPath [toolName c, "sandbox"]

configPath     :: Config -> FilePath
configPath c   = customPath [toolName c, "config.js"] 

staticPath     :: FilePath 
staticPath     = "resources/static"

customPath     :: [FilePath] -> FilePath
customPath ts  = joinPath ("resources" : "custom" : ts)

