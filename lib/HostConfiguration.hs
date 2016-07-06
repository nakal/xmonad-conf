
module HostConfiguration where

import Control.Applicative ((<$>))
import Network.BSD
import System.Directory
import System.IO

type WorkspaceName = String
type NetInterfaceName = String
type ExecuteCommand = ( String, [ String ] )

defaultLocale = "en"
defaultWorkspaceNames = ["web","com","dev","gfx","ofc","","","",""]
defaultNetInterfaceName = "re0"
defaultTerminal = "xterm"

data HostConfiguration = HostConfiguration {
        locale :: String                        ,
        workspaceNames :: [ WorkspaceName ]     ,
        terminal :: FilePath                    ,
        netInterfaceName :: NetInterfaceName    ,
        autostartPrograms :: [ ExecuteCommand ]
        }
        deriving ( Read, Show )

defaultHostConfiguration :: HostConfiguration
defaultHostConfiguration = HostConfiguration {
        locale = defaultLocale                          ,
        workspaceNames = defaultWorkspaceNames          ,
        terminal = defaultTerminal                      ,
        netInterfaceName = defaultNetInterfaceName      ,
        autostartPrograms = []
        }

readHostConfiguration :: FilePath -> IO HostConfiguration
readHostConfiguration homedir = do
        host <- myHostName
        let confpath = homedir ++ "/.xmonad/conf/" ++ host ++ ".hs"
        confexists <- doesFileExist confpath
        hPutStr stderr $ "Reading " ++ confpath ++ " "
        if confexists then do
                        contents <- readFile confpath
                        let parseresult = reads contents :: [ ( HostConfiguration, String ) ]
                        if null parseresult then do
                                        hPutStrLn stderr "failed to parse, using defaults."
                                        return defaultHostConfiguration
                                else do
                                        hPutStrLn stderr "ok."
                                        return $ fst $ head parseresult
                else do
                        hPutStrLn stderr "failed, using defaults."
                        return defaultHostConfiguration

myHostName :: IO String
myHostName = takeWhile (/= '.') <$> getHostName
