
module HostConfiguration where

import Control.Applicative ((<$>))
import Network.BSD
import System.Directory

type WorkspaceName = String
type NetInterfaceName = String
type ExecuteCommand = ( String, [ String ] )

defaultWorkspaceNames = ["web","com","dev","gfx","ofc","","","",""]
defaultNetInterfaceName = "re0"

data HostConfiguration = HostConfiguration {
        longitude :: Double                          ,
        latitude :: Double                           ,
        workspaceNames :: [ WorkspaceName ]          ,
        netInterfaceName :: NetInterfaceName         ,
        autostartPrograms :: [ ExecuteCommand ]
        }
        deriving ( Read, Show )

defaultHostConfiguration :: HostConfiguration
defaultHostConfiguration = HostConfiguration {
        longitude = -200.0                              ,
        latitude = -200.0                               ,
        workspaceNames = defaultWorkspaceNames          ,
        netInterfaceName = defaultNetInterfaceName      ,
        autostartPrograms = []
        }

readHostConfiguration :: FilePath -> IO HostConfiguration
readHostConfiguration homedir = do
        host <- myHostName
	let confpath = homedir ++ "/.xmonad/conf/" ++ host ++ ".hs"
	confexists <- doesFileExist confpath
        if confexists then do
                        contents <- readFile confpath
                        let parseresult = reads contents :: [ ( HostConfiguration, String ) ]
                        if null parseresult then return defaultHostConfiguration
                                else return $ fst $ head parseresult
                else return defaultHostConfiguration

myHostName :: IO String
myHostName = takeWhile (/= '.') <$> getHostName
