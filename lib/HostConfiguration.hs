
module HostConfiguration where

import Control.Applicative ((<$>))
import qualified Data.Map as M
import Graphics.X11.Types
import Network.BSD
import System.Directory
import System.IO

type WorkspaceName = String
type NetInterfaceName = String
type ExecuteCommand = ( String, [ String ] )
type UsernameAtHostnameColonPort = String
type Hostname = String
type PortNum = String

defaultLocale = "en"
defaultWorkspaceNames = ["web","com","dev","gfx","ofc","","","",""]
defaultNetInterfaceName = "re0"
defaultTerminal = "urxvt"

data HostConfiguration = HostConfiguration {
        locale :: String                        ,
        workspaceNames :: [ WorkspaceName ]     ,
        slimView :: Bool                        ,
        terminal :: FilePath                    ,
        netInterfaceName :: NetInterfaceName    ,
        autostartPrograms :: [ ExecuteCommand ] ,
        ssh :: [ ((KeyMask, KeySym),UsernameAtHostnameColonPort)]
        }
        deriving ( Read, Show )

defaultHostConfiguration :: HostConfiguration
defaultHostConfiguration = HostConfiguration {
        locale = defaultLocale                          ,
        workspaceNames = defaultWorkspaceNames          ,
        slimView = False                                ,
        terminal = defaultTerminal                      ,
        netInterfaceName = defaultNetInterfaceName      ,
        autostartPrograms = []                          ,
        ssh = []
        }

readHostConfiguration :: IO HostConfiguration
readHostConfiguration = do
        homedir <- getHomeDirectory
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

myHostName :: IO Hostname
myHostName = takeWhile (/= '.') <$> getHostName

sshConnections :: HostConfiguration -> [((KeyMask,KeySym),(Hostname,PortNum))]
sshConnections =
        map (\((k,s),uhcp) -> ((k,s), makePort $ break (== ':') uhcp)) . ssh
        where makePort (a,b) = (a, if (null b) then "22" else drop 1 b)
