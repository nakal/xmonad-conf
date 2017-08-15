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
defaultTerminal = "xterm"

-- | The mode in which the sysinfobar should be displayed
data SysInfoBarMode = Slim | Full
        deriving ( Read, Show, Eq )

data HostConfiguration = HostConfiguration {
        locale :: String                        ,
        workspaceNames :: [ WorkspaceName ]     ,
        barMode :: SysInfoBarMode               ,
        terminal :: FilePath                    ,
        autostartPrograms :: [ ExecuteCommand ] ,
        ssh :: [ ((KeyMask, KeySym),UsernameAtHostnameColonPort)]
        }
        deriving ( Read, Show )

defaultHostConfiguration :: HostConfiguration
defaultHostConfiguration = HostConfiguration {
        locale = defaultLocale                          ,
        workspaceNames = defaultWorkspaceNames          ,
        barMode = Full                                  ,
        terminal = defaultTerminal                      ,
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

-- SysInfoBar path
mySysInfoBar :: SysInfoBarMode -> String
mySysInfoBar mode =
        "xmobar -d .xmonad/" ++ barPrefix ++ "sysinfo_xmobar.rc"
        where barPrefix
                | mode == Slim = "slim_"
                | mode == Full = ""

sshConnections :: HostConfiguration -> [((KeyMask,KeySym),(Hostname,PortNum))]
sshConnections =
        map (\((k,s),uhcp) -> ((k,s), makePort $ break (== ':') uhcp)) . ssh
        where makePort (a,b) = (a, if (null b) then "22" else drop 1 b)
