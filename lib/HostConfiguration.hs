module HostConfiguration where

import Control.Applicative ((<$>))
import qualified Data.Map as M
import Data.HashMap.Lazy
import Graphics.X11.Types
import Network.HostName
import System.Directory
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Toml
import Text.Toml.Types

type WorkspaceName = String
type NetInterfaceName = String
type ExecuteCommand = ( String, [ String ] )
type UsernameAtHostnameColonPort = String
type Hostname = String
type PortNum = String
type SSHMapping = ((KeyMask, KeySym),UsernameAtHostnameColonPort)

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
        ssh :: [SSHMapping]
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

data GeneralSection = GeneralSection
        { gen_locale :: String
        , gen_terminal :: String
        , gen_barMode :: SysInfoBarMode
        }

parseGeneralSection :: Table -> GeneralSection
parseGeneralSection g =
        GeneralSection
                (case g ! T.pack "locale" of
                        VString l       -> T.unpack l
                        _               -> defaultLocale
                )
                (case g ! T.pack "terminal" of
                        VString t       -> T.unpack t
                        _               -> defaultTerminal
                )
                (case g ! T.pack "barmode" of
                        VString bm  -> if T.unpack bm == "Slim" then Slim
                                        else barMode defaultHostConfiguration
                        _      -> barMode defaultHostConfiguration
                )

parseWorkSpaceSection :: Table -> [WorkspaceName]
parseWorkSpaceSection g = defaultWorkspaceNames

parseAutostartSection :: Table -> [ExecuteCommand]
parseAutostartSection a = autostartPrograms defaultHostConfiguration

parseMappingsSection :: Table -> [SSHMapping]
parseMappingsSection s = ssh defaultHostConfiguration

parseConfiguration :: Table -> HostConfiguration
parseConfiguration t =
        let gen = parseGeneralSection $ case t ! T.pack "general" of
                        VTable general        -> general
                        _                     -> emptyTable
            wsp = parseWorkSpaceSection $ case t ! T.pack "workspaces" of
                    VTable ws   -> ws
                    _           -> emptyTable
            auto = parseAutostartSection $ case t ! T.pack "autostart" of
                    VTable a   -> a
                    _          -> emptyTable
            mappings = parseMappingsSection $ case t ! T.pack "mappings" of
                    VTable m   -> m
                    _          -> emptyTable
        in
                HostConfiguration
                        { locale = gen_locale gen
                        , workspaceNames = wsp
                        , barMode = gen_barMode gen
                        , terminal = gen_terminal gen
                        , autostartPrograms = auto
                        , ssh = mappings
                        }

readHostConfiguration :: IO HostConfiguration
readHostConfiguration = do
        homedir <- getHomeDirectory
        host <- myHostName
        let confpath = homedir ++ "/.xmonad/conf/" ++ host ++ ".toml"
        confexists <- doesFileExist confpath
        hPutStrLn stderr $ "Reading " ++ confpath
        if confexists then do
                        contents <- TIO.readFile confpath
                        let toml = parseTomlDoc "" contents
                        case toml of
                          Left err      ->      do
                                                        hPutStrLn stderr ("failed to parse TOML in " ++ confpath)
                                                        hPutStrLn stderr ("\t" ++ show err)
                                                        return defaultHostConfiguration
                          Right t       ->      do
                                                        print toml
                                                        return $ parseConfiguration t
                else do
                        hPutStrLn stderr "configuration not found, using defaults."
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
        fmap (\((k,s),uhcp) -> ((k,s), makePort $ break (== ':') uhcp)) . ssh
        where makePort (a,b) = (a, if Prelude.null b then "22" else drop 1 b)
