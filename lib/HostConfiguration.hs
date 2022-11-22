{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module HostConfiguration
        ( HostConfiguration
        , KeyMapping(..)
        , workspaces
        , workspaceMap
        , workspaceList
        , terminal
        , locale
        , readHostConfiguration
        , keyMappings
        , isSlim
        , sysInfoBar
        , autostartPrograms
        , commandForm
        ) where

import GHC.Generics
import Control.Applicative ((<$>))
import qualified Data.Map.Strict as M
import qualified Data.Maybe as MB
import Data.HashMap.Lazy
import qualified Data.Vector as V
import Graphics.X11.Types
import Network.HostName
import System.Directory
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Aeson
import Data.Aeson.Types

type NetInterfaceName = String
type ExecuteCommand = ( String, [ String ] )
type UsernameAtHostnameColonPort = String
type Hostname = String
type PortNum = String

type WorkspaceNames = M.Map Int String
type WorkspaceList = [ String ]

-- | Default desktop locale, if not configured
defaultLocale = "en"

-- | Default set of workspaces, if not configured
-- defaultWorkspaces = M.fromList $ zip [1..] ["web","com","dev","gfx","ofc","","","",""]
defaultWorkspaces = ["web","com","dev","gfx","ofc","","","",""]

-- | Default terminal to use, if not not configured
defaultTerminal = "xterm"

-- | The mode in which the sysinfobar should be displayed
data SysInfoBarMode = Slim | Full
        deriving ( Read, Show, Eq, Generic )

-- | General section as read from TOML
data GeneralSection = GeneralSection
        { gen_locale :: String
        , gen_terminal :: String
        , gen_barMode :: SysInfoBarMode
        } deriving (Show, Generic)

-- | Defaults for the general section
defaultGeneralSection :: GeneralSection
defaultGeneralSection = GeneralSection
        { gen_locale = defaultLocale
        , gen_terminal = defaultTerminal
        , gen_barMode = Full
        }

-- | Entire TOML configuration
data HostConfiguration = HostConfiguration
        { general :: GeneralSection
        , workspaces :: WorkspaceList
        , autostart :: [ [ String ] ]
        , keyMappings :: [KeyMapping]
        }
        deriving (Show, Generic)

-- | Defaults for the entire TOML configuration
defaultHostConfiguration :: HostConfiguration
defaultHostConfiguration = HostConfiguration
        { general = defaultGeneralSection
        , workspaces = defaultWorkspaces
        , autostart = []
        , keyMappings = []
        }

data KeyMapping = KeyMapping
        { kmKey :: String
        , kmName :: String
        , kmExecute :: [ String ]
        , kmInTerminal :: Bool
        } deriving ( Show, Generic)

instance FromJSON HostConfiguration where
        parseJSON (Object v) =
                HostConfiguration
                <$> v .:? "general" .!= general defaultHostConfiguration
                <*> v .:? "workspaces" .!= workspaces defaultHostConfiguration
                <*> v .:? "autostart" .!= autostart defaultHostConfiguration
                <*> v .:? "mapping" .!= keyMappings defaultHostConfiguration

instance FromJSON GeneralSection where
        parseJSON (Object v) =
                GeneralSection
                <$> v .:? "locale" .!= gen_locale defaultGeneralSection
                <*> v .:? "terminal" .!= gen_terminal defaultGeneralSection
                <*> v .:? "slimscreen" .!= gen_barMode defaultGeneralSection
        parseJSON invalid = prependFailure "parsing section general failed, " (typeMismatch "Object" invalid)

instance FromJSON SysInfoBarMode where
        parseJSON (Bool b) =
                return $ if b then Slim else Full
        parseJSON invalid = prependFailure "in section general: " (typeMismatch "slimscreen" invalid)

instance FromJSON KeyMapping where
        parseJSON (Object v) =
                KeyMapping
                <$> v .: "key"
                <*> v .: "name"
                <*> v .: "exec"
                <*> v .:? "in_terminal" .!= False
        parseJSON invalid = prependFailure "parsing key mapping failed, " (typeMismatch "mapping" invalid)

-- | Helper to check if we deal with a slim desktop variant
isSlim :: HostConfiguration -> Bool
isSlim hc = gen_barMode (general hc) == Slim

-- | Retrieves the current terminal application
terminal :: HostConfiguration -> String
terminal hc = gen_terminal (general hc)

-- | Retrieves the current locale
locale :: HostConfiguration -> String
locale hc = gen_locale (general hc)

-- | Retrieves the current workspace map as a Map
workspaceMap :: HostConfiguration -> M.Map String String
workspaceMap hc = M.foldrWithKey (\k v m -> M.insert v (wsname k v) m)  M.empty (M.fromList $ zip [1..] (workspaces hc))
        where wsname i n
                | isSlim hc = show i
                | otherwise = concat [ show i, ":", n ]

-- | Retrieves the current workspace list as [(num,name)]
workspaceList :: HostConfiguration -> [ (Int, String) ]
workspaceList hc = zip [1..9] (workspaces hc)

-- | Converts the array form from the configuration file to a pair
-- | separating command and parameters
commandForm :: [String] -> ExecuteCommand
commandForm (x:xs) = (x, xs)
commandForm _ = ("true",[])

autostartPrograms :: HostConfiguration -> [ExecuteCommand]
autostartPrograms = Prelude.map commandForm . autostart

-- | Locates, reads and parses the TOML configuration file
-- | and returns a HostConfiguration for general use
readHostConfiguration :: IO HostConfiguration
readHostConfiguration = do
        homedir <- getHomeDirectory
        host <- myHostName
        let confpath = homedir ++ "/.xmonad/conf/" ++ host ++ ".json"
        confexists <- doesFileExist confpath
        hPutStrLn stderr $ "Reading " ++ confpath
        if confexists then do
                        j <- eitherDecodeFileStrict confpath
                        case j of
                          Left err      ->      do
                                                        hPutStrLn stderr ("failed to parse JSON in " ++ confpath)
                                                        hPutStrLn stderr ("\t" ++ show err)
                                                        return defaultHostConfiguration
                          Right o       ->      return $ o
                else do
                        hPutStrLn stderr "configuration not found, using defaults."
                        return defaultHostConfiguration

-- | Helper to retrieve the short portion of the host name
-- | to use it a the filename
myHostName :: IO Hostname
myHostName = takeWhile (/= '.') <$> getHostName

-- | Returns the SysInfoBar execute path
sysInfoBar :: HostConfiguration -> String
sysInfoBar conf =
        "xmobar -d .xmonad/" ++ barPrefix ++ "sysinfo_xmobar.rc"
        where barPrefix
                | isSlim conf   = "slim_"
                | otherwise     = ""
