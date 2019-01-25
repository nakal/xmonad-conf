module HostConfiguration
        ( WorkspaceNames
        , HostConfiguration
        , KeyMapping(..)
        , workspaces
        , workspaceMap
        , terminal
        , locale
        , readHostConfiguration
        , keyMappings
        , isSlim
        , sysInfoBar
        , autostartPrograms
        ) where

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
import Text.Toml
import Text.Toml.Types

type NetInterfaceName = String
type ExecuteCommand = ( String, [ String ] )
type UsernameAtHostnameColonPort = String
type Hostname = String
type PortNum = String
data KeyMapping = KeyMapping
        { kmKey :: String
        , kmName :: String
        , kmExec :: ExecuteCommand
        , kmInTerminal :: Bool
        } deriving Show
type WorkspaceNames = M.Map Int String

defaultLocale = "en"
defaultWorkspaces = M.fromList $ zip [1..] ["web","com","dev","gfx","ofc","","","",""]
defaultTerminal = "xterm"

-- | The mode in which the sysinfobar should be displayed
data SysInfoBarMode = Slim | Full
        deriving ( Read, Show, Eq )

data GeneralSection = GeneralSection
        { gen_locale :: String
        , gen_terminal :: String
        , gen_barMode :: SysInfoBarMode
        } deriving Show

defaultGeneralSection :: GeneralSection
defaultGeneralSection = GeneralSection
        { gen_locale = "en"
        , gen_terminal = defaultTerminal
        , gen_barMode = Full
        }

data HostConfiguration = HostConfiguration
        { general :: GeneralSection
        , workspaces :: WorkspaceNames
        , autostartPrograms :: [ ExecuteCommand ]
        , keyMappings :: [KeyMapping]
        }
        deriving Show

isSlim :: HostConfiguration -> Bool
isSlim hc = gen_barMode (general hc) == Slim

terminal :: HostConfiguration -> String
terminal hc = gen_terminal (general hc)

locale :: HostConfiguration -> String
locale hc = gen_locale (general hc)

workspaceMap :: HostConfiguration -> M.Map String String
workspaceMap hc = M.foldrWithKey (\k v m -> M.insert v (wsname k v) m)  M.empty (workspaces hc)
        where wsname i n
                | isSlim hc = show i
                | otherwise = concat [ show i, ":", n ]

defaultHostConfiguration :: HostConfiguration
defaultHostConfiguration = HostConfiguration
        { general = defaultGeneralSection
        , workspaces = defaultWorkspaces
        , autostartPrograms = []
        , keyMappings = []
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
                (case g ! T.pack "slimscreen" of
                        VBoolean bm  -> if bm then Slim
                                        else gen_barMode defaultGeneralSection
                        _      -> gen_barMode defaultGeneralSection
                )

parseWorkSpaceSection :: Table -> WorkspaceNames
parseWorkSpaceSection w =
        M.fromList
                [
                        (num, T.unpack n) | num <- [1..9],
                        let tnum = T.pack (show num),
                        member tnum w,
                        let VString n = w ! T.pack (show num)
                ]

parseExec :: Node -> Maybe ExecuteCommand
parseExec e =
        case e of
          VArray v      -> let cmd = traverse
                                (\x -> case x of
                                        VString t     -> Just $ T.unpack t
                                        _             -> Nothing
                                ) (V.toList v)
                           in
                                case cmd of
                                  Just (bin:args)       -> Just (bin, args)
                                  _                     -> Nothing
          _             -> Nothing

parseAutostartSection :: Table -> [ExecuteCommand]
parseAutostartSection a =
        case a ! T.pack "exec" of
          VArray v      -> MB.mapMaybe parseExec (V.toList v)
          _             -> autostartPrograms defaultHostConfiguration

parseMapping :: Table -> Maybe KeyMapping
parseMapping mt =
        let k = case mt ! T.pack "key" of
                  VString t     ->      T.unpack t
                  _             ->      ""
            e = case mt ! T.pack "exec" of
                  v@(VArray _)  ->      parseExec v
                  _             ->      Nothing
            n = case mt ! T.pack "name" of
                  VString n     ->      T.unpack n
                  _             ->      ""
            t = case mt ! T.pack "in_terminal" of
                  VBoolean b    ->      b
                  _             ->      False
        in
                if Prelude.null k || MB.isNothing e
                   then
                        Nothing
                   else
                        Just $ KeyMapping k n (MB.fromJust e) t

parseMappingTable :: VTArray -> [KeyMapping]
parseMappingTable a = MB.catMaybes $ V.toList $ V.map parseMapping a

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
            mapping = case t ! T.pack "mapping" of
                    VTArray m           -> parseMappingTable m
                    _                   -> []
        in
                HostConfiguration
                        { general = gen
                        , workspaces = wsp
                        , autostartPrograms = auto
                        , keyMappings = mapping
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
                          Right t       ->      return $ parseConfiguration t
                else do
                        hPutStrLn stderr "configuration not found, using defaults."
                        return defaultHostConfiguration

myHostName :: IO Hostname
myHostName = takeWhile (/= '.') <$> getHostName

-- SysInfoBar path
sysInfoBar :: HostConfiguration -> String
sysInfoBar conf =
        "xmobar -d .xmonad/" ++ barPrefix ++ "sysinfo_xmobar.rc"
        where barPrefix
                | isSlim conf   = "slim_"
                | otherwise     = ""
