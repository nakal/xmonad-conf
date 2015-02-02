--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import Data.Monoid
import Data.List
import Control.Applicative ((<$>))
import System.IO
import System.Exit
import System.Posix.Process
import System.Directory
import Network.BSD
import Graphics.X11.Xlib.Display

import XMonad.Hooks.DynamicLog
import XMonad.Util.Run
import XMonad.Util.WindowProperties
import XMonad.Hooks.FadeInactive
import XMonad.Layout.PerWorkspace ( onWorkspace )
import XMonad.Layout.NoBorders ( smartBorders )
import XMonad.Layout.IM
import XMonad.Layout.ResizableTile
import XMonad.Config.Desktop ( desktopLayoutModifiers )
import XMonad.Layout.Reflect ( reflectHoriz )
import XMonad.Hooks.ManageDocks
import XMonad.Actions.CycleWS

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

type WorkspaceName = String
type NetInterfaceName = String
type ExecuteCommand = ( String, [ String ] )

defaultWorkspaceNames = ["web","com","dev","gfx","ofc","","irc","",""]
defaultNetInterfaceName = "re0"

data HostConfiguration = HostConfiguration {
        workspaceNames :: [ WorkspaceName ]          ,
        netInterfaceName :: NetInterfaceName         ,
        autostartPrograms :: [ ExecuteCommand ]
        }
        deriving ( Read, Show )

defaultHostConfiguration :: HostConfiguration
defaultHostConfiguration = HostConfiguration {
        workspaceNames = defaultWorkspaceNames          ,
        netInterfaceName = defaultNetInterfaceName      ,
        autostartPrograms = []
        }

-- xterm as default terminal in Xmonad
myTerminal      = "xterm"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
myModMask = mod4Mask

readHostConfiguration :: FilePath -> String -> IO HostConfiguration
readHostConfiguration homedir host = do
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

-- This function numbers the workspace names
numberedWorkspaces :: [ String ] -> [ String ]
numberedWorkspaces wsnames = zipWith (++) (map show [1..]) $ map appendName wsnames
	where appendName name = case null name of
		True -> ""
		_ -> (':' :) name

-- Safely returns a matching workspace name
getWorkspaceName :: [ String ] -> String -> String
getWorkspaceName wsnames name = case name `elemIndex` wsnames of
	Nothing	-> show $ length wsnames
	Just x	-> (show $ x+1) ++ ":" ++ name

-- readScreenWidth :: Display -> X Int
-- readScreenWidth dpy = fromIntegral $ displayWidth dpy $ defaultScreen dpy

readScreenWidthIO :: IO Int
readScreenWidthIO = do
	display <- openDisplay ""
	let width = fromIntegral $ displayWidth display (defaultScreen display)
	return width

dzenBitmap :: FilePath -> String -> String
dzenBitmap homedir pic = homedir ++ "/.xmonad/dzen2/" ++ pic ++ ".xbm"

panelLine :: FilePath -> String -> String
panelLine homedir netif =
        "^fg(lightblue)" ++
	( bitmap "cpu" ) ++ " ${cpu}% " ++
	( bitmap "mem" ) ++ " ${memperc}% " ++
	( bitmap "net_wired" ) ++ " " ++
	( bitmap "net_down_03" ) ++ "${downspeed " ++ netif ++ "} " ++
	( bitmap "net_up_03" ) ++ "${upspeed " ++ netif ++ "} " ++
	( bitmap "volume" ) ++ " ${exec mixer vol | egrep -o \"[0-9]+$\" | head -1 | egrep -o \"[0-9]*\"}% " ++
	"^fg(yellow) ${time %a %d.%m.%Y} ${time %R}"
	where bitmap name = "^i(" ++ (dzenBitmap homedir name) ++ ")"

writeConkyConf :: FilePath -> NetInterfaceName -> IO ()
writeConkyConf homedir netif =
	writeFile (homedir ++ "/.xmonad/conkyrc") (unlines $ conf homedir netif)
		where conf homedir netif = [
				"background yes",
				"out_to_console yes",
				"out_to_x no",
				"update_interval 1",
				"",
				"TEXT",
				panelLine homedir netif
			]

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ (XMonad.terminal conf) ++ " -e tmux new-session")
    , ((controlMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) -- plain terminal without tmux

    -- launch dmenu
    , ((modm,               xK_p     ), spawn $ "dmenu_run -nb '#202020' -nf lightcyan -sb yellow -sf black -fn '" ++ dzenFont ++ "'")

    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

    -- launch vim (in various ways, with most common uses)
    , ((modm,			xK_v     ), spawn $ (XMonad.terminal conf) ++ " -e vim -c \"Unite -start-insert file_rec\"")
    , ((modm .|. shiftMask,	xK_v     ), spawn $ (XMonad.terminal conf) ++ " -e vim ~/.vim/vimrc")
    , ((modm .|. controlMask,	xK_v     ), spawn $ "cd ~/src/nid-prototype ; " ++ (XMonad.terminal conf) ++ " -e vim -c \"Unite -start-insert file_rec\"")
    , ((modm .|. shiftMask,	xK_x     ), spawn $ (XMonad.terminal conf) ++ " -e vim ~/.xmonad/xmonad.hs")
    , ((modm,	xK_i     ), spawn $ (XMonad.terminal conf) ++ " -title weechat -e sh -c 'tmux has-session -t weechat && tmux attach-session -d -t weechat || tmux new-session -s weechat weechat'" )

    -- screensaver
    , ((mod1Mask .|. controlMask, xK_l     ), spawn "xscreensaver-command -lock")

    -- shutdown
    , ((modm .|. shiftMask, xK_BackSpace),
		(focusedHasProperty $ ClassName "VBoxSDL") >>= \p ->
			if (not p) then spawn "~/.xmonad/scripts/shutdown.sh"
				else return ()
	)

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    , ((modm,               xK_s     ), spawn $ (XMonad.terminal conf) ++ " -e ssh -t server1 'tmux new-session /bin/tcsh'" )
    , ((modm .|. shiftMask, xK_s     ), spawn $ (XMonad.terminal conf) ++ " -e ssh -t server2 'tmux new-session /bin/tcsh'" )
    , ((modm .|. shiftMask, xK_y     ), spawn $ (XMonad.terminal conf) ++ " -e ssh -t yuni 'tmux new-session /bin/tcsh'" )
    -- , ((modm .|. shiftMask, xK_g     ), spawn $ (XMonad.terminal conf) ++ " -e ssh -t gitlab 'tmux new-session'" )

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "killall conky dzen2; xmonad --recompile && xmonad --restart")

    , ((0              , xK_KP_Insert     ), toggleWS )
    , ((0              , xK_KP_Add     ), nextWS )
    , ((0              , xK_KP_Subtract     ), prevWS )

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    -- , ((modMask .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    -- same for numpad keys
    [((m, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_KP_End, xK_KP_Down, xK_KP_Page_Down,
                xK_KP_Left, xK_KP_Begin, xK_KP_Right,
                xK_KP_Home, xK_KP_Up, xK_KP_Page_Up]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]



------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
--myLayout = onWorkspace "6:gfx" gimpLayout $ onWorkspace "3:com" imLayout $ smartBorders (desktopLayoutModifiers (resizableTile ||| Mirror resizableTile ||| Full))
myLayout wsnames = onWorkspace (workspace "gfx") gimpLayout $ onWorkspace (workspace "com") imLayout $ smartBorders $ avoidStruts $ desktopLayoutModifiers (resizableTile ||| Mirror resizableTile ||| Full)
    where
    resizableTile = Tall nmaster delta ratio
    gimpLayout = avoidStruts $ withIM (0.12) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.15) (Role "gimp-dock") $ gridIM (0.15) (Role "gimp-dock") ||| resizableTile
    imLayout = avoidStruts $ withIM (0.12) (Role "buddy_list") $ reflectHoriz $ withIM (0.30) (Or (Role "conversation") (Title "weechat")) $ Full
    nmaster = 1
    ratio = toRational (2/(1+sqrt(5)::Double))
    delta = 3/100
    workspace wsname = getWorkspaceName wsnames wsname

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook wsnames = manageDocks <+> composeAll
    [ className =? "MPlayer"		--> doFloat
    , className =? "XMessage"		--> doFloat
    , className =? "Zenity"		--> doFloat
    , className =? "Iceweasel"		--> doShift  (getWorkspace "web")
    , className =? "Firefox"		--> doShift  (getWorkspace "web")
    , className =? "Claws-mail"		--> doShift  (getWorkspace "com")
    , className =? "Pidgin"		--> doShift  (getWorkspace "com")
    , className =? "VBoxSDL"		--> doShift  (getWorkspace "win")
    , className =? "Gimp"		--> doShift  (getWorkspace "gfx")
    , className =? "Inkscape"		--> doShift  (getWorkspace "gfx")
    , className =? "Darktable"		--> doShift  (getWorkspace "gfx")
    , className =? "Firefox"		--> doShift  (getWorkspace "web")
    , title =? "weechat"		--> doShift  (getWorkspace "irc")
    , isPrefixOf "libreoffice" <$> className	--> doShift (getWorkspace "ofc")
    , isPrefixOf "newwin - " <$> resource	--> doShift (getWorkspace "win")
    , resource  =? "desktop_window"	--> doIgnore
    , resource  =? "kdesktop"		--> doIgnore ]
	where getWorkspace name = getWorkspaceName wsnames name

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = docksEventHook

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook :: Handle -> FilePath -> X ()
myLogHook dzenbar homedir =
	dynamicLogWithPP $ defaultPP {
		ppCurrent           =   dzenColor "#ffffff" "#202020" . pad
		, ppVisible           =   dzenColor "lightblue" "#202020" . pad
		, ppHidden            =   dzenColor "lightblue" "#202020" . pad
		, ppHiddenNoWindows   =   dzenColor "#7b7b7b" "#202020" . pad
		, ppUrgent            =   dzenColor "#ff0000" "#202020" . pad
		, ppWsSep             =   " "
		, ppSep               =   "  |  "
		, ppLayout            =   dzenColor "lightblue" "#202020" .
		(\x -> case x of
		 "Tall"             ->      "^i(" ++ (dzenBitmap homedir "tall") ++ ")"
		 "ResizableTall"             ->      "^i(" ++ (dzenBitmap homedir "tall") ++ ")"
		 "Mirror Tall"      ->      "^i(" ++ (dzenBitmap homedir "mtall") ++ ")"
		 "Mirror ResizableTall"      ->      "^i(" ++ (dzenBitmap homedir "mtall") ++ ")"
		 "Full"                      ->      "^i(" ++ (dzenBitmap homedir "full") ++ ")"
		 "Simple Float"              ->      "~"
		 _                           ->      x
		)
		, ppTitle             =   (" " ++) . dzenColor "yellow" "#202020" . dzenEscape
		, ppOutput            =   hPutStrLn dzenbar
	}

{- Dzen status bars -}

dzenFont :: String
dzenFont = "Bitstream Vera Sans Mono:size=11:bold"

dzenExec :: String
dzenExec = "dzen2 -e '' -y 0 -h 24 -fg '#FFFFFF' -bg '#202020' -fn '" ++ dzenFont ++ "'"

-- left hand side, workspaces and layouts
myXmonadBar :: Int -> String
myXmonadBar screenwidth = dzenExec ++ " -x 0 -ta l -w " ++ (show $ screenwidth `div` 2)

-- right hand side, resources, date, time
myStatusBar :: String -> Int -> String
myStatusBar myHomeDirectory screenwidth = "conky -c " ++ myHomeDirectory ++ "/.xmonad/conkyrc | " ++
        dzenExec ++ " -ta r -x " ++ sw_div2 ++ " -w " ++ sw_div2
	where sw_div2 = show $ screenwidth `div` 2

xconfig conf dzenbar host homedir screenwidth = defaultConfig
		{
			terminal           = myTerminal,
			focusFollowsMouse  = myFocusFollowsMouse,
			clickJustFocuses   = myClickJustFocuses,
			borderWidth        = myBorderWidth,
			modMask            = myModMask,
			workspaces         = numberedWorkspaces wsnames,
			normalBorderColor  = myNormalBorderColor,
			focusedBorderColor = myFocusedBorderColor,

			keys               = myKeys,
			mouseBindings      = myMouseBindings,

			layoutHook         = myLayout wsnames,
			manageHook         = myManageHook wsnames,
			handleEventHook    = myEventHook,
			logHook            = myLogHook dzenbar homedir >> fadeInactiveLogHook 0xdddddddd,
			startupHook        = startup host homedir screenwidth conf
		}
                where wsnames = workspaceNames conf

-- startup hook reading and executing .startup file
startup :: String -> FilePath -> Int -> HostConfiguration -> X()
startup host homedir screenwidth conf = do
	io $ writeConkyConf homedir (netInterfaceName conf)
        autostartAllPrograms $ autostartPrograms conf
	spawn (myStatusBar homedir screenwidth)
        return ()

autostartAllPrograms :: [ ExecuteCommand ] -> X ()
autostartAllPrograms =
        mapM_ execprog
        where execprog prog = spawn $ (fst prog) ++ " " ++ (unwords $ snd prog)

main = do
	host <- myHostName
	homedir <- getHomeDirectory
	screenwidth <- readScreenWidthIO
	dzenbar <- spawnPipe $ myXmonadBar screenwidth
        conf <- readHostConfiguration homedir host
        hPutStrLn stderr $ show conf
	xmonad $ xconfig conf dzenbar host homedir screenwidth
