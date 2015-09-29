--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

{-# LANGUAGE FlexibleContexts #-}

import XMonad
import Data.Monoid
import Data.List
import Control.Applicative ((<$>))
import System.IO
import System.Exit
import System.Directory
import System.Posix.Process
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
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.FadeInactive

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import Command.CommandPipe
import Dzen.Tools
import HostConfiguration
import SysInfo.StatusBar
import SysInfo.StatusBarType

-- default terminal in Xmonad
myTerminal      = "urxvt"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels
myBorderWidth   = 0

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
myModMask = mod4Mask

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


-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ (XMonad.terminal conf) ++ " -e tmux -2 new-session")
    , ((controlMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) -- plain terminal without tmux

    -- launch dmenu
    , ((modm,               xK_p     ), spawn $ "dmenu_run -nb '#202020' -nf lightcyan -sb yellow -sf black -fn '" ++ dzenFont ++ "'")

    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

    -- launch vim (in various ways, with most common uses)
    , ((modm,			xK_v     ), spawn $ (XMonad.terminal conf) ++ " -e tmux -2 new-session 'vim -c CtrlP'")
    , ((modm .|. shiftMask,	xK_v     ), spawn $ (XMonad.terminal conf) ++ " -e vim ~/.vim/vimrc")
    , ((modm .|. controlMask,	xK_v     ), spawn $ "cd ~/src/nid-prototype ; " ++ (XMonad.terminal conf) ++ " -e tmux -2 new-session 'vim -c CtrlP'")
    , ((modm .|. shiftMask,	xK_x     ), spawn $ (XMonad.terminal conf) ++ " -e vim ~/.xmonad/xmonad.hs")
    , ((modm,	xK_i     ), spawn $ (XMonad.terminal conf) ++ " -title weechat -e sh -c 'tmux has-session -t weechat && tmux -2 attach-session -d -t weechat || tmux -2 new-session -s weechat weechat'" )

    -- screensaver
    , ((mod1Mask .|. controlMask, xK_l     ), spawn "xscreensaver-command -lock")

    , ((modm, xK_BackSpace     ), sendCommandToPipe "shade_toggle")

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

    , ((modm,               xK_s     ), spawn $ (XMonad.terminal conf) ++ " -e ssh -Y -t server1 'tmux -2 new-session'" )
    , ((modm .|. shiftMask, xK_s     ), spawn $ (XMonad.terminal conf) ++ " -e ssh -Y -t server2 'tmux -2 new-session'" )
    , ((modm .|. shiftMask, xK_y     ), spawn $ (XMonad.terminal conf) ++ " -e ssh -Y -t yuni 'tmux -2 new-session'" )

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
    , ((modm              , xK_q     ), spawn "killall dzen2; cd ~/.xmonad/lib/Exec && make ; xmonad --recompile && xmonad --restart")

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



sendCommandToPipe :: String -> X()
sendCommandToPipe cmd = do
        sb <- statusBarGet
        case gettype sb of
                FreeBSDBar ->
                        do
                                h <- getPipeHandle
                                io $ sendPipeCommandLine h cmd
                _          ->     return ()

getPipeHandle :: X Handle
getPipeHandle = do
        sb <- statusBarGet
        case gethandle sb of
                Just h  ->      return h
                Nothing ->
                        do
                                h <- io $ connectCommandPipe $ getpath sb
                                statusBarSetHandle sb h
                                return h

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
myLayout wsnames = onWorkspace (workspace "gfx") gimpLayout $ smartBorders $ avoidStruts $ desktopLayoutModifiers (resizableTile ||| Mirror resizableTile ||| Full)
    where
    resizableTile = Tall nmaster delta ratio
    gimpLayout = avoidStruts $ withIM (0.12) (Or (Role "gimp-toolbox") (Role "toolbox_window")) $ reflectHoriz $ withIM (0.15) (Role "gimp-dock") $ gridIM (0.15) (Role "gimp-dock") ||| resizableTile
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
-- 'className' and 'appName' are used below.
--
myManageHook :: [ String ] -> Query (Endo WindowSet)
myManageHook wsnames =
        manageDocks <+> composeAll
                [ className =? "MPlayer"		--> doFloat
                , className =? "XMessage"		--> doFloat
                , className =? "Zenity"		        --> doFloat
                , className =? "Iceweasel"		--> doShift  (getWorkspace "web")
                , className =? "Firefox"		--> doShift  (getWorkspace "web")
                , className =? "Claws-mail"		--> doShift  (getWorkspace "com")
                , className =? "Pidgin"		        --> doShift  (getWorkspace "com")
                , className =? "VBoxSDL"		--> doShift  (getWorkspace "win")
                , className =? "Gimp"		        --> doShift  (getWorkspace "gfx")
                , className =? "Inkscape"		--> doShift  (getWorkspace "gfx")
                , className =? "Dia"		        --> doShift  (getWorkspace "gfx")
                , className =? "Darktable"		--> doShift  (getWorkspace "gfx")
                , className =? "Firefox"		--> doShift  (getWorkspace "web")
                , title =? "weechat"		        --> insertPosition End Older <+> doShift  (getWorkspace "com")
                , title =? "mutt"		        --> insertPosition Master Newer <+> doShift  (getWorkspace "com")
                , isPrefixOf "libreoffice" <$> className	--> doShift (getWorkspace "ofc")
                , isPrefixOf "LibreOffice" <$> title            --> doShift (getWorkspace "ofc")
                , appName =? "libreoffice"                      --> doShift (getWorkspace "ofc")
                , isPrefixOf "newwin - " <$> appName	        --> doShift (getWorkspace "win")
                , appName  =? "desktop_window"	                --> doIgnore
                , appName  =? "kdesktop"		        --> doIgnore ]
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
myLogHook :: Handle -> FilePath -> HostConfiguration -> X ()
myLogHook dzenbar homedir conf = do
	fadeInactiveLogHook $ windowTransparency conf
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

-- left hand side, workspaces and layouts
myXmonadBar :: Int -> String
myXmonadBar screenwidth = dzenExec ++ " -x 0 -ta l -w " ++
        (show $ myXmonadBarWidth screenwidth)

xconfig conf dzenbar homedir screenwidth = defaultConfig
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
			logHook            = myLogHook dzenbar homedir conf,
			startupHook        = startup homedir screenwidth conf
		}
                where wsnames = workspaceNames conf

-- startup hook reading and executing .startup file
startup :: FilePath -> Int -> HostConfiguration -> X()
startup homedir screenwidth conf = do
        statusbartype <- startStatusBar homedir screenwidth conf
        statusBarPut $ StatusBarStatus statusbartype Nothing (pipeFileName homedir)
        autostartAllPrograms $ autostartPrograms conf
        return ()

autostartAllPrograms :: [ ExecuteCommand ] -> X ()
autostartAllPrograms =
        mapM_ execprog
        where execprog prog = spawn $ (fst prog) ++ " " ++ (unwords $ snd prog)

main = do
	homedir <- getHomeDirectory
	screenwidth <- readScreenWidthIO
	dzenbar <- spawnPipe $ myXmonadBar screenwidth
        conf <- readHostConfiguration homedir
        hPutStrLn stderr $ show conf
	xmonad $ xconfig conf dzenbar homedir screenwidth
