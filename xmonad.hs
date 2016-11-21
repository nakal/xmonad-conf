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
import Data.Ratio
import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
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
import XMonad.Layout.Minimize
import XMonad.Config.Desktop ( desktopLayoutModifiers )
import XMonad.Layout.Reflect ( reflectHoriz )
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Place
import XMonad.Actions.CycleWS
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.UrgencyHook

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import qualified HostConfiguration      as HC

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels
myBorderWidth   = 2

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
myModMask = mod4Mask

-- xdotool needs to map an Xmonad action to the correct
-- modifier key. This needs to be kept in sync with
-- the above myModMask to work correctly.
-- Super is the "windows key" for xdotool
myXDoToolKey = "Super"

-- Default font passed to desktop utils supporting Xft
-- (this is only dmenu for now)
defaultFont :: String
defaultFont = "Fantasque Sans Mono:size=12:bold"

-- Border colors for unfocused and focused windows, respectively.
myInactiveColor  = "#606060"
myBackgroundColor = "#202020"
myActiveColor = "#a8ff60"
myDefaultColor = "orange"
myFocusedBorderColor = myActiveColor
mySignalColor  = "red"

-- This function numbers the workspace names
numberedWorkspaces :: Bool -> [ String ] -> [ String ]
numberedWorkspaces slim wsnames = zipWith (++) (map show [1..]) $ map appendName wsnames
        where appendName name = case (slim || null name) of
                True -> ""
                _ -> (':' :) name

-- Safely returns a matching workspace name
getWorkspaceName :: Bool -> [ String ] -> String -> String
getWorkspaceName slim wsnames name = case name `elemIndex` wsnames of
        Nothing	-> show $ length wsnames
        Just x	-> (show $ x+1) ++ (
                if slim then "" else ":" ++ name
                )

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys hostconf conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ (XMonad.terminal conf) ++ " -e tmux -2 new-session")
    , ((controlMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) -- plain terminal without tmux

    -- launch dmenu
    , ((modm,               xK_p     ), spawn $ "dmenu_run -nb '" ++ myBackgroundColor ++ "' -nf '" ++ myInactiveColor ++ "' -sb '" ++ myActiveColor ++ "' -sf black -fn '" ++ defaultFont ++ "'")

    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

    -- launch vim (in various ways, with most common uses)
    , ((modm .|. shiftMask,	xK_v     ), spawn $ (XMonad.terminal conf) ++ " -e vim ~/.vim/vimrc")
    , ((modm,	xK_x     ), spawn "xfe")
    , ((modm .|. shiftMask,	xK_x     ), spawn $ (XMonad.terminal conf) ++ " -e vim ~/.xmonad/xmonad.hs")
    , ((modm,	xK_i     ), spawn $ (XMonad.terminal conf) ++ " -title weechat -e sh -c 'tmux has-session -t weechat && tmux -2 attach-session -d -t weechat || tmux -2 new-session -s weechat weechat'" )

    -- screensaver
    , ((mod1Mask .|. controlMask, xK_l     ), spawn "xscreensaver-command -lock")

    -- shutdown
    , ((modm .|. shiftMask, xK_BackSpace), vboxProtectedBinding "~/.xmonad/scripts/shutdown.sh")

    -- reboot
    , ((controlMask .|. shiftMask, xK_BackSpace), vboxProtectedBinding "~/.xmonad/scripts/reboot.sh")

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

    , ((controlMask .|. shiftMask, xK_s     ), spawn "~/.xmonad/scripts/ssh.sh" )
    , ((modm,               xK_z     ), spawn "~/.xmonad/scripts/vbox.sh" )
    , ((modm .|. shiftMask, xK_z     ), spawn "~/.xmonad/scripts/rdesktop.sh" )

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Focus urgent window
    , ((modm,               xK_u     ), focusUrgent)

    -- Clear urgent windows
    , ((modm .|. shiftMask, xK_u     ), clearUrgents)

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
    , ((modm              , xK_q     ), spawn "pkill xmobar; cd ~/.xmonad/lib && ghc --make SysInfoBar.hs ; xmonad --recompile && xmonad --restart")

    , ((0                 , xK_KP_Insert       ), toggleWS )
    , ((0                 , xK_KP_Add          ), nextWS )
    , ((0                 , xK_KP_Subtract     ), prevWS )
    , ((modm              , xK_KP_Add          ), sendMessage RestoreNextMinimizedWin )
    , ((modm              , xK_KP_Subtract     ), withFocused minimizeWindow )

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

    ++

    -- configure SSH connections from HostConfiguration to avoid
    -- leaking host names
    [
    ((m, k), spawn $ (XMonad.terminal conf) ++ " -e ssh -p" ++ port ++ " -Y -t " ++ con ++ " 'tmux -2 new-session'" )
        | ((m, k), (con,port)) <- HC.sshConnections hostconf
    ]

-- protects execution when VirtualBox is in focus
vboxProtectedBinding :: String -> X()
vboxProtectedBinding action =
    (focusedHasProperty $ ClassName "VBoxSDL") >>= \p ->
        if (not p)
            then spawn action
            else return ()

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
myLayout slim wsnames = onWorkspace (workspace "gfx") gimpLayout $ smartBorders $ avoidStruts $ desktopLayoutModifiers (resizableTile ||| Mirror resizableTile ||| Full)
    where
    resizableTile = minimize $ Tall nmaster delta ratio
    gimpLayout = avoidStruts $ withIM (0.12) (Or (Role "gimp-toolbox") (Role "toolbox_window")) $ reflectHoriz $ withIM (0.15) (Role "gimp-dock") $ gridIM (0.15) (Role "gimp-dock") ||| resizableTile
    nmaster = 1
    ratio = toRational (2/(1+sqrt(5)::Double))
    delta = 3/100
    workspace wsname = getWorkspaceName slim wsnames wsname

-- | Unfloat a window (sink)
doUnfloat :: ManageHook
doUnfloat = ask >>= \w -> doF $ W.sink w

doCenterFloat :: ManageHook
doCenterFloat = (placeHook $ fixed (1 % 2, 1 % 2)) <+> doFloat

doNotificationFloat :: ManageHook
doNotificationFloat = (placeHook $ fixed (19 % 20, 1 % 20)) <+> doFloat

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
myManageHook :: Bool -> [ String ] -> Query (Endo WindowSet)
myManageHook slim wsnames =
        manageDocks <+> composeAll
                [ className =? "MPlayer"		--> doCenterFloat
                , className =? "XMessage"		--> doCenterFloat
                , className =? "Zenity"                 --> doCenterFloat
                , className =? "xmDialog"               --> doCenterFloat
                , className =? "xmNotification"         --> doNotificationFloat
                , className =? "Iceweasel"		--> insertPosition Master Newer <+> doShift (getWorkspace "web")
                , className =? "Firefox"		--> insertPosition Master Newer <+> doShift (getWorkspace "web")
                , isPrefixOf "Vimperator Edit" <$> title --> insertPosition End Newer <+> doShift (getWorkspace "web")
                , className =? "Claws-mail"		--> doShift  (getWorkspace "com")
                , className =? "Thunderbird"		--> doShift  (getWorkspace "com")
                , className =? "Pidgin"                 --> doShift  (getWorkspace "com")
                , className =? "VBoxSDL"		--> doShift  (getWorkspace "win")
                , className =? "rdesktop"		--> doUnfloat <+> doShift  (getWorkspace "win")
                , className =? "Gimp"                   --> doShift  (getWorkspace "gfx")
                , className =? "Inkscape"		--> doShift  (getWorkspace "gfx")
                , className =? "Dia"                    --> doShift  (getWorkspace "gfx")
                , className =? "Darktable"		--> doShift  (getWorkspace "gfx")
                , title =? "weechat"                    --> insertPosition End Older <+> doShift  (getWorkspace "com")
                , title =? "mutt"                       --> insertPosition Master Newer <+> doShift  (getWorkspace "com")
                , isPrefixOf "OpenOffice" <$> className	--> doShift (getWorkspace "ofc")
                , isPrefixOf "libreoffice" <$> className	--> doShift (getWorkspace "ofc")
                , isPrefixOf "LibreOffice" <$> title            --> doShift (getWorkspace "ofc")
                , appName =? "libreoffice"                      --> doShift (getWorkspace "ofc")
                , isPrefixOf "newwin - " <$> appName            --> doShift (getWorkspace "win")
                , appName  =? "desktop_window"                  --> doIgnore
                , appName  =? "kdesktop"                        --> doIgnore ]
                        where getWorkspace name = getWorkspaceName slim wsnames name

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

myPad :: Bool -> String -> String
myPad False s = s ++ " "
myPad _ s = s

-- Workspace mode symbol
workspaceLayoutSymbol :: String -> String
workspaceLayoutSymbol modestr =
        "<action=`xdotool key " ++ myXDoToolKey ++ "+space`>" ++
                (case modestr of
                "Tall"                      ->      "<fn=1>\xf0db</fn>"
                "Minimize Tall"             ->      "<fn=1>\xf0db</fn>"
                "ResizableTall"             ->      "<fn=1>\xf0db</fn>"
                "Mirror Tall"               ->      "<fn=1>\xf01e</fn>"
                "Mirror ResizableTall"      ->      "<fn=1>\xf01e</fn>"
                "Full"                      ->      "<fn=1>\xf108</fn>"
                "Simple Float"              ->      "<fn=1>\xf24d</fn>"
                "IM ReflectX IM IM Grid"    ->      "<fn=1>\xf1fc</fn>"
                _                           ->      modestr
                ) ++ "</action>"

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook :: Handle -> HC.HostConfiguration -> X ()
myLogHook xmobar conf = do
        prevws <- prevWorkspace
        dynamicLogWithPP $
                defaultPP {
                        ppCurrent           =   xmobarColor myActiveColor myBackgroundColor . (myPad $ HC.slimView conf)
                        , ppVisible           =   xmobarWS myDefaultColor myBackgroundColor Nothing
                        , ppHidden            =   xmobarWS myDefaultColor myBackgroundColor prevws
                        , ppHiddenNoWindows   =   xmobarWS myInactiveColor myBackgroundColor prevws
                        , ppUrgent            =   xmobarWS mySignalColor myBackgroundColor prevws
                        , ppWsSep             =   " "
                        , ppSep               =   "  <fc=" ++ myInactiveColor ++ "><fn=1>\xf142</fn></fc>  "
                        , ppLayout            =   workspaceLayoutSymbol
                        , ppTitle             =   (" " ++) . xmobarColor myActiveColor myBackgroundColor . xmobarStrip
                        , ppOutput            =   hPutStrLn xmobar
        }
        where xmobarWS = xmobarWorkspace (HC.slimView conf)

prevWorkspace :: X (Maybe WorkspaceId)
prevWorkspace = do
        lst <- gets $ W.hidden . windowset
        case lst of
                [] -> return Nothing
                x:xs -> return $ Just $ W.tag x

xmobarWorkspace :: Bool -> String -> String -> Maybe WorkspaceId -> WorkspaceId -> String
xmobarWorkspace slim fg bg prevws =
        xmobarColor fg bg . (myPad slim) . addAction
        where
                addAction wrkspc = "<action=`xdotool key " ++ myXDoToolKey ++
                        "+" ++ (take 1 wrkspc) ++ "`>" ++
                        (markPrevious prevws wrkspc) ++ "</action>"
                markPrevious prevws wrkspc = case prevws of
                        Just w      -> if w == wrkspc then "<fn=2>" ++ w ++ "</fn>"
                                        else wrkspc
                        _           -> wrkspc

myXmonadBar :: String
myXmonadBar = "xmobar .xmonad/workspaces_xmobar.rc"

xconfig conf xmobar = withUrgencyHook NoUrgencyHook $ defaultConfig
        {
                terminal           = HC.terminal conf,
                focusFollowsMouse  = myFocusFollowsMouse,
                clickJustFocuses   = myClickJustFocuses,
                borderWidth        = myBorderWidth,
                modMask            = myModMask,
                workspaces         = numberedWorkspaces (HC.slimView conf) wsnames,
                normalBorderColor  = myInactiveColor,
                focusedBorderColor = myFocusedBorderColor,

                keys               = myKeys conf,
                mouseBindings      = myMouseBindings,

                layoutHook         = myLayout (HC.slimView conf) wsnames,
                manageHook         = myManageHook (HC.slimView conf) wsnames,
                handleEventHook    = myEventHook,
                logHook            = myLogHook xmobar conf,
                startupHook        = autostartAllPrograms conf
        }
        where wsnames = HC.workspaceNames conf

autostartAllPrograms :: HC.HostConfiguration -> X ()
autostartAllPrograms conf = do
        spawn $ "~/.xmonad/lib/SysInfoBar " ++ HC.locale conf ++ " " ++ HC.netInterfaceName conf
        mapM_ execprog $ HC.autostartPrograms conf
        where execprog prog = spawn $ (fst prog) ++ " " ++ (unwords $ snd prog)

main = do
        homedir <- getHomeDirectory
        conf <- HC.readHostConfiguration homedir
        hPutStrLn stderr $ show conf
        xmobar <- spawnPipe myXmonadBar
        xmonad $ xconfig conf xmobar
