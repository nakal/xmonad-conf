module Mappings
        ( myKeys
        , myMouseBindings
        ) where

import qualified Data.List as L ( elemIndex, isPrefixOf )
import qualified Data.Map as M ( fromList )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )

import XMonad
import XMonad.Actions.CycleWS ( toggleWS, prevWS, nextWS )
import XMonad.Hooks.UrgencyHook ( focusUrgent, clearUrgents )
import XMonad.Layout.Minimize ( minimizeWindow, MinimizeMsg(RestoreNextMinimizedWin) )
import XMonad.Util.Paste ( pasteSelection, pasteString )
import XMonad.Prompt.ConfirmPrompt ( confirmPrompt )
import XMonad.Util.Run ( runInTerm, runProcessWithInput, safeSpawnProg )
import qualified XMonad.StackSet as W
import XMonad.Util.WindowProperties ( focusedHasProperty, Property(ClassName) )

import Settings
import qualified HostConfiguration as HC
import Contrib.Ssh ( sshPrompt )
import Contrib.Vbox ( vboxPrompt )

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys hostconf conf = M.fromList $ let modm = modMask conf in

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), safeSpawnProg $ XMonad.terminal conf)
    , ((controlMask .|. shiftMask, xK_Return),  runInTerm "" "tmux -2 new-session")

    -- launch dmenu
    , ((modm,               xK_p     ), spawn $ "dmenu_run -nb '" ++ myBackgroundColor ++ "' -nf '" ++ myInactiveColor ++ "' -sb '" ++ myActiveColor ++ "' -sf black -fn '" ++ defaultFont ++ "'")

    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

    -- launch vim (in various ways, with most common uses)
    , ((modm .|. shiftMask, xK_v     ), runInTerm "" "vim ~/.vim/vimrc")
    , ((modm,               xK_x     ), spawn "xfe")
    , ((modm .|. shiftMask, xK_x     ), runInTerm "" "vim ~/.xmonad/xmonad.hs")
    , ((modm,               xK_i     ), runInTerm "-title weechat" "sh -c 'tmux has-session -t weechat && tmux -2 attach-session -d -t weechat || tmux -2 new-session -s weechat weechat'")

    -- screensaver
    , ((mod1Mask .|. controlMask, xK_l     ), spawn "xscreensaver-command -lock")

    -- shutdown
    , ((modm .|. shiftMask, xK_BackSpace), vboxProtectedBinding "shutdown" "~/.xmonad/scripts/shutdown.sh")

    -- reboot
    , ((controlMask .|. shiftMask, xK_BackSpace), vboxProtectedBinding "reboot" "~/.xmonad/scripts/reboot.sh")

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

    , ((shiftMask .|. controlMask, xK_s),
        sshPrompt promptConfig (\p -> runInTerm "" $ "ssh -t " ++ p ++ " tmux -2 new-session"))
    , ((modm,               xK_z     ), vboxPrompt promptConfig)
    , ((modm .|. shiftMask, xK_z     ), spawn "~/.xmonad/scripts/rdesktop.sh" )

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Focus urgent window
    , ((modm,               xK_u     ), focusUrgent)

    -- Clear urgent windows
    , ((modm .|. shiftMask, xK_u     ), clearUrgents)

    -- Paste from mouse selection
    , ((modm            ,       xK_v ), pasteSelection)

    -- Paste from clipboard
    , ((modm .|. controlMask,   xK_v ), runProcessWithInput "xclip" [ "-o", "-selection", "clipboard" ] "" >>= pasteString )

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

    , ((0                 , xK_F12       ), toggleWS )
    , ((modm              , xK_Left      ), prevWS )
    , ((modm              , xK_Right     ), nextWS )
    , ((modm              , xK_Down      ), withFocused minimizeWindow )
    , ((modm              , xK_Up        ), sendMessage RestoreNextMinimizedWin )
    , ((0                 , xK_KP_Insert       ), toggleWS )
    , ((0                 , xK_KP_Add          ), nextWS )
    , ((0                 , xK_KP_Subtract     ), prevWS )
    , ((modm              , xK_KP_Add          ), sendMessage RestoreNextMinimizedWin )
    , ((modm              , xK_KP_Subtract     ), withFocused minimizeWindow )

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    -- , ((modMask .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    ++

    -- F key / keypad digit -> change workspace
    -- mod+F key / mod + keypad digit -> shift window to workspace
    [((m, k), windows $ f i)
        | (i, k) <- zip (cycle $ XMonad.workspaces conf) (
                [xK_F1..xK_F9] ++
                [xK_KP_End, xK_KP_Down, xK_KP_Page_Down,
                 xK_KP_Left, xK_KP_Begin, xK_KP_Right,
                 xK_KP_Home, xK_KP_Up, xK_KP_Page_Up]
                )
        , (f, m) <- [(W.greedyView, 0), (W.shift, modm)]]
    ++

    -- mod-[1..9] -> change workspace
    -- mod-shift-[1..9] -> shift window to workspace
    -- (fallback for notebook without F keys)
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
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
    ((m, k), runInTerm "" $ "ssh -p" ++ port ++ " -Y -t " ++ con ++ " 'tmux -2 new-session'")
        | ((m, k), (con,port)) <- HC.sshConnections hostconf
    ]

-- protects execution when VirtualBox is in focus
vboxProtectedBinding :: String -> String -> X()
vboxProtectedBinding msg action =
    (focusedHasProperty $ ClassName "VBoxSDL") >>= \p ->
        if (not p)
            then confirmPrompt promptConfig msg (io $ spawn action)
            else return ()

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings conf = M.fromList $ let modm = modMask conf in

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


