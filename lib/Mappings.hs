module Mappings
        ( myKeys
        , emptyKeys
        , myKeymap
        , myMouseBindings
        ) where

import qualified Data.List as L ( elemIndex, isPrefixOf )
import qualified Data.Map as M ( fromList )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )

import XMonad
import XMonad.Actions.CycleWS ( toggleWS, prevWS, nextWS )
import XMonad.Actions.Minimize ( minimizeWindow, maximizeWindow, withLastMinimized )
import XMonad.Hooks.UrgencyHook ( focusUrgent, clearUrgents )
import XMonad.Util.Paste ( pasteSelection, pasteString )
import XMonad.Prompt.ConfirmPrompt ( confirmPrompt )
import XMonad.Util.Run ( runInTerm, runProcessWithInput, safeSpawnProg )
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.NamedActions
import XMonad.Util.WindowProperties ( focusedHasProperty, Property(ClassName) )

import Settings
import qualified HostConfiguration as HC
import Contrib.Ssh ( sshPrompt )
import Contrib.Vbox ( vboxPrompt )

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys hostconf conf =
        subtitle "Applications": mkNamedKeymap conf
        [ ( "M-S-<Return>",     addName "Terminal" $ safeSpawnProg $ XMonad.terminal conf )
        , ( "C-S-<Return>",     addName "Terminal in tmux" $ runInTerm "" "tmux -2 new-session" )
        , ( "C-M1-<Return>",    addName "Terminal (root)" $ runInTerm "" "sudo -i" )
        , ( "M-p",              addName "dmenu" $ spawn $ "dmenu_run -nb '" ++ myBackgroundColor ++ "' -nf '" ++ myInactiveColor ++ "' -sb   '" ++ myActiveColor ++ "' -sf black -fn '" ++ defaultFont ++ "'" )
        , ( "M-S-p",            addName "gmrun" $ spawn "gmrun" )
        , ( "M-S-v",            addName "Edit vimrc" $ runInTerm "" "vim ~/.vim/vimrc" )
        , ( "M-x",              addName "xfe" $ spawn "xfe" )
        , ( "M-o",              addName "office" $ spawn "~/.xmonad/scripts/office.sh" )
        , ( "M-i",              addName "weechat" $ runInTerm "-title weechat" "sh -c 'tmux has-session -t weechat && tmux -2 attach-session   -d -t weechat || tmux -2 new-session -s weechat weechat'" )
        ] ++
        subtitle "Prompts": mkNamedKeymap conf
        [ ( "C-S-s", addName "ssh" $ sshPrompt promptConfig (\p -> runInTerm "" $ "ssh -t " ++ p ++ " tmux -2 new-session") )
        , ( "M-z", addName "vbox" $ vboxPrompt promptConfig )
        , ( "M-S-z", addName "rdesktop" $ spawn "~/.xmonad/scripts/rdesktop.sh" )
        ] ++
        subtitle "WM operations": mkNamedKeymap conf
        [ ( "M-q",              addName "restart" $ spawn "pkill xmobar; cd ~/.xmonad/lib && ghc --make SysInfoBar.hs ; xmonad --recompile && xmonad --restart" )
        , ( "M-S-q",            addName "exit" $ io (exitWith ExitSuccess) )
        , ( "C-M1-l",            addName "lock screen" $ spawn "xscreensaver-command -lock" )
        , ( "M-S-<Backspace>",  addName "shutdown" $ vboxProtectedBinding "shutdown" "~/.xmonad/scripts/shutdown.sh" )
        , ( "C-S-<Backspace>",  addName "reboot" $ vboxProtectedBinding "reboot" "~/.xmonad/scripts/reboot.sh" )
        ] ++
        subtitle "Window operations": mkNamedKeymap conf
        [ ( "M-S-c",            addName "kill window" kill )
        , ( "M-<Space>",        addName "next layout" $ sendMessage NextLayout )
        , ( "M-S-<Space>",      addName "reset layout" $ setLayout $ XMonad.layoutHook conf )
        , ( "M-n",              addName "refresh windows" refresh )
        , ( "M-<Tab>",          addName "focus next" $ windows W.focusDown )
        , ( "M-j",              addName "focus next" $ windows W.focusDown )
        , ( "M-k",              addName "focus prev" $ windows W.focusUp )
        , ( "M-m",              addName "focus master" $ windows W.focusMaster )
        , ( "M-u",              addName "focus urgent" focusUrgent )
        , ( "M-S-u",            addName "clear urgents" clearUrgents)
        , ( "M-<Return>",       addName "swap master" $ windows W.swapMaster)
        , ( "M-S-j",            addName "swap down" $ windows W.swapDown )
        , ( "M-S-k",            addName "swap up" $ windows W.swapUp )
        , ( "M-t",              addName "sink" $ withFocused $ windows . W.sink)
        , ( "M-h",              addName "shrink" $ sendMessage Shrink )
        , ( "M-l",              addName "expand" $ sendMessage Expand )
        , ( "M-<Down>",         addName "minimize" $ withFocused minimizeWindow )
        , ( "M-<Up>",           addName "restore" $ withLastMinimized maximizeWindow )
        , ( "M-,",              addName "master +1" $ sendMessage (IncMasterN 1) )
        , ( "M-.",              addName "master -1" $ sendMessage (IncMasterN (-1)) )
        ] ++
        subtitle "Pasting": mkNamedKeymap conf
        [ ( "M-v", addName "paste btn3" $ pasteSelection )
        , ( "M-C-v", addName "paste clip" $ runProcessWithInput "xclip" [ "-o", "-selection", "clipboard" ] "" >>= pasteString )
        ] ++
        subtitle "WS operations": mkNamedKeymap conf
        [ ( "<KP_F12>", addName "toggle" toggleWS )
        , ( "<KP_Insert>", addName "toggle" toggleWS )
        , ( "M-<Left>", addName "prev" prevWS )
        , ( "<KP_Subtract>", addName "prev" prevWS )
        , ( "M-<Right>", addName "next" nextWS )
        , ( "<KP_Add>", addName "next" nextWS )
        ]

otherKeys hostconf conf =

        -- F key / keypad digit -> change workspace
        -- mod+F key / mod + keypad digit -> shift window to workspace
        [((m, k), windows $ f i)
                | (i, k) <- zip (cycle $ XMonad.workspaces conf) (
                        [xK_F1..xK_F9] ++
                        [xK_KP_End, xK_KP_Down, xK_KP_Page_Down,
                        xK_KP_Left, xK_KP_Begin, xK_KP_Right,
                        xK_KP_Home, xK_KP_Up, xK_KP_Page_Up]
                        )
                , (f, m) <- [(W.greedyView, 0), (W.shift, modMask conf)]
        ] ++

        -- mod-[1..9] -> change workspace
        -- mod-shift-[1..9] -> shift window to workspace
        -- (fallback for notebook without F keys)
        [((m .|. modMask conf, k), windows $ f i)
                | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
                , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
        ] ++

        --
        -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
        -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
        --
        [((m .|. modMask conf, key), screenWorkspace sc >>= flip whenJust (windows . f))
                | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
                , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
        ] ++

        -- configure SSH connections from HostConfiguration to avoid
        -- leaking host names
        [((m, k), runInTerm "" $ "ssh -p" ++ port ++ " -Y -t " ++ con ++ " 'tmux -2 new-session'")
                | ((m, k), (con,port)) <- HC.sshConnections hostconf
        ]

emptyKeys c = mkKeymap c []

myKeymap :: HC.HostConfiguration -> XConfig l -> XConfig l
myKeymap hostconf conf = addDescrKeys ((mod4Mask .|. shiftMask, xK_slash), xMessage) (myKeys conf) conf
        `additionalKeys` (otherKeys hostconf conf)

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


