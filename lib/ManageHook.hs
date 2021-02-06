module ManageHook
        ( myManageHook
        ) where

import qualified Data.List as L ( isPrefixOf )
import Data.Monoid ( Endo )
import Data.Ratio ( (%) )

import XMonad
import XMonad.Hooks.InsertPosition ( insertPosition, Position(Master, End), Focus(Newer, Older) )
import XMonad.Hooks.ManageDocks ( manageDocks )
import XMonad.Hooks.Place ( placeHook, fixed )
import qualified XMonad.StackSet as W

import qualified HostConfiguration as HC
import Workspaces

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
myManageHook :: HC.HostConfiguration -> Query (Endo WindowSet)
myManageHook conf =
        manageDocks <+> composeAll
                [ className =? "MPlayer"                --> doCenterFloat
                , className =? "XMessage"               --> doCenterFloat
                , className =? "Zenity"                 --> doCenterFloat
                , className =? "xmDialog"               --> doCenterFloat
                , className =? "xmNotification"         --> doNotificationFloat
                , title =? "Calendar"                   --> doNotificationFloat
                , className =? "Iceweasel"              --> insertPosition End Newer <+> doShift (getWorkspace "web")
                , className =? "Firefox"                --> insertPosition Master Newer <+> doShift (getWorkspace "web")
                , className =? "Firefox-esr"                --> insertPosition Master Newer <+> doShift (getWorkspace "web")
                , L.isPrefixOf "Vimperator Edit" <$> title --> insertPosition End Newer <+> doShift (getWorkspace "web")
                , className =? "Chromium-browser"       --> insertPosition Master Newer <+> doShift (getWorkspace "web")
                , className =? "Claws-mail"             --> doShift  (getWorkspace "com")
                , className =? "Thunderbird"            --> insertPosition End Newer <+> doShift  (getWorkspace "com")
                , className =? "Pidgin"                 --> doShift  (getWorkspace "com")
                , className =? "VBoxSDL"                --> doShift  (getWorkspace "win")
                , className =? "rdesktop"               --> doUnfloat <+> doShift  (getWorkspace "win")
                , className =? "Gimp"                   --> doShift  (getWorkspace "gfx")
                , className =? "Inkscape"               --> doShift  (getWorkspace "gfx")
                , className =? "Dia"                    --> doShift  (getWorkspace "gfx")
                , className =? "Darktable"              --> doShift  (getWorkspace "gfx")
                , title =? "weechat"                    --> insertPosition End Older <+> doShift  (getWorkspace "com")
                , title =? "mutt"                       --> insertPosition Master Newer <+> doShift  (getWorkspace "com")
                , title =? "neomutt"                    --> insertPosition Master Newer <+> doShift  (getWorkspace "com")
                , L.isPrefixOf "OpenOffice" <$> className       --> doShift (getWorkspace "ofc")
                , L.isPrefixOf "libreoffice" <$> className      --> doShift (getWorkspace "ofc")
                , L.isPrefixOf "LibreOffice" <$> title            --> doShift (getWorkspace "ofc")
                , appName =? "libreoffice"                      --> doShift (getWorkspace "ofc")
                , L.isPrefixOf "newwin - " <$> appName            --> doShift (getWorkspace "win")
                , appName  =? "desktop_window"                  --> doIgnore
                , appName  =? "kdesktop"                        --> doIgnore ]
        where getWorkspace = getWorkspaceName conf

-- | Unfloat a window (sink)
doUnfloat :: ManageHook
doUnfloat = ask >>= \w -> doF $ W.sink w

doCenterFloat :: ManageHook
doCenterFloat = placeHook (fixed (1 % 2, 1 % 2)) <+> doFloat

doNotificationFloat :: ManageHook
doNotificationFloat = placeHook (fixed (19 % 20, 1 % 20)) <+> doFloat

