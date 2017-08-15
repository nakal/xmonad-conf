module Workspaces
        ( myLogHook
        , numberedWorkspaces
        , myXmonadBar
        , getWorkspaceName
        ) where

import qualified Data.List as L ( elemIndex )
import System.IO ( Handle, hPutStrLn )

import XMonad
import XMonad.Hooks.DynamicLog (
        dynamicLogWithPP
        , ppCurrent
        , ppHidden
        , ppHiddenNoWindows
        , ppLayout
        , ppOutput
        , ppSep
        , ppTitle
        , ppUrgent
        , ppVisible
        , ppWsSep
        , xmobarColor
        , xmobarStrip
        )
import qualified XMonad.StackSet as W

import qualified HostConfiguration as HC
import Settings

myPad :: HC.SysInfoBarMode -> String -> String
myPad HC.Slim = id
myPad HC.Full = (++) " "

-- Workspace mode symbol
workspaceLayoutSymbol :: String -> String
workspaceLayoutSymbol modestr =
        "<action=`xdotool key " ++ myXDoToolKey ++ "+space`>" ++
                (case modestr of
                "Minimize Tall"             ->      "Tall"
                "ResizableTall"             ->      "Tall"
                "Mirror Tall"               ->      "MTall"
                "Mirror Minimize Tall"      ->      "MTall"
                "Mirror ResizableTall"      ->      "MTall"
                "Simple Float"              ->      "Float"
                "IM ReflectX IM IM Grid"    ->      "Gimp"
                _                           ->      modestr
                ) ++ "</action>"

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook :: Handle -> HC.HostConfiguration -> X ()
myLogHook xmobar conf = do
        prevws <- prevWorkspace
        dynamicLogWithPP $
                def {
                        ppCurrent           =   xmobarColor myActiveColor myBackgroundColor . (myPad $ HC.barMode conf)
                        , ppVisible           =   xmobarWS myDefaultColor myBackgroundColor Nothing
                        , ppHidden            =   xmobarWS myDefaultColor myBackgroundColor prevws
                        , ppHiddenNoWindows   =   xmobarWS myInactiveColor myBackgroundColor prevws
                        , ppUrgent            =   xmobarWS mySignalColor myBackgroundColor prevws
                        , ppWsSep             =   " "
                        , ppSep               =   " <fc=" ++ myInactiveColor ++ ">|</fc> "
                        , ppLayout            =   workspaceLayoutSymbol
                        , ppTitle             =   wsTitle (HC.barMode conf)
                        , ppOutput            =   hPutStrLn xmobar
        }
        where
                xmobarWS = xmobarWorkspace (HC.barMode conf)
                wsTitle mode
                        | mode == HC.Slim = \_ -> ""
                        | mode == HC.Full = (" " ++) . xmobarColor myActiveColor myBackgroundColor . xmobarStrip

prevWorkspace :: X (Maybe WorkspaceId)
prevWorkspace = do
        lst <- gets $ W.hidden . windowset
        case lst of
                [] -> return Nothing
                x:xs -> return $ Just $ W.tag x

xmobarWorkspace :: HC.SysInfoBarMode -> String -> String -> Maybe WorkspaceId -> WorkspaceId -> String
xmobarWorkspace mode fg bg prevws =
        xmobarColor fg bg . (myPad mode) . addAction
        where
                addAction wrkspc = "<action=`xdotool key " ++ myXDoToolKey ++
                        "+" ++ (take 1 wrkspc) ++ "`>" ++
                        (markPrevious prevws wrkspc) ++ "</action>"
                markPrevious prevws wrkspc = case prevws of
                        Just w      -> if w == wrkspc then "<fn=1>" ++ w ++ "</fn>"
                                        else wrkspc
                        _           -> wrkspc

myXmonadBar :: HC.SysInfoBarMode -> String
myXmonadBar mode =
        "xmobar .xmonad/" ++ barPrefix ++ "workspaces_xmobar.rc"
        where barPrefix
                | mode == HC.Slim = "slim_"
                | mode == HC.Full = ""

-- This function numbers the workspace names
numberedWorkspaces :: HC.SysInfoBarMode -> [ String ] -> [ String ]
numberedWorkspaces mode wsnames = zipWith (++) (map show [1..]) $ map appendName wsnames
        where appendName name
                | mode == HC.Slim || null name = ""
                | otherwise = (':' :) name

-- Safely returns a matching workspace name
getWorkspaceName :: HC.SysInfoBarMode -> [ String ] -> String -> String
getWorkspaceName mode wsnames name = case name `L.elemIndex` wsnames of
        Nothing -> show $ length wsnames
        Just x  -> (show $ x+1) ++ suffix
        where suffix
                | mode == HC.Slim = ""
                | mode == HC.Full = ":" ++ name
