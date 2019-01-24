module Workspaces
        ( myLogHook
        , numberedWorkspaces
        , myXmonadBar
        , getWorkspaceName
        ) where

import qualified Data.List as L ( elemIndex )
import qualified Data.Map.Strict as M
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

myPad :: HC.HostConfiguration -> String -> String
myPad hc = if HC.isSlim hc then id else (++) ""

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
                        ppCurrent           =   xmobarColor myActiveColor myBackgroundColor . myPad conf
                        , ppVisible           =   xmobarWS myDefaultColor myBackgroundColor Nothing
                        , ppHidden            =   xmobarWS myDefaultColor myBackgroundColor prevws
                        , ppHiddenNoWindows   =   xmobarWS myInactiveColor myBackgroundColor prevws
                        , ppUrgent            =   xmobarWS mySignalColor myBackgroundColor prevws
                        , ppWsSep             =   " "
                        , ppSep               =   " <fc=" ++ myInactiveColor ++ ">|</fc> "
                        , ppLayout            =   workspaceLayoutSymbol
                        , ppTitle             =   wsTitle
                        , ppOutput            =   hPutStrLn xmobar
        }
        where
                xmobarWS = xmobarWorkspace conf
                wsTitle = if HC.isSlim conf
                             then
                                const ""
                             else
                                (" " ++) . xmobarColor myActiveColor myBackgroundColor . xmobarStrip

prevWorkspace :: X (Maybe WorkspaceId)
prevWorkspace = do
        lst <- gets $ W.hidden . windowset
        case lst of
                [] -> return Nothing
                x:xs -> return $ Just $ W.tag x

xmobarWorkspace :: HC.HostConfiguration -> String -> String -> Maybe WorkspaceId -> WorkspaceId -> String
xmobarWorkspace conf fg bg prevws =
        xmobarColor fg bg . myPad conf . addAction
        where
                addAction wrkspc = "<action=`xdotool key " ++ myXDoToolKey ++
                        "+" ++ take 1 wrkspc ++ "`>" ++
                        markPrevious prevws wrkspc ++ "</action>"
                markPrevious prevws wrkspc = case prevws of
                        Just w      -> if w == wrkspc then "<fn=1>" ++ w ++ "</fn>"
                                        else wrkspc
                        _           -> wrkspc

myXmonadBar :: HC.HostConfiguration -> String
myXmonadBar conf =
        "xmobar .xmonad/" ++ barPrefix ++ "workspaces_xmobar.rc"
        where barPrefix
                | HC.isSlim conf   = "slim_"
                | otherwise     = ""

-- This function numbers the workspace names
numberedWorkspaces :: HC.HostConfiguration -> [ String ]
numberedWorkspaces conf = map name $ M.assocs (HC.workspaces conf)
        where name (i, n)
                | HC.isSlim conf || null n      = show i
                | otherwise                     = concat [show i, ":", n]

-- Safely returns a matching workspace name
getWorkspaceName :: HC.HostConfiguration -> String -> String
getWorkspaceName conf name = case M.lookup name (HC.workspaceMap conf) of
        Nothing -> last $ numberedWorkspaces conf
        Just x  -> x
