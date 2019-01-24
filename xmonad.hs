-- xmonad configuration
-- see: https://github.com/nakal/xmonad-conf

{-# LANGUAGE FlexibleContexts #-}

import Control.Applicative ((<$>))
import Data.Default
import System.IO ( stderr, hPrint )
import System.Info ( os )

import XMonad
import XMonad.Hooks.UrgencyHook
        ( withUrgencyHook
        , NoUrgencyHook(NoUrgencyHook)
        )
import XMonad.Util.Run ( spawnPipe )

import qualified Events as EV ( myEventHook )
import qualified HostConfiguration as HC
import qualified Layout as LA ( myLayout )
import qualified Mappings as M ( myKeys, emptyKeys, myKeymap, myMouseBindings )
import qualified ManageHook as MH ( myManageHook )
import qualified Settings as S
        ( myFocusFollowsMouse
        , myClickJustFocuses
        , myBorderWidth
        , myModMask
        , myInactiveColor
        , myFocusedBorderColor
        )
import qualified Workspaces as WS
        ( numberedWorkspaces
        , myXmonadBar
        , myLogHook
        )

xconfig conf xmobar = withUrgencyHook NoUrgencyHook $ M.myKeymap conf $ def
        {
                terminal           = HC.terminal conf,
                focusFollowsMouse  = S.myFocusFollowsMouse,
                clickJustFocuses   = S.myClickJustFocuses,
                borderWidth        = S.myBorderWidth,
                modMask            = S.myModMask,
                workspaces         = WS.numberedWorkspaces conf,
                normalBorderColor  = S.myInactiveColor,
                focusedBorderColor = S.myFocusedBorderColor,

                keys               = M.emptyKeys,
                mouseBindings      = M.myMouseBindings,

                layoutHook         = LA.myLayout conf,
                manageHook         = MH.myManageHook conf,
                handleEventHook    = EV.myEventHook,
                logHook            = WS.myLogHook xmobar conf,
                startupHook        = autostartAllPrograms conf
        }

autostartAllPrograms :: HC.HostConfiguration -> X ()
autostartAllPrograms conf = do
        case os of
                "freebsd" -> spawn "~/.xmonad/lib/SysInfoBar"
                "openbsd" -> spawn $ "sysinfobar | " ++ HC.sysInfoBar conf
                "linux"   -> spawn $ "sysinfobar | " ++ HC.sysInfoBar conf
                _         -> return ()
        mapM_ execprog $ HC.autostartPrograms conf
        where execprog prog = spawn $ fst prog ++ " " ++ unwords (snd prog)

main = do
        conf <- HC.readHostConfiguration
        hPrint stderr conf
        xmobar <- spawnPipe (WS.myXmonadBar conf)
        xmonad $ xconfig conf xmobar
