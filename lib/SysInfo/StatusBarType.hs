
{-# LANGUAGE DeriveDataTypeable #-}

module SysInfo.StatusBarType
        (StatusBarType(ConkyBar,FreeBSDBar)
        ,statusBarPut
        ,statusBarGet
        ) where

import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Core

data StatusBarType = NoBar | ConkyBar | FreeBSDBar deriving Typeable

instance ExtensionClass StatusBarType where
        initialValue = NoBar

statusBarPut :: StatusBarType -> X()
statusBarPut = XS.put

statusBarGet :: X StatusBarType
statusBarGet = XS.get
