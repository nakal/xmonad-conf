
{-# LANGUAGE DeriveDataTypeable #-}

module SysInfo.StatusBarType
        (StatusBarStatus(StatusBarStatus, gettype, gethandle)
        ,StatusBarType(ConkyBar,FreeBSDBar)
        ,statusBarPut
        ,statusBarGet
        ,statusBarSetHandle
        ) where

import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Core
import System.IO

data StatusBarType = NoBar | ConkyBar | FreeBSDBar

data StatusBarStatus = StatusBarStatus
        {gettype :: StatusBarType
        ,gethandle :: Maybe Handle
        } deriving Typeable

instance ExtensionClass StatusBarStatus where
        initialValue = StatusBarStatus NoBar Nothing

statusBarPut :: StatusBarStatus -> X()
statusBarPut = XS.put

statusBarGet :: X StatusBarStatus
statusBarGet = XS.get

statusBarSetHandle :: StatusBarStatus -> Handle -> X()
statusBarSetHandle sb h = statusBarPut $ sb { gethandle = Just h }
