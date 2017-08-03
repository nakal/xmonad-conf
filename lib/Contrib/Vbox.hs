-----------------------------------------------------------------------------
-- |
-- Module      :  Contrib.Vbox
-- Copyright   :  (C) 2017 Martin Sugioarto
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :  unstable
-- Portability :  unportable
--
-- A VirtualBox prompt for XMonad
--
-----------------------------------------------------------------------------

module Contrib.Vbox
    ( -- * Usage
      -- $usage
      vboxPrompt,
      Vbox,
    ) where

import XMonad
import XMonad.Util.Run
import XMonad.Prompt

-- $usage
-- 1. In your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Prompt
-- > import Contrib.Vbox
--
-- 2. In your keybindings add something like:
--
-- >   , ((modm, xK_z), vboxPrompt def)
--
-- For detailed instruction on editing the key binding see
-- "XMonad.Doc.Extending#Editing_key_bindings".

data Vbox = Vbox

instance XPrompt Vbox where
    showXPrompt       Vbox = "Start VirtualBox: "

startVbox :: String -> X()
startVbox vmname = safeSpawn "VBoxSDL" [ "--startvm", vmname, "--nograbonclick" ]

vboxPrompt :: XPConfig -> X ()
vboxPrompt c = do
  sc <- vboxComplList
  mkXPrompt Vbox c (mkComplFunFromList sc) startVbox

vboxComplList :: X [String]
vboxComplList = do
        l <- (fmap lines $ runProcessWithInput "VBoxManage" [ "list", "vms" ] "")
        return $ map (read . head) $ filter (not . null) $ map words l
