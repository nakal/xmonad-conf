module Settings where

import XMonad ( mod4Mask )
import XMonad.Prompt
        (
          def
        , XPConfig (font, position, bgColor, fgColor, borderColor, promptBorderWidth)
        , XPPosition (Bottom)
        )

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

promptConfig :: XPConfig
promptConfig = def
        { font = "xft:" ++ defaultFont
        , position = Bottom
        , bgColor = myBackgroundColor
        , fgColor = myDefaultColor
        , borderColor = myActiveColor
        , promptBorderWidth = myBorderWidth
        }
