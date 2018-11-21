module Layout
        ( myLayout
        ) where

import XMonad
import XMonad.Config.Desktop ( desktopLayoutModifiers )
import XMonad.Hooks.ManageDocks ( avoidStruts )
import XMonad.Layout.IM ( withIM, gridIM )
import XMonad.Layout.Minimize ( minimize )
import XMonad.Layout.NoBorders ( smartBorders )
import XMonad.Layout.PerWorkspace ( onWorkspace )
import XMonad.Layout.Reflect ( reflectHoriz )
import XMonad.Util.WindowProperties ( Property(Role, Or) )

import Workspaces

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout mode wsnames = onWorkspace (workspace "gfx") gimpLayout $ smartBorders $ avoidStruts $ desktopLayoutModifiers (resizableTile ||| Mirror resizableTile ||| Full)
    where
    resizableTile = minimize $ Tall nmaster delta ratio
    gimpLayout = avoidStruts $ withIM 0.12 (Or (Role "gimp-toolbox") (Role "toolbox_window")) $ reflectHoriz $ withIM 0.15 (Role "gimp-dock") $ gridIM 0.15 (Role "gimp-dock") ||| resizableTile
    nmaster = 1
    ratio = toRational (2 / (1 + sqrt 5::Double))
    delta = 3/100
    workspace = getWorkspaceName mode wsnames
