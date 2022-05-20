--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad

import XMonad.Hooks.ManageDocks
import XMonad.Util.SpawnOnce
import XMonad.Util.Run

import Data.Monoid

import Personal.Settings
import Personal.KeyBindings
import Personal.Workspace
import Personal.Hooks


------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
    xmproc0 <- spawnPipe "xmobar -x 0 $HOME/.xmonad/xmobar/xmobarrc0"
    xmproc1 <- spawnPipe "xmobar -x 1 $HOME/.xmonad/xmobar/xmobarrc1"
    xmproc2 <- spawnPipe "xmobar -x 2 $HOME/.xmonad/xmobar/xmobarrc2"
    xmonad $ docks def {

        -- General Settings
        terminal           = Personal.Settings.myTerminal,
        focusFollowsMouse  = Personal.Settings.myFocusFollowsMouse,
        clickJustFocuses   = Personal.Settings.myClickJustFocuses,
        borderWidth        = Personal.Settings.myBorderWidth,
        modMask            = Personal.Settings.myModMask,
        normalBorderColor  = Personal.Settings.myNormalBorderColor,
        focusedBorderColor = Personal.Settings.myFocusedBorderColor,

        -- Workspace
        workspaces         = Personal.Workspace.myWorkspaces,

        -- Key bindings
        keys               = Personal.KeyBindings.myKeys,
        mouseBindings      = Personal.KeyBindings.myMouseBindings,

        -- hooks, layouts
        layoutHook         = Personal.Hooks.myLayout,
        manageHook         = Personal.Hooks.myManageHook,
        handleEventHook    = Data.Monoid.mempty,
        logHook            = Personal.Hooks.myLogHook xmproc0 xmproc1 xmproc2,
        startupHook        = Personal.Hooks.myStartupHook

    }
 
