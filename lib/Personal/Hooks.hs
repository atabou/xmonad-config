module Personal.Hooks where

import XMonad
import System.IO (hPutStrLn)
import Data.Monoid

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName

import XMonad.Layout.Spacing

import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce
import XMonad.Util.Run

import Personal.Workspace

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
    -- which denotes layout choice.

myLayout =  spacingRaw False (Border 44 10 10 10) True (Border 10 10 10 10) True $ avoidStruts (tiled ||| Mirror tiled ||| Full)
    where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio   = 1/2

    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

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
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll [ 

      className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore 

    ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
-- myEventHook = Data.Monoid.mempty

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
    setWMName "LG3D"
    spawnOnce "nitrogen --restore &"
    spawnOnce "picom --experimental-backends -b &"



------------------------------------------------------------------------

-- Status bars and logging:
    -- Perform an arbitrary action on each internal state change or X event.
    -- See the 'XMonad.Hooks.DynamicLog' extension for examples.

myLogHook xmproc0 xmproc1 xmproc2 = dynamicLogWithPP $ namedScratchpadFilterOutWorkspacePP $ xmobarPP {
      ppOutput = \x -> hPutStrLn xmproc0 x
                    >> hPutStrLn xmproc1 x
                    >> hPutStrLn xmproc2 x
    , ppCurrent            = xmobarColor "#c792ea" "" . wrap "[" "]" 
    , ppVisible            = xmobarColor "#c792ea" "" -- . clickable
    , ppHidden             = xmobarColor "#82AAFF" "" . wrap "*" "" -- . clickable -- Hidden workspaces
    , ppHiddenNoWindows    = xmobarColor "#82AAFF" "" -- . clickable                                                             -- Hidden workspaces (no windows)
    , ppTitle              = xmobarColor "#b3afc2" "" . shorten 60                                                            -- Title of active window
    , ppSep                = "<fc=#666666> <fn=1>|</fn> </fc>"                                                                -- Separator character
    , ppUrgent             = xmobarColor "#C45500" "" . wrap "!" "!"                                                          -- Urgent workspace
    , ppExtras             = [windowCount]                                                                                    -- # of windows current workspace
    , ppOrder              = \(ws:l:t:ex) -> [ws,l]++ex++[t]                                                                  -- order of things in xmobar

}




