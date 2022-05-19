--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import System.Exit
import System.IO (hPutStrLn)

import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce
import XMonad.Util.Run

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName

import XMonad.Layout.Spacing

import qualified XMonad.StackSet as W

import Data.Maybe (fromJust, isJust)
import Data.Monoid
import qualified Data.Map        as M

import Personal.Settings
import Personal.KeyBindings


-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--

--myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]
myWorkspaces    = [" dev "," www "," sys "," doc "," vbox "," chat "," mus "," vid "," gfx "]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..]

clickable ws = "<action=xdotoo key super+" ++show i++ ">" ++ws++ "</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"

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



myLayout =  spacingRaw False (Border 0 10 10 10) True (Border 10 10 10 10) True $ avoidStruts (tiled ||| Mirror tiled ||| Full)
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
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

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
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
    xmproc0 <- spawnPipe "xmobar -x 0 $HOME/.xmonad/xmobar/xmobarrc0"
    xmproc1 <- spawnPipe "xmobar -x 1 $HOME/.xmonad/xmobar/xmobarrc1"
    xmproc2 <- spawnPipe "xmobar -x 2 $HOME/.xmonad/xmobar/xmobarrc2"
    xmonad $ docks def {

        -- simple stuff
        terminal           = Personal.Settings.myTerminal,
        focusFollowsMouse  = Personal.Settings.myFocusFollowsMouse,
        clickJustFocuses   = Personal.Settings.myClickJustFocuses,
        borderWidth        = Personal.Settings.myBorderWidth,
        modMask            = Personal.Settings.myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

        -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

        -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = dynamicLogWithPP $ namedScratchpadFilterOutWorkspacePP $ xmobarPP {
    
            -- Status bars and logging:
            -- Perform an arbitrary action on each internal state change or X event.
            -- See the 'XMonad.Hooks.DynamicLog' extension for examples.
            --

              ppOutput             = \x -> hPutStrLn xmproc0 x
                                        >> hPutStrLn xmproc1 x
                                        >> hPutStrLn xmproc2 x
            , ppCurrent            = xmobarColor "#c792ea" "" . wrap "[" "]" 
            , ppVisible            = xmobarColor "#c792ea" "" . clickable
            , ppHidden             = xmobarColor "#82AAFF" "" . wrap "*" "" . clickable -- Hidden workspaces
            , ppHiddenNoWindows    = xmobarColor "#82AAFF" "" . clickable                                                             -- Hidden workspaces (no windows)
            , ppTitle              = xmobarColor "#b3afc2" "" . shorten 60                                                            -- Title of active window
            , ppSep                = "<fc=#666666> <fn=1>|</fn> </fc>"                                                                -- Separator character
            , ppUrgent             = xmobarColor "#C45500" "" . wrap "!" "!"                                                          -- Urgent workspace
            , ppExtras             = [windowCount]                                                                                    -- # of windows current workspace
            , ppOrder              = \(ws:l:t:ex) -> [ws,l]++ex++[t]                                                                  -- order of things in xmobar

        },

        startupHook        = myStartupHook

    }
 
