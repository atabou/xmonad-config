module Personal.Settings where

import GHC.Word
import XMonad

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.

myTerminal :: String
myTerminal = "alacritty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.

myBorderWidth::GHC.Word.Word32
myBorderWidth = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.

myModMask = mod4Mask

-- Border colors for unfocused and focused windows, respectively.
--

myNormalBorderColor :: String
myNormalBorderColor = "#dddddd"

myFocusedBorderColor :: String
myFocusedBorderColor = "#ff0000"


