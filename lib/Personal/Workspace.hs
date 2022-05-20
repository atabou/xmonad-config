module Personal.Workspace where

import XMonad
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import Data.Maybe (fromJust, isJust)

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

-- clickable ws = "<action=xdotoo key super+" ++show i++ ">" ++ws++ "</action>"
--    where i = fromJust $ M.lookup ws myWorkspaceIndices
