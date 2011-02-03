import XMonad
import XMonad.Config.Gnome
import qualified Data.Map as M
import XMonad.Actions.SinkAll
import XMonad.Util.Scratchpad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.DwmStyle

myKeys conf@(XConfig {XMonad.modMask = modMask}) =
    [ ((modMask .|. shiftMask, xK_t     ), sinkAll)
    , ((modMask,               xK_w     ), scratchpadSpawnAction conf)
    ]

newKeys x = M.union (keys gnomeConfig x) (M.fromList (myKeys x))

myManageHook = scratchpadManageHookDefault <+> manageDocks

main = do
	xmonad $ gnomeConfig {
		modMask = mod4Mask,
		focusedBorderColor = "blue",
        workspaces = ["code", "web", "comm"] ++ map show [4..9],
        keys = newKeys,
        manageHook = myManageHook,
        layoutHook = dwmStyle shrinkText defaultTheme (layoutHook gnomeConfig)
	}
