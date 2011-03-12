import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.Search
import XMonad.Actions.SinkAll
import XMonad.Actions.WindowBringer
import XMonad.Config.Gnome
import XMonad.Hooks.ManageDocks
import XMonad.Layout.DwmStyle
import XMonad.Layout.IndependentScreens
import XMonad.Prompt
import XMonad.Prompt.AppLauncher as AL
import XMonad.Util.Scratchpad
import qualified Data.Map as M
import qualified XMonad.StackSet as W

myWorkspaces = ["1", "2", "3", "4", "5"]

duckduckgo = searchEngine "ddg" "https://duckduckgo.com/?q="
iddg = intelligent duckduckgo
isearch = intelligent google

scratchpadSpawn = "gnome-terminal --disable-factory --name scratchpad"

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask, xK_t            ), sinkAll)
    , ((modm,               xK_bracketright ), nextScreen )
    , ((modm,               xK_bracketleft  ), prevScreen )
    , ((modm .|. shiftMask, xK_bracketright ), shiftNextScreen )
    , ((modm .|. shiftMask, xK_bracketleft  ), shiftPrevScreen )
    , ((modm,               xK_s            ), promptSearch defaultXPConfig isearch )
    , ((modm .|. shiftMask, xK_s            ), selectSearch isearch )
    , ((modm .|. shiftMask, xK_p            ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    , ((modm,               xK_p            ), spawn "gnome-do")
    , ((modm,               xK_o            ), scratchpadSpawnActionCustom scratchpadSpawn )
    , ((modm,               xK_b            ), spawn "tomboy-panel" )
    , ((modm .|. shiftMask, xK_b            ), spawn "tomboy --new-note" )
    , ((modm,               xK_f            ), gotoMenu )
    , ((modm .|. shiftMask, xK_f            ), bringMenu )
    ]

newKeys x = M.union (myKeys x) (keys gnomeConfig x)

myManageHook = composeAll
	[ title     =? "Quick Tabs" --> doIgnore
	, className =? "deskbar-applet" --> doIgnore
	, resource =? "Do" --> doIgnore
	, manageDocks
	, scratchpadManageHookDefault
	]

main = do
  nScreens <- countScreens
  xmonad $ gnomeConfig {
       modMask = mod4Mask,
       focusedBorderColor = "blue",
       workspaces = myWorkspaces,
       keys = newKeys,
       manageHook = myManageHook <+> manageHook gnomeConfig,
       layoutHook = dwmStyle shrinkText defaultTheme (layoutHook gnomeConfig)
  }
