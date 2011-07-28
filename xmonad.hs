import XMonad hiding ( (|||) )
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GridSelect
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.Search
import XMonad.Actions.SinkAll
import XMonad.Actions.WindowBringer
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Combo
import XMonad.Layout.DragPane
import XMonad.Layout.DwmStyle
import XMonad.Layout.IndependentScreens
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
import XMonad.Prompt
import XMonad.Prompt.AppLauncher as AL
import XMonad.Prompt.Input
import XMonad.Prompt.Workspace
import XMonad.Util.Run ( spawnPipe )
import XMonad.Util.Scratchpad
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import System.IO ( hPutStrLn )

myWorkspaces =
	[ "web"
	, "build"
	, "code"
	, "test"
	, "ref"
	, "mail"
	, "system"
	, "im"
	, "notes"
	, "misc"
	, "music"
    ]

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
    , ((modm,               xK_p            ), spawn "dmenu_run")
    , ((modm .|. shiftMask, xK_p            ), spawn "gnome-do")
    , ((modm,               xK_o            ), scratchpadSpawnActionCustom scratchpadSpawn )
    , ((modm,               xK_b            ), spawn "tomboy-panel" )
    , ((modm .|. shiftMask, xK_b            ), spawn "tomboy --new-note" )
    , ((modm,               xK_f            ), gotoMenu )
    , ((modm .|. shiftMask, xK_f            ), bringMenu )
    , ((modm .|. shiftMask, xK_a            ), inputPrompt defaultXPConfig "Workspace" ?+ addWorkspace )
    , ((modm,               xK_r            ), renameWorkspace defaultXPConfig)
    , ((modm .|. shiftMask, xK_r            ), removeWorkspace )
    , ((modm,               xK_m            ), selectWorkspace defaultXPConfig )
    , ((modm .|. shiftMask, xK_m            ), workspacePrompt defaultXPConfig (windows . W.shift))
    , ((modm,               xK_w            ), goToSelected defaultGSConfig)
    , ((modm .|. shiftMask, xK_Right), sendMessage $ Move R)
    , ((modm .|. shiftMask, xK_Left ), sendMessage $ Move L)
    , ((modm .|. shiftMask, xK_Up   ), sendMessage $ Move U)
    , ((modm .|. shiftMask, xK_Down ), sendMessage $ Move D)
    ]

    ++
    zip (zip (repeat (modm)) [xK_1..xK_9]) (map (withNthWorkspace W.greedyView) [0..])
    ++
    zip (zip (repeat (modm .|. shiftMask)) [xK_1..xK_9]) (map (withNthWorkspace W.shift) [0..])

newKeys x = M.union (myKeys x) (keys desktopConfig x)

myManageHook = composeAll
	[ title     =? "Quick Tabs" --> doIgnore
	, className =? "deskbar-applet" --> doIgnore
	, resource =? "Do" --> doIgnore
	, manageDocks
	, scratchpadManageHookDefault
	]

myLayout = dwmStyle shrinkText defaultTheme
                $ desktopLayoutModifiers
                $ windowNavigation
                $ layoutHook desktopConfig
                ||| custom
            where tall = ResizableTall 1 (3/100) (1/2) []
                  term = named "term" $ tall ****//* Full
                  full = noBorders Full
                  custom = term ||| full

main = do
  nScreens <- countScreens
  xmproc <- spawnPipe "xmobar"
  xmonad $ withUrgencyHook NoUrgencyHook $ desktopConfig {
       modMask = mod4Mask,
       focusedBorderColor = "blue",
       workspaces = myWorkspaces,
       keys = newKeys,
       manageHook = myManageHook <+> manageHook desktopConfig,
       layoutHook = myLayout,
       logHook = dynamicLogWithPP $ xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 70
                        -- , ppHiddenNoWindows = id
                        , ppSort = fmap (.scratchpadFilterOutWorkspace) $ ppSort defaultPP
                        }
  }
