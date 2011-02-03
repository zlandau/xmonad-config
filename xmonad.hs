import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.SinkAll
import XMonad.Config.Gnome
import XMonad.Hooks.ManageDocks
import XMonad.Layout.DwmStyle
import XMonad.Layout.IndependentScreens
import XMonad.Util.Scratchpad
import qualified Data.Map as M
import qualified XMonad.StackSet as W

myWorkspaces = ["1", "2", "3", "4", "5"]

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask, xK_t            ), sinkAll)
    , ((modm,               xK_w            ), scratchpadSpawnAction conf)
    , ((modm,               xK_bracketright ), nextScreen )
    , ((modm,               xK_bracketleft  ), prevScreen )
    , ((modm .|. shiftMask, xK_bracketright ), shiftNextScreen )
    , ((modm .|. shiftMask, xK_bracketleft  ), shiftPrevScreen )
    ]
    ++

    -- Don't switch monitors when selecting view
    [((m .|. modm, k), windows $ onCurrentScreen f i)
             | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
             , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    -- Name screens by physical location, not ScreenID
    [((m .|. modm, k), f sc)
            | (k, sc) <- zip [xK_w, xK_e] [0..]
            , (f, m) <- [(viewScreen, 0), (sendToScreen, shiftMask)]] 

newKeys x = M.union (myKeys x) (keys gnomeConfig x)

myManageHook = scratchpadManageHookDefault <+> manageDocks

main = do
  nScreens <- countScreens
  xmonad $ gnomeConfig {
       modMask = mod4Mask,
       focusedBorderColor = "blue",
       workspaces = withScreens nScreens myWorkspaces,
       keys = newKeys,
       manageHook = myManageHook,
       layoutHook = dwmStyle shrinkText defaultTheme (layoutHook gnomeConfig)
  }
