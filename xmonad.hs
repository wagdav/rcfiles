import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import qualified Data.Map as M

import XMonad.Actions.CycleWS
import XMonad.Util.Scratchpad
import qualified XMonad.StackSet as W

-- The main function.
main = xmonad =<< statusBar myBar myPP toggleStrutsKey 
    (withUrgencyHook NoUrgencyHook $ myConfig)

-- Command to launch the bar.
myBar = "xmobar"

myManageHook = composeAll
    [ className =? "com-mathworks-util-PostVMInit" --> doFloat
    , className =? "Ipython" --> doFloat
    , title =? "File Transfers" --> doFloat
    ]
newManageHook =
    scratchpadManageHook (W.RationalRect 0.1 0.25 0.8 0.5) <+>
    myManageHook <+>
    manageHook defaultConfig

-- It determines what's being written to the bar.
myPP = xmobarPP
    { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">"
    , ppTitle = xmobarColor "green" "" . shorten 80
    , ppUrgent = xmobarColor "red" "" . ('^':)
    }

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- Key bindings
myKeys conf@(XConfig {XMonad.modMask = modm}) = 
    -- MPD keys
    [ ((0, 0x1008ff14), spawn "mpc toggle") -- play/pause song
    , ((0, 0x1008ff15), spawn "mpc stop") -- stop song
    , ((0, 0x1008ff16), spawn "mpc prev") -- previous song
    , ((0, 0x1008ff17), spawn "mpc next") -- next song
    -- CycleWS
    , ((modm, xK_z), toggleWS)
    -- ScratchPad
    , ((modm, xK_s), scratchpadSpawnAction defaultConfig)
    ]
newKeys x = M.union
     (keys defaultConfig x)
     (M.fromList (myKeys x))

myConfig = defaultConfig 
    { modMask = mod4Mask
    , borderWidth=2
    , manageHook = newManageHook
    , startupHook = setWMName "LG3D"
    , keys = newKeys
    }

