import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect

import XMonad.Util.Run
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W

import XMonad.Layout.NoBorders

import qualified Data.Map as M
import Control.Monad
import System.IO

-- The main function.
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
     { modMask = mod4Mask
    , borderWidth=2
    , manageHook = newManageHook
    , startupHook = setWMName "LG3D"
    , keys = newKeys
    , logHook = myLogHook xmproc
    , layoutHook = myLayout
    }

myLayout = avoidStruts $ tiled ||| Mirror tiled ||| Full
    where
        tiled = smartBorders (Tall nmaster delta ratio)
        nmaster = 1
        ratio = 1/2
        delta = 3/100

myManageHook = composeAll
    [ className =? "com-mathworks-util-PostVMInit" --> doFloat
    , className =? "Ipython" --> doFloat
    , title =? "File Transfers" --> doFloat
    ]
newManageHook =
    scratchpadManageHook (W.RationalRect 0.1 0.25 0.8 0.5) <+>
    namedScratchpadManageHook scratchpads <+>
    myManageHook <+>
    manageHook defaultConfig

-- It determines what's being written to the bar.
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ customPP { ppOutput = hPutStrLn h }

customPP :: PP
customPP = defaultPP
    { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">"
    , ppTitle = xmobarColor "green" "" . shorten 80
    , ppUrgent = xmobarColor "red" "" . wrap "!" "!"
    , ppSort = fmap (.namedScratchpadFilterOutWorkspace) $ ppSort defaultPP
    }

-- Key bindings
myKeys conf@(XConfig {XMonad.modMask = modm}) = 
    -- MPD keys
    [ ((0, 0x1008ff14), spawn "mpc toggle") -- play/pause song
    , ((0, 0x1008ff15), spawn "mpc stop") -- stop song
    , ((0, 0x1008ff16), spawn "mpc prev") -- previous song
    , ((0, 0x1008ff17), spawn "mpc next") -- next song
    -- CycleWS
    , ((modm, xK_quoteleft), toggleSkip ["NSP"])
    -- ScratchPad
    , ((modm, xK_s), scratchpadSpawnAction defaultConfig)
    , ((modm, xK_v), namedScratchpadAction scratchpads "editor")
    , ((modm, xK_b), namedScratchpadAction scratchpads "editor-lac")
    , ((modm, xK_Insert), namedScratchpadAction scratchpads "music")
    -- GridSelect
    , ((modm , xK_f), goToSelected defaultGSConfig)
    ]

-- toggle any workspace but scratchpad
toggleSkip :: [WorkspaceId] -> X ()
toggleSkip skips = do
    hs <- gets (flip skipTags skips . W.hidden . windowset)
    unless (null hs) (windows . W.view . W.tag $ head hs)

newKeys x = M.union
     (keys defaultConfig x)
     (M.fromList (myKeys x))

scratchpads :: [NamedScratchpad]
scratchpads =
    [ NS "editor" "gvim --role Editor --servername crpppc312"
             (role =? "Editor")
             (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
    , NS "editor-lac" "ssh lac 'gvim --role lac-editor --servername lac'"
             (role =? "lac-editor")
             (nonFloating)
    , NS "music" "x-terminal-emulator -title Music -e ncmpc --colors"
             (title =? "Music")
             (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
    ]
    where role = stringProperty "WM_WINDOW_ROLE"
