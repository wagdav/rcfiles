import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)
import System.IO

import XMonad.Hooks.SetWMName
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect

import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W

import XMonad.Layout.NoBorders

import Control.Monad

-- The main function.
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig
        { modMask = mod4Mask -- Rebind Mod to the Windows key
        , borderWidth=2
        , manageHook = myManageHook
        , startupHook = setWMName "LG3D"
        , logHook = myLogHook xmproc
        , layoutHook = myLayout
        , workspaces = myWorkspaces
        } `additionalKeys` myKeys


myWorkspaces = ["1:web","2:local","3:local","4:lac","5:lac","6","7","8","9"]

myKeys =
        -- MPD keys
        [ ((0, 0x1008ff14), spawn "mpc toggle") -- play/pause song
        , ((0, 0x1008ff15), spawn "mpc stop")   -- stop song
        , ((0, 0x1008ff16), spawn "mpc prev")   -- previous song
        , ((0, 0x1008ff17), spawn "mpc next")   -- next song
        -- CycleWS
        , ((mod4Mask, xK_quoteleft), toggleSkip ["NSP"])
        -- ScratchPad
        , ((mod4Mask, xK_s), scratchpadSpawnAction defaultConfig)
        , ((mod4Mask, xK_v), namedScratchpadAction scratchpads "editor")
        , ((mod4Mask, xK_b), namedScratchpadAction scratchpads "editor-lac")
        , ((mod4Mask, xK_Insert), namedScratchpadAction scratchpads "music")
        , ((mod4Mask, xK_o), namedScratchpadAction scratchpads "mail")
        -- GridSelect
        , ((mod4Mask , xK_f), goToSelected defaultGSConfig)
        ]
        ++ switchNonGreedyView


switchNonGreedyView = [
    ((m .|. mod4Mask, k), windows $ f i) -- Replace 'mod4Mask' with your mod key of choice.
    | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]


myLayout = smartBorders $ avoidStruts $ tiled ||| Mirror tiled ||| Full
    where
        tiled = smartBorders (Tall nmaster delta ratio)
        nmaster = 1
        ratio = 1/2
        delta = 3/100


myManageHook =
    scratchpadManageHook (W.RationalRect 0.1 0.25 0.8 0.5) <+>
    namedScratchpadManageHook scratchpads <+>
    composeAll
        [ className =? "com-mathworks-util-PostVMInit" --> doFloat
        , className =? "Ipython" --> doFloat
        , title =? "File Transfers" --> doFloat
        , title =? "ImageJ" --> doFloat
        , className =? "Blender" --> doFloat
        ] <+>
    manageDocks <+>
    manageHook defaultConfig


-- It determines what's being written to the bar.
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ customPP { ppOutput = hPutStrLn h }

customPP :: PP
customPP = xmobarPP
    {   ppCurrent = xmobarColor "yellow" "" . wrap "[" "]",
        ppTitle = xmobarColor "green" "" . shorten 80
    ,   ppSort = fmap (.namedScratchpadFilterOutWorkspace) $ ppSort defaultPP
    }


-- Toggle any workspace but scratchpad
toggleSkip :: [WorkspaceId] -> X ()
toggleSkip skips = do
    hs <- gets (flip skipTags skips . W.hidden . windowset)
    unless (null hs) (windows . W.view . W.tag $ head hs)


-- Scratchpads
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
    , NS "mail" "x-terminal-emulator -title Mail -e mutt"
             (title =? "Mail")
             (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
    ]
    where role = stringProperty "WM_WINDOW_ROLE"
