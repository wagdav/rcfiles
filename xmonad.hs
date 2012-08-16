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
import XMonad.Layout.GridVariants as GV
import XMonad.Layout.ToggleLayouts
import XMonad.Hooks.ManageHelpers -- isDialog

import qualified XMonad.Prompt         as P
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.Actions.Search as S
import qualified Data.Map as M

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


myWorkspaces = ["1:web","2:local","3:local","4:remote","5:remote","6","7","8","9"]

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
        , ((mod4Mask, xK_c), namedScratchpadAction scratchpads "editor-lac")
        , ((mod4Mask, xK_Insert), namedScratchpadAction scratchpads "music")
        , ((mod4Mask, xK_o), namedScratchpadAction scratchpads "mail")
        -- GridSelect
        , ((mod4Mask , xK_f), goToSelected defaultGSConfig)

        , ((mod4Mask, xK_F12), sendMessage $ Toggle "Full")

        -- search
        , ((mod4Mask, xK_slash), SM.submap $ searchEngineMap $
                S.promptSearch P.defaultXPConfig)
        , ((mod4Mask .|. shiftMask, xK_slash), SM.submap $ searchEngineMap $ S.selectSearch)
        -- xscreensaver
        , ((mod4Mask .|. shiftMask, xK_l), spawn "xscreensaver-command -lock")
        ]
        -- ++ switchNonGreedyView


switchNonGreedyView = [
    ((m .|. mod4Mask, k), windows $ f i) -- Replace 'mod4Mask' with your mod key of choice.
    | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]

myLayout = toggle $ smartBorders $ avoidStruts $
            tiled ||| Mirror tiled ||| Full ||| grid
    where
        tiled = (Tall nmaster delta ratio)
        nmaster = 1
        ratio = 2/3
        delta = 3/100

        grid = GV.Grid (1.25)
        toggle = toggleLayouts (noBorders Full)


myManageHook =
    scratchpadManageHook (W.RationalRect 0.1 0.25 0.8 0.5) <+>
    namedScratchpadManageHook scratchpads <+>
    composeAll
        -- prevent new figure windows from stealing focus
        [ className =? "com-mathworks-util-PostVMInit" --> doFloat <+> doF W.focusDown
        , className =? "Ipython" --> doFloat <+> doF W.focusDown
        , className =? "Google-chrome" --> doShift "1:web"
        , title =? "File Transfers" --> doFloat
        , title =? "ImageJ" --> doFloat
        , className =? "Blender" --> doFloat
        , title =? "xmix" --> doFloat
        , isDialog --> doFloat
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
    [ NS "editor" "gvim --role Editor --servername 127.0.0.1"
             (role =? "Editor")
             (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
    , NS "editor-lac" "ssh lac 'gvim --role lac-editor --servername lac'"
             (role =? "lac-editor")
             (nonFloating)
    , NS "music" "x-terminal-emulator -title Music -e ncmpcpp"
             (title =? "Music")
             (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
    , NS "mail" "x-terminal-emulator -title Mail -e mutt"
             (title =? "Mail")
             (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
    ]
    where role = stringProperty "WM_WINDOW_ROLE"


searchEngineMap method = M.fromList $
       [ ((0, xK_g), method S.google)
       , ((0, xK_h), method S.hoogle)
       , ((0, xK_w), method S.wikipedia)
       , ((0, xK_i), method S.imdb)
       , ((0, xK_s), method S.scholar)
       , ((0, xK_m), method S.maps)
       , ((0, xK_d), method S.deb)
       , ((0, xK_y), method S.youtube)
       , ((0, xK_t), method S.dictionary)
       ]
