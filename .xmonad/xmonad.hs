import Data.List
import Control.Monad
import System.IO
import qualified Data.Map as M

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.Volume
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.ToggleLayouts
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Scratchpad
import XMonad.Util.Themes
import XMonad.Prompt.Pass (passPrompt)
import qualified XMonad.Actions.Search as S
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.Prompt.Shell   as PShell
import qualified XMonad.StackSet       as W


-- The main function.
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ ewmh $ def
        { modMask = mod4Mask -- Rebind Mod to the Windows key
        , borderWidth = 2
        , manageHook = myManageHook
        , logHook = myLogHook xmproc
        , layoutHook = myLayout
        , workspaces = myWorkspaces
        , handleEventHook = myHandleEventHook
        } `additionalKeys` myKeys


myWorkspaces = ["1:web","2:local","3:local","4:remote","5:remote","6","7","8","9"]

myKeys =
        -- MPD keys
        [ ((0, 0x1008ff14), spawn "mpc toggle") -- play/pause song
        , ((0, 0x1008ff15), spawn "mpc stop")   -- stop song
        , ((0, 0x1008ff16), spawn "mpc prev")   -- previous song
        , ((0, 0x1008ff17), spawn "mpc next")   -- next song
        -- ThinkVantage button
        , ((0, 0x1008ff41), spawn "pavucontrol")
        -- CycleWS
        , ((mod4Mask, xK_quoteleft), toggleSkip ["NSP"])
        -- ScratchPad
        , ((mod4Mask, xK_s), scratchpadSpawnActionCustom "termite --name scratchpad")
        -- full screen
        , ((mod4Mask, xK_F12), sendMessage $ Toggle "Full")
        -- search
        , ((mod4Mask, xK_slash), SM.submap $ searchEngineMap $
                S.promptSearch def)
        , ((mod4Mask .|. shiftMask, xK_slash),
                SM.submap $ searchEngineMap S.selectSearch)
        -- xscreensaver
        , ((mod4Mask .|. shiftMask, xK_l), spawn "slock")
        -- key bindings for resizable tall
        , ((mod4Mask, xK_a), sendMessage MirrorShrink)
        , ((mod4Mask, xK_z), sendMessage MirrorExpand)
        -- shell prompt
        , ((mod4Mask, xK_r), PShell.shellPrompt def)
        -- password
        , ((mod4Mask .|. shiftMask, xK_p), passPrompt def)
        -- toggle xmobar
        , ((mod4Mask, xK_b), sendMessage ToggleStruts)
        -- brighness control
        , ((0, 0x1008FF02), spawn "xbacklight -inc 5")
        , ((0, 0x1008FF03), spawn "xbacklight -dec 5")
        -- volume control
        , ((0, 0x1008ff13), void $ raiseVolume 10)
        , ((0, 0x1008ff11), void $ lowerVolume 10)
        , ((0, 0x1008ffb2), void $ toggleMuteChannels ["Capture"])
        , ((0, 0x1008ff12), void toggleMute)
        -- Grid Select
        , ((mod4Mask, xK_g), goToSelected def)
        -- Screenshot
        , ((0, xK_Print), spawn "flameshot gui")
        ]

myLayout = avoidStruts
         $ toggle
         $ smartBorders
         $ tiled ||| Mirror tiled ||| Full ||| simpleTabbed
    where
        tiled = ResizableTall nmaster delta ratio []
        nmaster = 1
        ratio = 2/3
        delta = 1/100

        toggle = toggleLayouts (noBorders Full)


myManageHook = composeAll
    [ manageDocks
    , scratchpadManageHook (W.RationalRect 0.1 0.25 0.8 0.5)

    , className =? "com-mathworks-util-PostVMInit" --> doFloat <+> doF W.focusDown
    , className =? "Ipython" --> doFloat <+> doF W.focusDown
    , className =? "Octave" --> doF W.focusDown
    , className =? "Pavucontrol" --> doFloat
    , className =? "TeamViewer" --> doFloat
    , className =? "VidyoDesktop" --> doFloat
    , title =? "File Transfers" --> doFloat

    , isDialog --> doFloat
    , isFullscreen --> doFullFloat

    , manageHook def
    ]

myHandleEventHook = composeAll
    [ docksEventHook
    , fullscreenEventHook
    , handleEventHook def
    ]

-- It determines what's being written to the bar.
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ customPP { ppOutput = hPutStrLn h }

customPP :: PP
customPP = xmobarPP
    {   ppCurrent = xmobarColor "yellow" "" . wrap "[" "]",
        ppTitle = xmobarColor "green" "" . shorten 100
    ,   ppSort = (.scratchpadFilterOutWorkspace) <$> ppSort def
    }


-- Toggle any workspace but scratchpad
toggleSkip :: [WorkspaceId] -> X ()
toggleSkip skips = do
    hs <- gets (flip skipTags skips . W.hidden . windowset)
    unless (null hs) (windows . W.view . W.tag $ head hs)

searchEngineMap method = M.fromList
       [ ((0, xK_g), method S.google)
       , ((0, xK_h), method S.hackage)
       , ((0, xK_w), method S.wikipedia)
       , ((0, xK_i), method S.imdb)
       , ((0, xK_s), method S.stackage)
       , ((0, xK_m), method S.maps)
       , ((0, xK_d), method S.deb)
       , ((0, xK_y), method S.youtube)
       , ((0, xK_t), method S.dictionary)
       ]
