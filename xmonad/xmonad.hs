import           System.IO
import           System.Process

import           XMonad
import           XMonad.Hooks.DynamicBars
import           XMonad.Hooks.DynamicLog
import qualified XMonad.Hooks.EwmhDesktops   as X.H.E
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.NoBorders
import qualified XMonad.StackSet             as W
import           XMonad.Util.EZConfig
import           XMonad.Util.Run

main = xmonad . fullscreenSupport . X.H.E.ewmh . docks $
    myConfig' `additionalKeys` myKeys `additionalMouseBindings` myButtons `additionalKeysP` myKeys'

myConfig' =
    docks $
        defaultConfig
            { terminal = myTerminal
            , layoutHook = myLayoutHook
            , manageHook = myManageHook
            , handleEventHook = myEventHook
            , logHook = myLogHook
            , startupHook = myStartupHook
            , modMask = myModMask
            }

myTerminal = "kitty"

myModMask = mod4Mask

myLayoutHook = myLayout

myLayout = (avoidStruts $ tall) ||| (avoidStruts $ Mirror $ tall) ||| noBorders Full
  where
    tall = (Tall 1 (3/100) (1/2))

myManageHook =
    composeAll
    [ insertPosition Below Newer
    , manageHook defaultConfig
    , isDialog --> doFloat
    , className =? "Sxiv" --> doFloat
    , className =? "feh" --> doFloat
    , className =? "keepassxc" --> doFloat
    , className =? "Pavucontrol" --> doFloat
    , isFullscreen --> doFullFloat
    ]

myStartupHook = dynStatusBarStartup startBar killBars

myEventHook = handleEventHook def <+> X.H.E.fullscreenEventHook <+> dynStatusBarEventHook startBar killBars

myLogHook = myMultiPP

startBar :: DynamicStatusBar
startBar (S id) = spawnPipe $ "xmobar -x " ++ show id
killBars :: DynamicStatusBarCleanup -- IO () ()
killBars = return ()

myMultiPP :: X ()
myMultiPP = multiPP focusedPP unfocusedPP
  where
    focusedPP =
        xmobarPP
            { ppCurrent = xmobarColor "black" "" . myWrap
            , ppTitle = const ""
            , ppVisible = const ""
            , ppVisibleNoWindows = Just $ const ""
            , ppHidden = xmobarColor "black" "" . wrap "<" ">"
            , ppUrgent = xmobarColor "red" "" . myWrap
            , ppSep = " | "
            }
    unfocusedPP =
        xmobarPP
            { ppCurrent = xmobarColor "black" "" . myWrap
            , ppTitle = const ""
            , ppVisible = const ""
            , ppVisibleNoWindows = Just $ const ""
            , ppHidden = xmobarColor "black" "" . wrap "<" ">"
            , ppUrgent = xmobarColor "black" "" . myWrap
            , ppSep = " | "
            }

myWrap :: String -> String
myWrap = wrap "[" "]"

myKeys =
    [ ((myModMask .|. controlMask, xK_space      ), liftIO switchXkbLayout)
    , ((myModMask                , xK_bracketleft), spawn "sleep 1.2; scrotfeh")
    , ((myModMask                , xK_Print      ), spawn "scrotfeh select")
    , ((myModMask .|. controlMask, xK_p          ), spawn
            "echo 'select\nright\nleft\nfull' | dmenu -p 'screenshot mode' | xargs scrotfeh")
    ]

myKeys' =
    [ ("<XF86AudioPlay>"        , spawn "playerctl play-pause")
    , ("<XF86AudioPrev>"        , spawn "playerctl previous")
    , ("<XF86AudioNext>"        , spawn "playerctl next")
    , ("<XF86AudioRaiseVolume>" , spawn "amixer -D pulse sset Master 5%+")
    , ("<XF86AudioLowerVolume>" , spawn "amixer -D pulse sset Master 5%-")
    , ("<XF86MonBrightnessUp>"  , spawn "brightnessctl s +10%")
    , ("<XF86MonBrightnessDown>", spawn "brightnessctl s 10%-")
    ]

myButtons =
    [((myModMask .|. shiftMask, button3), \w ->
      focus w >> (withFocused $ windows . W.sink) >> (windows W.swapUp))
    ]

switchXkbLayout :: IO ()
switchXkbLayout = do
    layout <- getXkbLayout
    let args = "-option altwin:swap_lalt_lwin,lv3:ralt_switch_multikey"
    let capsLockCommand = "xmodmap -e 'keycode 105 = Caps_Lock'"
    case layout of
        "us" -> spawn $ "setxkbmap tr " ++ args ++ "; " ++ capsLockCommand
        "tr" -> spawn $ "setxkbmap us " ++ args ++ "; " ++ capsLockCommand

getXkbLayout :: IO String
getXkbLayout = do
    let processString = "setxkbmap -query | grep layout | tail -c 3"
    (_, Just hout, _, _) <- createProcess (shell processString) {std_out = CreatePipe}
    hGetLine hout
