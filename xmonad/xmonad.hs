import qualified Data.Map.Strict as M
import           System.IO
import           System.Process

import           XMonad
import           XMonad.Hooks.DynamicBars
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.NoBorders
import qualified XMonad.StackSet             as W
import           XMonad.Util.EZConfig
import           XMonad.Util.Run

main = xmonad . ewmhFullscreen . ewmh . docks $
    myConfig' `additionalKeys` myKeys `additionalMouseBindings` myButtons
    `additionalKeysP` mediaKeys

myConfig' =
    docks $
        def
            { terminal = myTerminal
            , layoutHook = myLayoutHook
            , manageHook = myManageHook
            , handleEventHook = myEventHook
            , logHook = myLogHook
            , startupHook = myStartupHook
            , modMask = myModMask
            , normalBorderColor = (selectColor' "color8")
            , focusedBorderColor = (selectColor' "color3")
            }

myTerminal = "kitty"

myModMask = mod4Mask

myLayoutHook = myLayout

myLayout = (avoidStruts $ tall) ||| (avoidStruts $ Mirror $ tall) |||
  noBorders Full
  where
    tall = (Tall 1 (3/100) (1/2))

myManageHook =
    composeAll
    [ insertPosition Below Newer
    , manageHook def
    , isDialog --> doFloat
    , className =? "Sxiv" --> doFloat
    , className =? "feh" --> doFloat
    , className =? "keepassxc" --> doFloat
    , className =? "Pavucontrol" --> doFloat
    , isFullscreen --> doFullFloat
    ]

myStartupHook = dynStatusBarStartup startBar killBars

myEventHook = handleEventHook def <+>
  dynStatusBarEventHook startBar killBars

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
            { ppCurrent = xmobarColor (selectColor' "color3") "" . wrap "[" "]"
            , ppTitle = const ""
            , ppVisible = const ""
            , ppVisibleNoWindows = Just $ const ""
            , ppHidden = xmobarColor (selectColor' "color7") "" . wrap "<" ">"
            , ppUrgent = xmobarColor (selectColor' "Color17") "" . wrap "[" "]"
            , ppSep = " | "
            }
    unfocusedPP = focusedPP
      { ppCurrent = xmobarColor (selectColor' "color7") "" . wrap "[" "]" }

myKeys =
    [ ((myModMask .|. controlMask, xK_space      ), liftIO switchXkbLayout)
    , ((myModMask                , xK_Print      ), spawn "scrotfeh select")
    , ((myModMask .|. controlMask, xK_p          ), spawn
            "echo 'select\nright\nleft\nfull' | dmenu -p 'screenshot mode' | \
             \ xargs scrotfeh")
    ]

mediaKeys =
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
    (_, Just hout, _, _) <- createProcess (shell processString)
      {std_out = CreatePipe}
    hGetLine hout

type HexColor = String
type ColorName = String
type Colors = M.Map ColorName HexColor
data Palette =
  Colors { getColors :: !Colors
         , getFallBack :: !HexColor }

myPalette :: Palette
myPalette = Colors colors fallback
  where
    fallback = head . M.keys $ colors
    colors = M.fromList [ ("color0" , "#fdf6e3")
                        , ("color1" , "#dc322f")
                        , ("color2" , "#859900")
                        , ("color3" , "#b58900")
                        , ("color4" , "#268bd2")
                        , ("color5" , "#6c71c4")
                        , ("color6" , "#2aa198")
                        , ("color7" , "#586e75")
                        , ("color8" , "#839496")
                        , ("color9" , "#dc322f")
                        , ("color10", "#859900")
                        , ("color11", "#b58900")
                        , ("color12", "#268bd2")
                        , ("color13", "#6c71c4")
                        , ("color14", "#2aa198")
                        , ("color15", "#002b36")
                        , ("color16", "#cb4b16")
                        , ("color17", "#d33682")
                        , ("color18", "#eee8d5")
                        , ("color19", "#93a1a1")
                        , ("color20", "#657b83")
                        , ("color21", "#073642") 
                        ]

selectColor :: Palette -> ColorName -> HexColor
selectColor p cn = case M.lookup cn (getColors p) of
                     Just hex -> hex
                     _ -> getFallBack p

selectColor' :: ColorName -> HexColor
selectColor' = selectColor myPalette
