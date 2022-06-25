{-# LANGUAGE PartialTypeSignatures #-}

import           Data.Semigroup                 ( Endo )
import           System.Exit
import           System.IO
import           XMonad
import           XMonad.Actions.NoBorders
import           XMonad.Actions.WindowGo
import           XMonad.Config.Desktop
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.Fullscreen
                                         hiding ( fullscreenEventHook )
import           XMonad.Layout.Gaps
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.Named
import           XMonad.Layout.NoBorders
import           XMonad.Layout.OneBig
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spacing
import qualified XMonad.StackSet               as W
import           XMonad.Util.EZConfig           ( additionalKeys
                                                , additionalKeysP
                                                , removeKeysP
                                                )
import           XMonad.Util.Run
import           XMonad.Util.SpawnOnce

main :: IO ()
main = xmonad . docks . ewmhFullscreen . ewmh $ myConfig

myConfig :: XConfig _
myConfig =
  desktopConfig { terminal           = term
                , modMask            = mask
                , borderWidth        = myBorderWidth
                , normalBorderColor  = bg0
                , focusedBorderColor = bg1
                , workspaces         = myWorkspaces
                , focusFollowsMouse  = False
                , manageHook         = myManageHook
                , layoutHook         = myLayoutHook
                , startupHook        = myStartupHook
                , handleEventHook    = handleEventHook desktopConfig
                }
    `additionalKeys`  myAdditionalKeys
    `additionalKeysP` myAdditionalKeysP
    `removeKeysP`     ["M-,", "M-."]

term :: String
term = "alacritty -e tmux"

fg0 :: String
fg0 = "#d8dee9"

fg1 :: String
fg1 = "#4c566a"

bg0 :: String
bg0 = "#2e3440"

bg1 :: String
bg1 = "#88c0d0"

myCurrentWSColor :: String
myCurrentWSColor = "#a3be8c"

myUrgentWSColor :: String
myUrgentWSColor = "#d08770"

myPP :: PP
myPP = xmobarPP { ppUrgent  = xmobarColor myUrgentWSColor ""
                , ppCurrent = xmobarColor myCurrentWSColor ""
                , ppTitle   = xmobarColor fg0 ""
                , ppLayout  = const ""
                , ppSep = "  <icon=/usr/share/icons/stlarch_icons/tile.xbm/>  "
                }

mask :: KeyMask
mask = mod4Mask

myBorderWidth :: Dimension
myBorderWidth = 1

myWorkspaces :: [String]
myWorkspaces = map show ([1 .. 9] :: [Int]) <> map snd myExtraWorkspaces

myExtraWorkspaces :: [(KeySym, String)]
myExtraWorkspaces = [(xK_0, "0")]

screenshotsFolder :: String
screenshotsFolder = "\"~/Pics/screenshots/screen-%Y-%m-%d-%T.png\""

myAdditionalKeysP :: [(String, X ())]
myAdditionalKeysP =
  [ ("M-<Return>", spawn term)
  , ("M-o w", runOrRaise "firefox" (className =? "firefox"))
  , ("M-o b"     , spawn "nautilus")
  , ("M-o t", runOrRaise "telegram-desktop" (className =? "TelegramDesktop"))
  , ("M-e"       , spawn "emacsclient -c -a=''")
  , ("M-o e"     , io exitSuccess)
  , ("M-o f", spawn "emacsclient -c -a='' --eval '(elfeed)'")
  , ( "M-o c l"
    , spawn
      "gsettings set org.gnome.desktop.interface color-scheme 'prefer-light' && sh ~/.config/alacritty/light.sh && sh ~/.config/polybar/light.sh"
    )
  , ( "M-o c d"
    , spawn
      "gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark' && sh ~/.config/alacritty/dark.sh && sh ~/.config/polybar/dark.sh"
    )
  , ("M-d"                   , spawn "rofi -show run")
  , ("M-o j"                 , spawn "rofi -show window")
  , ("M-o p"                 , spawn "pass clip -r")
  , ("<XF86AudioLowerVolume>", spawn "pactl -- set-sink-volume 0 -5%")
  , ("<XF86AudioRaiseVolume>", spawn "pactl -- set-sink-volume 0 +5%")
  , ("<XF86AudioMute>", spawn "pactl set-sink-mute 0 toggle")
  , ("M-o q"                 , io exitSuccess)
  , ("M-o S-s", spawn $ "escrotum -C " <> screenshotsFolder)
  , ("M-o s", spawn $ "escrotum -C -s " <> screenshotsFolder)
  , ("M--"                   , sendMessage Expand)
  , ("M-m"                   , windows W.swapMaster)
  , ("M-S-q"                 , kill)
  , ("M-<Tab>"               , sendMessage NextLayout)
  , ( "M-s"
    , sendMessage (Toggle FULL)
      >> sendMessage ToggleStruts
      >> sendMessage ToggleGaps
    )
  ]

myAdditionalKeys :: [((KeyMask, KeySym), X ())]
myAdditionalKeys =
  [ ((mask, key), windows $ W.greedyView ws) | (key, ws) <- myExtraWorkspaces ]
  <> [ ((mask .|. shiftMask, key), windows $ W.shift ws)
     | (key, ws) <- myExtraWorkspaces
     ]

myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
  [ isFullscreen --> doFullFloat
  , manageDocks
  , return True --> doF W.swapDown
  , manageHook desktopConfig
  , fullscreenManageHook
  , isDialog --> doCenterFloat
  ]

myLayoutHook :: Choose _ _ Window
myLayoutHook = fancy ||| big ||| tiled
 where
  tiled =
    named "Tall"
      . avoidStruts
      . smartBorders
      . smartSpacing spacingSize
      . mkToggle (NOBORDERS ?? FULL ?? EOT)
      $ ResizableTall nmaster delta ratio []
  big =
    named "Console"
      . avoidStruts
      . smartBorders
      . smartSpacing spacingSize
      . mkToggle (NOBORDERS ?? FULL ?? EOT)
      $ OneBig (4 / 5) (4 / 5)
  fancy =
    named "Fancy"
      . avoidStruts
      . gaps [(L, leftGap), (R, rightGap), (U, topGap), (D, bottomGap)]
      . smartSpacing spacingSize
      . mkToggle (NOBORDERS ?? FULL ?? EOT)
      $ ResizableTall nmaster delta ratio []
  leftGap     = 30
  rightGap    = 30
  topGap      = 10
  bottomGap   = 30
  spacingSize = 4
  nmaster     = 1
  delta       = 3 / 100
  ratio       = 1 / 2

startupCommands :: [String]
startupCommands =
  [ "polybar -r &"
  , "~/.fehbg --restore &"
  , "picom --config ~/.picom.conf &"
  , "redshift -l 50.4461248:30.5214979 &"
  , "deadd-notification-center &"
  ]

myStartupHook :: X ()
myStartupHook = mapM_ spawnOnce startupCommands
