{-# LANGUAGE PartialTypeSignatures #-}

import Data.Semigroup (Endo)
import GHC.IO.Handle (Handle)
import XMonad
import XMonad.Actions.WindowGo
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Circle
import XMonad.Layout.Fullscreen hiding
  ( fullscreenEventHook,
  )
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.OneBig
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
  ( additionalKeys,
    additionalKeysP,
  )
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad . docks . ewmhFullscreen . ewmh . myConfig $ xmproc

myConfig :: Handle -> XConfig _
myConfig output =
  desktopConfig
    { terminal = term,
      modMask = mask,
      borderWidth = myBorderWidth,
      normalBorderColor = bg0,
      focusedBorderColor = bg1,
      workspaces = myWorkspaces,
      focusFollowsMouse = False,
      manageHook = myManageHook,
      layoutHook = myLayoutHook,
      startupHook = myStartupHook,
      logHook = dynamicLogWithPP (myPP output),
      handleEventHook = handleEventHook desktopConfig
    }
    `additionalKeys` myAdditionalKeys
    `additionalKeysP` myAdditionalKeysP

term :: String
term = "alacritty -e tmux"

fg0 :: String
fg0 = "#d8dee9"

-- fg1 :: String
-- fg1 = "#4c566a"

bg0 :: String
bg0 = "#2e3440"

bg1 :: String
bg1 = "#88c0d0"

myCurrentWSColor :: String
myCurrentWSColor = "#a3be8c"

myUrgentWSColor :: String
myUrgentWSColor = "#d08770"

myPP :: Handle -> PP
myPP xmproc =
  xmobarPP
    { ppUrgent = xmobarColor myUrgentWSColor "",
      ppCurrent = xmobarColor myCurrentWSColor "",
      ppTitle = const "",
      ppOutput = hPutStrLn xmproc,
      ppSep = "  <icon=/usr/share/icons/stlarch_icons/tile.xbm/>  "
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
  [ -- Spawning programs
    ("M-<Return>", spawn term),
    ("M-o w", runOrRaise "firefox" (className =? "firefox")),
    ("M-o S-w", spawn "WEBKIT_FORCE_SANDBOX=0 nyxt"),
    ("M-o b", spawn "thunar"),
    ("M-o t", runOrRaise "telegram-desktop" (className =? "TelegramDesktop")),
    -- emacs
    ("M-e", spawn "emacsclient -c -a=''"),
    ("M-o f", spawn "emacsclient -c -a='' --eval '(elfeed)'"),
    ("M-d", spawn "rofi -show run"),
    ("M-o j", spawn "rofi -show window"),
    ("M-o p", spawn "pass clip -r"), -- -r is for rofi
    -- Audio controls
    ("<XF86AudioLowerVolume>", spawn "pactl -- set-sink-volume 0 -5%"),
    ("<XF86AudioRaiseVolume>", spawn "pactl -- set-sink-volume 0 +5%"),
    ("<XF86AudioMute>", spawn "pactl set-sink-mute 0 toggle"),
    -- Screenshots
    ("M-p M-p", spawn $ "escrotum -C " <> screenshotsFolder),
    ("M-p p", spawn $ "escrotum -C -s " <> screenshotsFolder),
    -- Xmonad messages
    ("M-m", windows W.swapMaster),
    ("M-S-q", kill),
    ("M-<Tab>", sendMessage NextLayout),
    ("M-f", sendMessage (Toggle FULL) >> sendMessage ToggleStruts),
    ("M-S-h", sendMessage MirrorShrink),
    ("M-S-l", sendMessage MirrorExpand)
  ]

myAdditionalKeys :: [((KeyMask, KeySym), X ())]
myAdditionalKeys =
  [((mask, key), windows $ W.greedyView ws) | (key, ws) <- myExtraWorkspaces]
    <> [ ((mask .|. shiftMask, key), windows $ W.shift ws)
         | (key, ws) <- myExtraWorkspaces
       ]

myManageHook :: Query (Endo WindowSet)
myManageHook =
  composeAll
    [ isFullscreen --> doFullFloat,
      manageDocks,
      return True --> doF W.swapDown,
      manageHook desktopConfig,
      fullscreenManageHook,
      isDialog --> doCenterFloat
    ]

myLayoutHook :: Choose _ _ Window
myLayoutHook = tiled ||| big ||| circled
  where
    tiled =
      named "Main"
        . avoidStruts
        . smartBorders
        . smartSpacing gapSize
        -- . spacingWithEdge (gapSize * 2)
        . mkToggle (NOBORDERS ?? FULL ?? EOT)
        $ ResizableTall nmaster delta ratio []
    big =
      named "Console"
        . avoidStruts
        . smartBorders
        . smartSpacing gapSize
        . mkToggle (NOBORDERS ?? FULL ?? EOT)
        $ OneBig (4 / 5) (4 / 5)
    circled =
      named "Fancy" . avoidStruts . withBorder myBorderWidth $
        mkToggle
          (NOBORDERS ?? FULL ?? EOT)
          Circle
    gapSize = 4
    nmaster = 1
    delta = 3 / 100
    ratio = 1 / 2

-- TODO: specify all dependencies
myStartupHook :: X ()
myStartupHook = do
  spawnOnce "~/.fehbg --restore &"
  spawnOnce "picom --config ~/.picom.conf &"
  spawnOnce "redshift -l 50.4461248:30.5214979 &"
  spawnOnce "aw-qt &"
  spawnOnce "wired -r &"
  spawnOnce "emacs --daemon &"
