import           XMonad
import           XMonad.Config.Desktop
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.Circle
import           XMonad.Layout.Fullscreen
                                         hiding ( fullscreenEventHook )
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
                                                )
import           XMonad.Util.Run
import           XMonad.Util.SpawnOnce

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad . docks . ewmhFullscreen . ewmh $ myConfig xmproc

myConfig output =
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
                , logHook            = dynamicLogWithPP (myPP output)
                , handleEventHook    = handleEventHook desktopConfig
                }
    `additionalKeys`  myAdditionalKeys
    `additionalKeysP` myAdditionalKeysP

term :: String
term = "alacritty"

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

myPP xmproc = xmobarPP
  { ppUrgent  = xmobarColor myUrgentWSColor ""
  , ppCurrent = xmobarColor myCurrentWSColor ""
  , ppTitle   = const ""
  , ppOutput  = hPutStrLn xmproc
  , ppSep     = "  <icon=/usr/share/icons/stlarch_icons/tile.xbm/>  "
  }

-- toggleStrutsKey XConfig { XMonad.modMask = modMask } = (modMask, xK_o)

mask :: KeyMask
mask = mod4Mask

myBorderWidth :: Dimension
myBorderWidth = 1

myWorkspaces :: [String]
myWorkspaces = map show [1 .. 9] ++ map snd myExtraWorkspaces

myExtraWorkspaces = [(xK_0, "0")]

dmenuOptions :: String
dmenuOptions =
  "-fn \"Iosevka Arjaz-13\" -nb \""
    ++ bg0
    ++ "\" -nf \""
    ++ fg0
    ++ "\" -sb \""
    ++ bg1
    ++ "\" -sf \""
    ++ bg0
    ++ "\""

browser :: String
browser = "brave"

fileBrowser :: String
fileBrowser = "thunar"

screenshotsFolder :: String
screenshotsFolder = "\"~/Pics/screenshots/screen-%Y-%m-%d-%T.png\""

myAdditionalKeysP :: [(String, X ())]
myAdditionalKeysP =
  [ -- Spawning programs
    ("M-<Return>", spawn term)
  , ("M-w"       , spawn browser)
  , ("M-S-w"     , spawn "WEBKIT_FORCE_SANDBOX=0 nyxt")
  , ("M-b"       , spawn fileBrowser)
  , ("M-<F2>"    , spawn "telegram-desktop")
  , ( "M-s d"
    , spawn
      "setxkbmap -option grp:ctrl_alt_toggle dvorak,ru,ua -option compose:ralt -option ctrl:nocaps"
    )
  , ( "M-s q"
    , spawn
      "setxkbmap -option grp:ctrl_alt_toggle us,ru,ua -option compose:ralt -option ctrl:nocaps"
    )
    -- emacs
  , ("M-e"  , spawn "emacsclient -c -a=''")
  , ("M-o c", spawn "emacsclient -c -a='' --eval '(org-roam-capture)'")
  , ("M-o e", spawn "emacsclient -c -a='' --eval '(eshell)'")
  , ("M-o v", spawn "emacsclient -c -a='' --eval '(vterm)'")
  , ("M-o d", spawn "emacsclient -c -a='' --eval '(dired nil)'")
  , ("M-o f", spawn "emacsclient -c -a='' --eval '(elfeed)'")
  , ("M-o g", spawn "emacsclient -c -a='' --eval '(elpher)'")
  , ("M-o t", spawn "emacsclient -c -a='' --eval '(telega)'")
  , ( "M-o i"
    , spawn
      "emacsclient -c -a='' --eval '(find-file \"~/.dotfiles/emacs/init.el\")'"
    )
  ,
    -- dmenu
    ("M-d", spawn $ "dmenu_run -p Run: " ++ dmenuOptions)
  , ("M-p", spawn $ "passmenu -p Pass: " ++ dmenuOptions)
  , ("M-n", spawn $ "networkmanager_dmenu " ++ dmenuOptions)
  ,
    -- Audio controls
    ("<XF86AudioLowerVolume>", spawn "pactl -- set-sink-volume 0 -5%")
  , ("<XF86AudioRaiseVolume>", spawn "pactl -- set-sink-volume 0 +5%")
  , ("<XF86AudioMute>", spawn "pactl set-sink-mute 0 toggle")
  ,
    -- Screenshots
    ("<Print> <Print>", spawn $ "escrotum -C " ++ screenshotsFolder)
  , ("<Print> p", spawn $ "escrotum -C -s " ++ screenshotsFolder)
  ,
    -- Xmonad messages
    ("M-m"                   , windows W.swapMaster)
  , ("M-S-q"                 , kill)
  , ("M-<Tab>"               , sendMessage NextLayout)
  , ("M-f", sendMessage (Toggle FULL) >> sendMessage ToggleStruts)
  , ("M-S-h"                 , sendMessage MirrorShrink)
  , ("M-S-l"                 , sendMessage MirrorExpand)
  ]

myAdditionalKeys :: [((KeyMask, KeySym), X ())]
myAdditionalKeys =
  [ ((mask, key), windows $ W.greedyView ws) | (key, ws) <- myExtraWorkspaces ]
  ++ [ ((mask .|. shiftMask, key), windows $ W.shift ws)
     | (key, ws) <- myExtraWorkspaces
     ]

myManageHook = composeAll
  [ isFullscreen --> doFullFloat
  , manageDocks
  , return True --> doF W.swapDown
  , className =? "TelegramDesktop" --> moveTo "9"
  , className =? "Slack" --> moveTo "9"
  , manageHook desktopConfig
  , fullscreenManageHook
  ]
  where moveTo = doF . W.shift

myLayoutHook = tiled ||| big ||| circled
 where
  tiled =
    named "Main"
      . avoidStruts
      . smartBorders
      . smartSpacing gapSize
      . mkToggle (NOBORDERS ?? FULL ?? EOT)
      $ ResizableTall nmaster delta ratio []
  big =
    named "Console"
      . avoidStruts
      . smartBorders
      . smartSpacing gapSize
      . mkToggle (NOBORDERS ?? FULL ?? EOT)
      $ OneBig (4 / 5) (4 / 5)
  circled = named "Fancy" . avoidStruts . withBorder myBorderWidth $ mkToggle
    (NOBORDERS ?? FULL ?? EOT)
    Circle
  gapSize = 4
  nmaster = 1
  delta   = 3 / 100
  ratio   = 1 / 2

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "~/.fehbg --restore &"
  spawnOnce "picom --config ~/.picom.conf &"
  spawnOnce "redshift -l 50.4461248:30.5214979 &"
  spawnOnce "aw-qt &"
  spawnOnce "emacs --daemon &"
