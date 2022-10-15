{-# LANGUAGE PartialTypeSignatures #-}

-- import qualified DBus.Cliest                   as DBusClient
-- import qualified XMonad.DBus                   as DBus
import           Data.Semigroup                 ( Endo )
import           System.Exit
import           XMonad
import           XMonad.Actions.WindowGo
import           XMonad.Config.Desktop
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
import           XMonad.Util.SpawnOnce

main :: IO ()
main = xmonad . docks . ewmhFullscreen . ewmh $ myConfig

myConfig :: XConfig _
myConfig =
  def { terminal           = term
      , modMask            = mask
      , borderWidth        = myBorderWidth
      , normalBorderColor  = normalBorderColor'
      , focusedBorderColor = focusedBorderColor'
      , workspaces         = map snd myWorkspaces
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

-- TODO: Depend on the system theme
normalBorderColor' :: String
normalBorderColor' = "#202328"

-- TODO: Depend on the system theme
focusedBorderColor' :: String
focusedBorderColor' = "#955f5f"

mask :: KeyMask
mask = mod4Mask

myBorderWidth :: Dimension
myBorderWidth = 2

myWorkspaces :: [(KeySym, String)]
myWorkspaces =
  [ (xK_1, "1")
  , (xK_2, "2")
  , (xK_3, "3")
  , (xK_4, "4")
  , (xK_5, "5")
  , (xK_6, "6")
  , (xK_7, "7")
  , (xK_8, "8")
  , (xK_9, "9")
  , (xK_0, "0")
  ]

screenshotsFolder :: String
screenshotsFolder = "\"~/Pics/screenshots/screen-%Y-%m-%d-%T.png\""

myAdditionalKeysP :: [(String, X ())]
myAdditionalKeysP =
  [ ("M-<Return>", spawn term)
  , ("M-o w", runOrRaise "firefox" (className =? "firefox"))
  , ("M-o b"     , spawn "nautilus")
  , ("M-o t", runOrRaise "telegram-desktop" (className =? "TelegramDesktop"))
  , ("M-e"       , spawn "emacsclient -c -a=''")
  , ("M-o e e"   , io exitSuccess)
  , ( "M-o c l"
    , spawn
      -- TODO: fix that ugliness
      "gsettings set org.gnome.desktop.interface color-scheme 'prefer-light' && sh ~/.config/alacritty/light.sh && sh ~/.config/polybar/light.sh"
    )
  , ( "M-o c d"
    , spawn
      -- TODO: fix that ugliness
      "gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark' && sh ~/.config/alacritty/dark.sh && sh ~/.config/polybar/dark.sh"
    )
  , ("M-d"                   , spawn "rofi -show run")
  , ("M-o j"                 , spawn "rofi -show window")
  , ("M-o p"                 , spawn "sh -c 'pass clip -r'")
  , ("<XF86AudioLowerVolume>", spawn "pactl -- set-sink-volume 0 -5%")
  , ("<XF86AudioRaiseVolume>", spawn "pactl -- set-sink-volume 0 +5%")
  , ("<XF86AudioMute>", spawn "pactl set-sink-mute 0 toggle")
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
  [ ((mask, key), windows $ W.greedyView ws) | (key, ws) <- myWorkspaces ]
  <> [ ((mask .|. controlMask, key), windows $ W.shift ws)
     | (key, ws) <- myWorkspaces
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
myLayoutHook = fancy ||| tiled ||| console
 where
  tiled =
    named "Tall"
      . avoidStruts
      . smartBorders
      . smartSpacing spacingSize
      . mkToggle (NOBORDERS ?? FULL ?? EOT)
      $ ResizableTall nmaster delta ratio []
  console =
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
  [ "polybar -r &" -- TODO: use eww instead
  -- , "eww open bar &"
  , "~/.fehbg --restore &"
  , "picom --config ~/.picom.conf &"
  , "redshift -l 50.4461248:30.5214979 &"
  , "deadd-notification-center &"
  ]

-- TODO: action to bring a window to the current workspace
-- https://hackage.haskell.org/package/xmonad-contrib-0.17.1/docs/XMonad-Actions-WindowBringer.html
myStartupHook :: X ()
myStartupHook = mapM_ spawnOnce startupCommands

myLogHook :: X ()
myLogHook = pure ()
