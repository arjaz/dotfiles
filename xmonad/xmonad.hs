{-# LANGUAGE PartialTypeSignatures #-}

-- import qualified DBus.Cliest                   as DBusClient
-- import qualified XMonad.DBus                   as DBus
import Control.Monad (liftM2)
import Data.List (isInfixOf)
import Data.Semigroup (Endo)
import System.Exit
import XMonad
import XMonad.Actions.WindowGo
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.RefocusLast (isFloat)
import XMonad.Layout.Gaps
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.OneBig
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (
    additionalKeys,
    additionalKeysP,
    removeKeysP,
 )
import XMonad.Util.SpawnOnce

main :: IO ()
main = xmonad . docks . ewmhFullscreen . ewmh $ myConfig

myConfig :: XConfig _
myConfig =
    def
        { terminal = term
        , modMask = mask
        , borderWidth = myBorderWidth
        , normalBorderColor = normalBorderColor'
        , focusedBorderColor = focusedBorderColor'
        , workspaces = map snd myWorkspaces
        , focusFollowsMouse = False
        , manageHook = myManageHook
        , layoutHook = myLayoutHook
        , startupHook = myStartupHook
        , handleEventHook = handleEventHook desktopConfig
        }
        `additionalKeys` myAdditionalKeys
        `additionalKeysP` myAdditionalKeysP
        `removeKeysP` ["M-,", "M-."]

term :: String
term = "kitty -e nu"

normalBorderColor' :: String
normalBorderColor' = "#202328"

focusedBorderColor' :: String
focusedBorderColor' = "#955f5f"

mask :: KeyMask
mask = mod4Mask

myBorderWidth :: Dimension
myBorderWidth = 1

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

screenshotFilename :: String
screenshotFilename = "\"~/pictures/screenshots/screen-%Y-%m-%d-%T.png\""

myAdditionalKeysP :: [(String, X ())]
myAdditionalKeysP =
    [ ("M-h", windows W.focusDown)
    , ("M-i", windows W.focusUp)
    , ("S-M-n", windows W.swapDown)
    , ("S-M-p", windows W.swapUp)
    , ("M-<Return>", spawn term)
    , ("M-o w", runOrRaise "firefox" (className =? "firefox"))
    , ("M-o t", runOrRaise "telegram-desktop" (className =? "TelegramDesktop"))
    , ("M-o l", runOrRaise "slack" (className =? "Slack"))
    , ("M-o i", runOrRaise "discord" (className =? "Discord"))
    , ("M-o h", runOrRaise "thunderbird" (className =? "Thunderbird"))
    , ("M-o m", runOrRaise "mpdevil" (className =? "Mpdevil"))
    , ("M-o e", runOrRaise "emacsclient -c -a=''" (className =? "Emacs"))
    , ("M-e", spawn "emacsclient -c -a=''")
    , ("M-o q q", io exitSuccess)
    , ("M-o c c", spawn "eww close calendar || eww open calendar")
    , ("M-o c k", spawn "setxkbmap -option grp:sclk_toggle us,ua -option compose:ralt")
    , ("M-o c l", spawn "sh ~/dotfiles/scripts/to-light-theme.sh")
    , ("M-o c d", spawn "sh ~/dotfiles/scripts/to-dark-theme.sh")
    , ("M-o d y", spawn "sh ~/dotfiles/scripts/xclip-yt-dlp.sh")
    , ("M-d", spawn "rofi -show run")
    , ("M-o p", spawn "pass clip -r")
    , ("<XF86AudioPause>", spawn "playerctl pause")
    , ("<XF86AudioPlay>", spawn "playerctl play")
    , ("<XF86AudioLowerVolume>", spawn "pactl -- set-sink-volume 0 -5%")
    , ("<XF86AudioRaiseVolume>", spawn "pactl -- set-sink-volume 0 +5%")
    , ("<XF86AudioMute>", spawn "pactl set-sink-mute 0 toggle")
    , ("M-o S-s", spawn $ "maim -i $(xdotool getactivewindow) | xclip -selection clipboard -t image/png")
    , ("M-o s", spawn $ "maim -s | xclip -selection clipboard -t image/png")
    , ("M-o M-S-s", spawn $ "maim " <> screenshotFilename)
    , ("M-o M-s", spawn $ "maim -s " <> screenshotFilename)
    , ("S-M-h", sendMessage Expand)
    , ("S-M-i", sendMessage Shrink)
    , ("M-m", windows W.swapMaster)
    , ("M-x", kill)
    , ("M-b", spawn "eww close bar || eww open bar")
    ,
        ( "M-s"
        , sendMessage (Toggle FULL)
            >> sendMessage ToggleStruts
            >> sendMessage ToggleGaps
        )
    ]

myAdditionalKeys :: [((KeyMask, KeySym), X ())]
myAdditionalKeys =
    [((mask, key), windows $ W.greedyView ws) | (key, ws) <- myWorkspaces]
        <> [((mask .|. shiftMask, key), windows $ W.shift ws) | (key, ws) <- myWorkspaces]

myManageHook :: Query (Endo WindowSet)
myManageHook =
    composeAll
        [ isFullscreen --> doFullFloat
        , className =? "firefox" --> viewShift "1"
        , className =? "Slack" --> viewShift "7"
        , className =? "Mpdevil" --> viewShift "8"
        , className =? "Mpdevil" --> doRectFloat (W.RationalRect 0.02 0.1 0.7 0.8)
        , title =? "Telegram" --> viewShift "9"
        , title =? "Telegram" --> doRectFloat (W.RationalRect 0.05 0.1 0.27 0.8)
        , isDialog --> doCenterFloat
        , manageDocks
        , manageHook desktopConfig
        , fmap not isFloat >> fmap not (className =? "TelegramDesktop") --> doF W.swapDown
        , -- TODO: make that work
          fmap (isInfixOf "_NET_WM_STATE_BELOW") (stringProperty "_NET_WM_WINDOW_STATE") --> doF W.swapUp
        ]
  where
    viewShift = doF . liftM2 (.) W.greedyView W.shift

myLayoutHook :: Choose _ _ Window
myLayoutHook =
    -- layoutHints
    fancy ||| console
  where
    console =
        avoidStruts
            . smartBorders
            . smartSpacing spacingSize
            . mkToggle (NOBORDERS ?? FULL ?? EOT)
            $ OneBig (4 / 5) (4 / 5)
    fancy =
        avoidStruts
            . gaps [(L, leftGap), (R, rightGap), (U, topGap), (D, bottomGap)]
            . smartBorders -- TODO: use lessBorders
            . smartSpacing spacingSize
            . mkToggle (NOBORDERS ?? FULL ?? EOT)
            $ ResizableTall nmaster delta ratio []
    leftGap = 30
    rightGap = 30
    topGap = 10
    bottomGap = 30
    spacingSize = 4
    nmaster = 1
    delta = 3 / 100
    ratio = 1 / 2

startupCommands :: [String]
startupCommands =
    [ "eww open bar"
    , "redshift -l 50.4461248:30.5214979 -t 6500:3000 &"
    , "wired &"
    , "picom --config ~/.config/compton.conf &"
    , -- TODO: why do I have to do that?
      --       systemd's --user service doesn't work for some reason
      "/usr/lib/xdg-desktop-portal --replace &"
    , "~/dotfiles/scripts/to-light-theme.sh &"
    , "~/screenlayout/1.sh &"
    ]

myStartupHook :: X ()
myStartupHook = mapM_ spawnOnce startupCommands
