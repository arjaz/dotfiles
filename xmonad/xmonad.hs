import Graphics.X11.ExtraTypes.XF86
import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Circle
import XMonad.Layout.Named
import XMonad.Layout.OneBig

import XMonad.Layout.Fullscreen hiding (fullscreenEventHook)

-- import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.Spacing
import XMonad.Layout.ZoomRow
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import XMonad.Util.SpawnOnce

import qualified Codec.Binary.UTF8.String as UTF8
import qualified DBus as D
import qualified DBus.Client as D

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ docks $ myConfig xmproc

myConfig output =
  desktopConfig
    { terminal = myTerminal
    , modMask = myModMask
    , borderWidth = myBorderWidth
    , normalBorderColor = bg0
    , focusedBorderColor = bg1
    , workspaces = myWorkspaces
    , focusFollowsMouse = False
    , manageHook = myManageHook
    , layoutHook = myLayoutHook
    , startupHook = myStartupHook <+> ewmhDesktopsStartup
    , logHook = dynamicLogWithPP $ myPP output
    , handleEventHook =
        fullscreenEventHook <+>
        ewmhDesktopsEventHook <+> handleEventHook desktopConfig
    } `additionalKeys`
  myAdditionalKeys

myTerminal = "st"

fg0 = "#d8dee9"

fg1 = "#4c566a"

bg0 = "#2e3440"

bg1 = "#88c0d0"

myCurrentWSColor = "#a3be8c"

myUrgentWSColor = "#d08770"

myPP xmproc =
  xmobarPP
    { ppUrgent = xmobarColor myUrgentWSColor ""
    , ppCurrent = xmobarColor myCurrentWSColor "" . wrap "<" ">"
    , ppTitle = const ""
    , ppOutput = hPutStrLn xmproc
    }

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myModMask = mod4Mask

myBorderWidth = 1

myWorkspaces = map show [1 .. 9] ++ (map snd myExtraWorkspaces)

myExtraWorkspaces = [(xK_0, "0")]

getKeyBinding mod key prog = [((mod, key), spawn prog)]

--TODO: escrotum, networkmanager_dmenu, dmenumount, pass
myAdditionalKeys =
  [((myModMask, xK_d), spawn "dmenu_run")] ++
  [((myModMask, xK_p), spawn "passmenu")] ++
  [((myModMask, xK_n), spawn "networkmanager_dmenu")] ++
  [ ((myModMask, key), (windows $ W.greedyView ws))
  | (key, ws) <- myExtraWorkspaces
  ] ++
  [ ((myModMask .|. shiftMask, key), (windows $ W.shift ws))
  | (key, ws) <- myExtraWorkspaces
  ] ++
  [((myModMask, xK_Tab), sendMessage NextLayout)] ++
  [((myModMask, xK_m), windows W.swapMaster)] ++
  [((myModMask, xK_Return), spawn myTerminal)] ++
  [((myModMask .|. shiftMask, xK_q), kill)] ++
  [((myModMask, xK_f), sendMessage (Toggle FULL) >> sendMessage ToggleStruts)] ++
  [((myModMask .|. shiftMask, xK_w), spawn "emacsclient -c")] ++
  -- [((myModMask, xK_F1), spawn "firefox")] ++
  [((myModMask, xK_F1), spawn "qutebrowser")] ++
  [((myModMask, xK_F2), spawn "telegram-desktop")] ++
  [((0, xF86XK_AudioLowerVolume), spawn "pactl -- set-sink-volume 0 -5%")] ++
  [((0, xF86XK_AudioRaiseVolume), spawn "pactl -- set-sink-volume 0 +5%")] ++
  [((0, xF86XK_AudioMute), spawn "pactl set-sink-mute 0 toggle")] ++
  [((myModMask, xK_Left), spawn "pactl -- set-sink-volume 0 -5%")] ++
  [((myModMask, xK_Right), spawn "pactl -- set-sink-volume 0 +5%")] ++
  [ ( (0, xK_Print)
    , spawn "escrotum \"~/Pics/screenshots/screen-%Y-%m-%d-%T.png\"")
  ] ++
  [ ( (controlMask, xK_Print)
    , spawn "escrotum -s \"~/Pics/screenshots/screen-%Y-%m-%d-%T.png\"")
  ] ++
  [ ( (0, xF86XK_MonBrightnessDown)
    , spawn "true $(pkexec /usr/bin/brillo -U 10)")
  ] ++
  [((0, xF86XK_MonBrightnessUp), spawn "true $(pkexec /usr/bin/brillo -A 10)")] ++
  [((myModMask, xK_Up), spawn "true $(pkexec /usr/bin/brillo -A 10)")] ++
  [((myModMask, xK_Down), spawn "true $(pkexec /usr/bin/brillo -U 10)")] ++
  [ ( (myModMask, xK_Escape)
    , spawn "betterlockscreen -l blur -t 'Eendracht Maakt Magt'")
  ]

myManageHook =
  composeAll
    [ isFullscreen --> doFullFloat
    , manageDocks
    , return True --> doF W.swapDown
    , className =? "TelegramDesktop" --> moveTo "9"
    , manageHook desktopConfig
    , fullscreenManageHook
    , scratchpadManageHookDefault
    ]
  where
    moveTo = doF . W.shift

myLayoutHook =
  avoidStruts . smartBorders . smartSpacing gapSize $
  mkToggle (NOBORDERS ?? FULL ?? EOT) $ tiled ||| big ||| circled
  where
    -- mono = Full
    tiled = Tall nmaster delta ratio
    big = named "One Big" $ OneBig (4 / 5) (4 / 5)
    circled = Circle
    -- eBSP = emptyBSP
    gapSize = 5
    nmaster = 1
    delta = 3 / 100
    ratio = 1 / 2

myStartupHook = do
  spawnOnce
    "setxkbmap -option grp:alt_shift_toggle us,ru,ua -option compose:ralt"
  spawnOnce "~/.fehbg --restore"
  spawnOnce "DRI_PRIME=0 picom --config ~/.picom.conf"
  spawnOnce "redshift-gtk"
  spawnOnce "emacs --daemon"
