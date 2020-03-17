import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.Spacing
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.SpawnOnce

main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

--TODO: gaps
myConfig =
  desktopConfig
    { terminal = myTerminal
    , modMask = myModMask
    , borderWidth = myBorderWidth
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , workspaces = myWorkspaces
    , focusFollowsMouse = False
    , manageHook = myManageHook
    , layoutHook = myLayoutHook
    , startupHook = myStartupHook
    } `additionalKeys`
  myAdditionalKeys

myTerminal = "st"

myNormalBorderColor = "#2e3440"

myFocusedBorderColor = "#4c566a"

myBar = "xmobar"

myCurrentWSColor = "#a3be8c"

myUrgentWSColor = "#d08770"

myPP =
  xmobarPP
    { ppUrgent = xmobarColor myUrgentWSColor ""
    , ppCurrent = xmobarColor myCurrentWSColor "" . wrap "<" ">"
    , ppTitle = xmobarColor myCurrentWSColor "" . shorten 0
    }

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myModMask = mod4Mask

myBorderWidth = 2

myWorkspaces =
  ["1", "2", "3", "4", "5", "6", "7", "8", "9"] ++ (map snd myExtraWorkspaces)

myExtraWorkspaces = [(xK_0, "0")]

getKeyBinding mod key prog = [((mod, key), spawn prog)]

--TODO: mute, brightness and volume through fn-keys, escrotum, networkmanager_dmenu, dmenumount
myAdditionalKeys =
  [((myModMask, xK_d), spawn "dmenu_run")] ++
  [ ((myModMask, key), (windows $ W.greedyView ws))
  | (key, ws) <- myExtraWorkspaces
  ] ++
  [ ((myModMask .|. shiftMask, key), (windows $ W.shift ws))
  | (key, ws) <- myExtraWorkspaces
  ] ++
  [((myModMask, xK_m), windows W.swapMaster)] ++
  [((myModMask, xK_Return), spawn myTerminal)] ++
  [((myModMask .|. shiftMask, xK_q), kill)] ++
  [((myModMask .|. shiftMask, xK_w), spawn "emacsclient -c")] ++
  [((myModMask, xK_F1), spawn "firefox")] ++
  [((myModMask, xK_F2), spawn "telegram-desktop")] ++
  [((myModMask, xK_Left), spawn "pactl -- set-sink-volume 0 -5%")] ++
  [((myModMask, xK_Right), spawn "pactl -- set-sink-volume 0 +5%")] ++
  [((myModMask, xK_Up), spawn "true $(pkexec /usr/bin/brillo -A 10)")] ++
  [((myModMask, xK_Down), spawn "true $(pkexec /usr/bin/brillo -U 10)")] ++
  [ ( (myModMask, xK_Escape)
    , spawn "betterlockscreen -l blur -t 'Eendracht Maakt Magt'")
  ]

myManageHook = composeAll [className =? "TelegramDesktop" --> moveTo "9"]
  where
    moveTo = doF . W.shift

myLayoutHook = smartBorders $ gapedTiled ||| eBSP ||| mono
  where
    mono = Full
    gapedTiled = smartSpacing gapSize $ Tall nmaster delta ratio
    eBSP = smartSpacing gapSize $ emptyBSP
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
