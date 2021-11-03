Config
  {
    -- appearance
    font = "xft:Iosevka Arjaz-13"
  , borderColor = "#2e3440"
  , border = NoBorder
  , bgColor = "#2e3440"
  , fgColor = "#d8dee9"
  , position = Top

  --general behaviour
  , lowerOnStart = True
  , hideOnStart = False
  , allDesktops = True
  , overrideRedirect = True
  , persistent = True

  , commands =
      [ Run Memory ["-t", "<available>M"] 10
      , Run Com "getMasterVolume" [] "volumelevel" 10
      , Run Date "%d/%m/%Y, %a, %I:%M%P" "date" 10
      , Run
        Volume
        "default"
        "Master"
        [ "--template"
        , "<status> <volume>%"
        , "--"
        , "-l"
        , "<icon=/usr/share/icons/stlarch_icons/vol2.xbm/>"
        , "-m"
        , "<icon=/usr/share/icons/stlarch_icons/vol1.xbm/>"
        , "-h"
        , "<icon=/usr/share/icons/stlarch_icons/vol1.xbm/>"
        , "--on"
        , ""
        , "--off"
        , "<icon=/usr/share/icons/stlarch_icons/vol3.xbm/>"
        , "--onc"
        , "#d8dee9"
        , "--offc"
        , "#d8dee9"
        ]
        1
      , Run DiskU [("/", "<free>")] [] 20
      , Run Kbd [ ("us(dvorak)", "dv") ]
      , Run StdinReader
      ]

  -- layout
  , sepChar = "%"
  , alignSep = "}{"
  , template =
      " %StdinReader% }{ %memory% <> %disku% | %default:Master% | %date% | %kbd% "
  }
