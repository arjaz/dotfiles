Config
  { font = "xft:Roboto Mono-12"
  , borderColor = "#2e3440"
  , border = NoBorder
  , bgColor = "#2e3440"
  , fgColor = "#d8dee9"
  , lowerOnStart = True
  , hideOnStart = False
  , allDesktops = True
  , persistent = True
  , commands =
      [ Run Memory ["-t", "<available>M"] 10
      , Run Com "getMasterVolume" [] "volumelevel" 10
      , Run Date "%d/%m/%Y, %a, %I:%M%P" "date" 10
      , Run Volume "default" "Master" ["--template", "Vol: <volume>%"] 1
      , Run DiskU [("nvme0n1p2", "<free>")] [] 20
      , Run Kbd []
      , Run StdinReader
      ]
  , sepChar = "%"
  , alignSep = "}{"
  , template =
      " %StdinReader% }{ %memory% %disku% | %default:Master% | %date% | %kbd% "
  }
