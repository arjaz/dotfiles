Config
  { font = "xft:Fira Code-10"
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
      , Run Swap ["-t", "<free>M"] 10
      , Run Com "getMasterVolume" [] "volumelevel" 10
      , Run Date "%d/%m/%Y, %I:%M%P" "date" 10
      , Run Volume "default" "Master" ["--template", "Vol: <volume>%"] 1
      , Run
          CoreTemp
          [ "--template"
          , "<core0>°C"
          , "--Low"
          , "70" -- units: °C
          , "--High"
          , "80" -- units: °C
          , "--low"
          , "#a3be8c"
          , "--normal"
          , "#ebcb8b"
          , "--high"
          , "#bf616a"
          ]
          50
      , Run
          Battery
          [ "--template"
          , "<acstatus>"
          , "--Low"
          , "10" -- units: %
          , "--High"
          , "80" -- units: %
          , "--low"
          , "#bf616a"
          , "--normal"
          , "#ebcb8b"
          , "--high"
          , "#a3be8c"
          , "--" -- battery specific options
                                       -- discharging status
          , "-o"
          , "<left>% (<timeleft>)"
                                       -- AC "on" status
          , "-O"
          , "Charging"
                                       -- charged status
          , "-i"
          , "Charged"
          ]
          50
      , Run DiskU [("sda8", "<free>")] [] 20
      , Run Kbd []
      , Run StdinReader
      ]
  , sepChar = "%"
  , alignSep = "}{"
  , template =
      " %StdinReader% }{ %battery% | %memory% %swap% %disku% | %coretemp% | %default:Master% | %date% | %kbd% "
  }
