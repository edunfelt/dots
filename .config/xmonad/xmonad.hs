----------------------------------------------------------------------------------------------
--
-- .--.--.--------.-----.-----.---.-.--|  |
-- |_   _|        |  _  |     |  _  |  _  |
-- |__.__|__|__|__|_____|__|__|___._|_____|
--
-- Emilia's xmonad config
-- Edited: 2021-04-16
-- Author: Emilia Dunfelt, edun@dunfelt.se
--
-- Structure:
-- 1. Modules
-- 1.1 Basics
-- 1.2 Hooks
-- 1.3 Layout
-- 1.4 Actions
-- 1.5 Utilities
-- 1.6 Data
-- 2. Variables
-- 2.1 Colors
-- 2.2 Theme
-- 2.3 Xmobar
-- 2.4 PP settings
-- 3. Workspaces
-- 3.1 Workspaces
-- 3.2 Projects
-- 3.3 Prompt
-- 4. Layouts
-- 4.1 Startup
-- 4.2 Layouts
-- 5. Utilities
-- 5.1 Managehook
-- 5.2 Scratchpads
-- 6. Keybindings
-- 6.1 General
-- 6.2 Navigation
-- 6.3 Keypad navigation
-- 6.4 Layout
-- 6.5 Resizing
-- 6.6 Tab-management
-- 6.7 Media keys
-- 6.8 Productivity
-- 7. Main
----------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------
-- 1. Modules
----------------------------------------------------------------------------------------------

-- 1.1 Basics --------------------------------------------------------------------------------
import XMonad                                            -- core libraries
import qualified XMonad.StackSet as W                    -- window stack manipulation
import XMonad.Prompt                                     -- graphical prompts
import System.IO                                         -- for xmobar

-- 1.2 Hooks ---------------------------------------------------------------------------------
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks                          -- don't cover the bar with windows
import XMonad.Hooks.SetWMName                            -- needed for JetBrains IDEs
import XMonad.ManageHook

-- 1.3 Layout --------------------------------------------------------------------------------
import XMonad.Layout.WindowNavigation                    -- window navigation
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile                       -- change window width/height
import XMonad.Layout.LimitWindows                        -- increase/decrease number of windows that can be seen
import XMonad.Layout.Tabbed                              -- tabbed layout
import XMonad.Layout.SubLayouts
import XMonad.Layout.Simplest
import XMonad.Layout.MultiToggle as MT
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.Renamed

-- 1.4 Actions -------------------------------------------------------------------------------
import XMonad.Actions.Promote                            -- to move focued window to master
import XMonad.Actions.CycleWS                            -- cycle through workspaces
import XMonad.Actions.WithAll                            -- killAll
import XMonad.Actions.SpawnOn
import XMonad.Actions.DynamicProjects

-- 1.5 Utilities -----------------------------------------------------------------------------
import XMonad.Util.Run                                   -- spawnPipe and hPutStrLn
import XMonad.Util.EZConfig (additionalKeysP)            -- Emacs-style keybindings
import XMonad.Util.NamedScratchpad

-- 1.6 Data ----------------------------------------------------------------------------------
import qualified Data.Map as M
import Data.Monoid


----------------------------------------------------------------------------------------------
-- 2. Variables
----------------------------------------------------------------------------------------------

myTerminal :: [Char]
myTerminal                  = "kitty"

myModMask :: KeyMask
myModMask                   = mod4Mask

-- 2.1 Colors --------------------------------------------------------------------------------
color0                      = "#fbf1f2"
color1                      = "#d57e85"
color2                      = "#a3b367"
color3                      = "#dcb16c"
color4                      = "#7297b9"
color5                      = "#bb99b4"
color6                      = "#69a9a7"
color7                      = "#8b8198"
color8                      = "#bfb9c6"
color9                      = "#ebb790"
color10                     = "#f2f1f4"
color11                     = "#d8d5dd"
color12                     = "#a59daf"
color13                     = "#72677e"
color14                     = "#baa58c"
color15                     = "#585062"

-- 2.2 Theme ----------------------------------------------------------------------------------
myFont :: [Char]
myFont                      = "xft:Anonymice Nerd Font:pixelsize=14:antialias=true:hinting=true,FontAwesome:pixelsize=14"

myBorderWidth :: Dimension
myBorderWidth               = 3

myFocusedBorderColor :: [Char]
myFocusedBorderColor        = color5

myNormalBorderColor :: [Char]
myNormalBorderColor         = color11

-- 2.3 Xmobar ---------------------------------------------------------------------------------
myLogHook :: Handle -> X ()
myLogHook h                 = dynamicLogWithPP $ wsPP { ppOutput = hPutStrLn h }

myWsBar :: [Char]
myWsBar        = "xmobar ~/.config/xmonad/xmobarrc"

-- 2.4 PP settings ---------------------------------------------------------------------------------
wsPP :: PP
wsPP           = xmobarPP { ppOrder               = id
                          , ppTitle               = xmobarColor   color5 "" . shorten 50
                          , ppCurrent             = xmobarColor   color4 "" . wrap "{ " " }"
                          , ppUrgent              = xmobarColor   color1 ""
                          , ppVisible             = xmobarColor   color8 "" 
                          , ppHidden              = xmobarColor   color8 "" 
                          , ppHiddenNoWindows     = const ""
                          , ppSep                 = "  :  "
                          , ppWsSep               = "    "
                          , ppSort                = fmap
                                  (namedScratchpadFilterOutWorkspace.)
                                  (ppSort def)
                          }


-----------------------------------------------------------------------------------------------
-- 3. Workspaces
-----------------------------------------------------------------------------------------------

-- 3.1 Workspaces -----------------------------------------------------------------------------
myWorkspaces :: [WorkspaceId]
myWorkspaces = [ "the"          -- thesis
               , "num"          -- TA
               , "www"          -- browsing
               , "res"          -- research
               , "com"          -- communication
               , "mul"          -- multimedia, ok it's basically just music
               , "prg"          -- coding general project
               , "msc"          -- misc. (but useful)
               , "tmp"          -- temporary stuff
               ]

-- 3.2 Projects -------------------------------------------------------------------------------
myProjects :: [Project]
myProjects = [Project 
                { projectName = myWorkspaces !! 0
                , projectDirectory = "/media/nas/home/current/thesis2021-emilia"
                , projectStartHook = Just $ do
                    spawnOn (myWorkspaces !! 0) "zotero"
                    spawnOn (myWorkspaces !! 0) "kitty -e vim"
                    spawnOn (myWorkspaces !! 0) "qutebrowser ':session-load thesis' --nowindow"
                }
            , Project
                { projectName = myWorkspaces !! 1
                , projectDirectory = "/media/nas/home/current/TA/mm5016_numerical_analysis"
                , projectStartHook = Just $ do
                    spawnOn (myWorkspaces !! 1) "qutebrowser ':session-load TA' --nowindow"
                    spawnOn (myWorkspaces !! 1) "kitty -e ranger /media/nas/home/current/TA"
                    spawnOn (myWorkspaces !! 1) "zotero"
                }
             , Project
                { projectName = myWorkspaces !! 2
                , projectDirectory = "$HOME"
                , projectStartHook = Just $ do
                    spawnOn (myWorkspaces !! 2) "qutebrowser ':session-load work --nowindow"
                }
             , Project
                 { projectName = myWorkspaces !! 3
                 , projectDirectory = "/media/nas/home/current"
                 , projectStartHook = Just $ do
                     spawnOn (myWorkspaces !! 3) "zotero"
                     spawnOn (myWorkspaces !! 3) "qutebrowser ':session-load thesis --nowindow"
                     spawnOn (myWorkspaces !! 3) "kitty -e ranger /media/nas/home/current"
                 }
             , Project
                 { projectName = myWorkspaces !! 4
                 , projectDirectory = "/media/nas/home/current"
                 , projectStartHook = Just $ do
                     spawnOn (myWorkspaces !! 4) "discord"
                     spawnOn (myWorkspaces !! 4) "thunderbird"
                     spawnOn (myWorkspaces !! 4) "telegram-desktop"
                 }
             , Project
                 { projectName = myWorkspaces !! 5
                 , projectDirectory = "$HOME"
                 , projectStartHook = Just $ do
                     spawnOn (myWorkspaces !! 5) "spotify"
                 }
             , Project
                 { projectName = myWorkspaces !! 6
                 , projectDirectory = "/media/nas/home/current"
                 , projectStartHook = Just $ do
                     spawnOn (myWorkspaces !! 6) "code"
                     spawnOn (myWorkspaces !! 6) "cantor"
                     spawnOn (myWorkspaces !! 6) myTerminal
                 }
             , Project
                 { projectName = myWorkspaces !! 7
                 , projectDirectory = "$HOME"
                 , projectStartHook = Just $ do
                     spawnOn (myWorkspaces !! 7) "emacs"
                 }
             , Project
                 { projectName = myWorkspaces !! 8
                 , projectDirectory = "/media/nas/home"
                 , projectStartHook = Nothing
                 }
             ]
                 
-- 3.3 Prompt ---------------------------------------------------------------------------------
myPromptTheme :: XPConfig
myPromptTheme = def
    { font = myFont
    , bgColor = color7
    , fgColor = color0
    , fgHLight = color0
    , bgHLight = color4
    , borderColor = color14
    , promptBorderWidth = 0
    , height = 20
    , position = Bottom
    }

-----------------------------------------------------------------------------------------------
-- 4. Layouts
-----------------------------------------------------------------------------------------------

-- 4.1 Startup --------------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
    setWMName "LG3D"
    -- WS 1: Work
    -- spawnOn " \xf120 " myTerminal
    -- spawnOn " \xf120 " myTerminal
    -- WS 2: Browse
    -- spawnOn " \xf269 " (myTerminal ++ " -e  ranger")
    -- spawnOn " \xf269 " "firefox"
    -- WS 3: Music etc.
    -- spawnOn " \xf001 " (myTerminal ++ " -e calcurse")
    -- spawnOn " \xf001 " (myTerminal ++ " -e cava")
    -- spawnOn " \xf001 " "spotify"
    -- WS 4: Chat & Email
    -- spawnOn " \xf0e6 " (myTerminal ++ " -e weechat")
    -- spawnOn " \xf0e6 " "telegram-desktop"
    -- spawnOn " \xf0e6 " "thunderbird"
    -- WS 5: Resources
    -- spawnOn " \xf085 " "milanote"

-- 4.2 Layouts --------------------------------------------------------------------------------
tall = renamed [Replace "tall"]
     $ ResizableTall 1 (3/100) (6/10) []

threecol = renamed [Replace "threecol"]
         $ ThreeCol 1 (3/100) (1/2)

bsp = renamed [Replace "bsp"]
    $ emptyBSP

-- Tabbed layout theme
myTabConfig :: Theme
myTabConfig = def { inactiveColor = "#d8d5dd"
                    , inactiveBorderColor = "#bfb9c6"
                    , inactiveTextColor = "#a59daf"
                    , activeColor = "#bb99b4"
                    , activeBorderColor = "#bb99b4"
                    , activeTextColor = "#fbf1f2"
                    , fontName = "xft:Anonymice Nerd Font:pixelsize=12:antialias=true:hinting=true,FontAwesome:pixelsize=14"
                    }
    
myDefaultLayout = tall
              ||| threecol
              ||| bsp

myLayoutHook = avoidStruts 
             $ configurableNavigation noNavigateBorders 
             $ addTabs shrinkText myTabConfig 
             $ subLayout [0] (Simplest) 
             $ spacingRaw True (Border 5 5 5 5) True (Border 5 5 5 5) True
             $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
             myDefaultLayout


-----------------------------------------------------------------------------------------------
-- 5. Utilities
-----------------------------------------------------------------------------------------------

-- 5.1 Managehook -----------------------------------------------------------------------------
myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
    [ className =? "Anki"               --> doFloat
    , className =? "gimp"               --> doFloat
    , className =? "VirtualBox Manager" --> doFloat
    , className =? "VirtualBox Machine" --> doFloat
    , className =? "Matplotlib"         --> doFloat
    , className =? "TelegramDesktop"    --> doFloat
    ]
    <+> manageDocks 
    <+> manageSpawn 
    <+> namedScratchpadManageHook myScratchPads

-- 5.2 Scratchpads ----------------------------------------------------------------------------
myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "calendar" spawnCal findCal manageCal
                , NS "diary" spawnDiary findDiary manageDiary
                ]
  where
    spawnCal  = "kitty -e calcurse"
    findCal   = resource =? "calcurse"
    manageCal = customFloating $ W.RationalRect (1/6) (1/6) (1/4) (1/4)

    spawnDiary  = "kitty -e vim -c VimwikiMakeDiaryNote"
    findDiary   = title =? "diary"
    manageDiary = customFloating $ W.RationalRect (1/6) (1/6) (1/4) (1/4)


----------------------------------------------------------------------------------------------
-- 6. Keybindings
----------------------------------------------------------------------------------------------
myKeys :: [([Char], X ())]
myKeys =
-- 6.1 General -------------------------------------------------------------------------------
    [ ("M-<Return>", spawn myTerminal)                                          -- open a terminal
    , ("M-b", spawn "qutebrowser")                                              -- start firefox
    , ("M-<Esc>", spawn "xmonad --restart")                                     -- restart xmonad
    , ("M-S-<Esc>", spawn "xmonad --recompile")                                 -- recompile xmonad
    , ("M-p", spawn "dmenu_run -nf '#FBF1F2' -nb '#8B8198' -sb '#BB99B4' -sf '#585062'")
    
-- 6.2 Navigation ----------------------------------------------------------------------------
    , ("M-j", windows W.focusDown)                                              -- move focus up
    , ("M-k", windows W.focusUp)                                                -- move focus down
    , ("M-S-j", windows W.swapDown)                                             -- swap focused with next
    , ("M-S-k", windows W.swapUp)                                               -- swap focused with previous
    , ("M-<Backspace>", promote)                                                -- promote focused to master
    , ("M-m", windows W.focusMaster)                                            -- focus master
    , ("M-q", kill)                                                             -- kill focused
    , ("M-S-q", killAll)                                                        -- kill workspace
    , ("M-<Tab>", moveTo Next NonEmptyWS)                                       -- move to next workspace
    , ("M-S-<Tab>", moveTo Prev NonEmptyWS)                                     -- move to previous workspace
    , ("M-0", moveTo Next EmptyWS)                                              -- move focused to next empty workspace
    , ("M-w", switchProjectPrompt myPromptTheme)                                -- switch project
    , ("M-S-w", shiftToProjectPrompt myPromptTheme)                             -- move window to project

-- 6.3 Keypad navigation --------------------------------------------------------------------
    , ("M-<KP_End>", windows $ W.greedyView " \xf120 ")
    , ("M-S-<KP_End>", windows $ W.shift " \xf120 ")
    , ("M-<KP_Down>", windows $ W.greedyView " \xf269 ")
    , ("M-S-<KP_Down>", windows $ W.shift " \xf269 ")
    , ("M-<KP_Page_Down>", windows $ W.greedyView " \xf001 ")
    , ("M-S-<KP_Page_Down>", windows $ W.shift " \xf001 ")
    , ("M-<KP_Left>", windows $ W.greedyView " \xf0e6 ")
    , ("M-S-<KP_Left>", windows $ W.shift " \xf0e6 ")
    , ("M-<KP_Begin>", windows $ W.greedyView " \xf085 ")
    , ("M-S-<KP_Begin>", windows $ W.shift " \xf085 ")
    , ("M-<KP_Right>", windows $ W.greedyView " \xf19c ")
    , ("M-S-<KP_Right>", windows $ W.shift " \xf19c ")
    , ("M-<Page_Up>", prevWS)
    , ("M-<Page_Down>", nextWS)

-- 6.4 Layout --------------------------------------------------------------------------------
    , ("M-<Space>", sendMessage NextLayout)                                     -- next layout
    , ("M-S-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)   -- fullscreen view

-- 6.5 Resizing ------------------------------------------------------------------------------
    , ("M-h", sendMessage Shrink)                                               -- shrink horizontally
    , ("M-l", sendMessage Expand)                                               -- expand horizontally
    , ("M-n", sendMessage MirrorShrink)                                         -- shrink vertically
    , ("M-u", sendMessage MirrorExpand)                                         -- expand vertically
    , ("M-<KP_Add>", sendMessage (IncMasterN 1))                                -- increase master "size"
    , ("M-<KP_Subtract>", sendMessage (IncMasterN (-1)))                        -- decrease master "size"
    , ("M-S-<KP_Add>", increaseLimit)                                           -- increase workspace limit
    , ("M-S-<KP_Subtract>", decreaseLimit)                                      -- decrease workspace limit
    , ("M-<Left>",    sendMessage $ ExpandTowards L)
    , ("M-<Right>",   sendMessage $ ExpandTowards R)
    , ("M-<Up>",      sendMessage $ ExpandTowards U)
    , ("M-<Down>",    sendMessage $ ExpandTowards D)

-- 6.6 Tab-management ------------------------------------------------------------------------
    , ("M-C-h", sendMessage $ pullGroup L)                                      -- merge with left
    , ("M-C-j", sendMessage $ pullGroup D)                                      -- merge with below
    , ("M-C-k", sendMessage $ pullGroup U)                                      -- merge with above
    , ("M-C-l", sendMessage $ pullGroup R)                                      -- merge with right
    , ("M-C-S-j", onGroup W.focusDown')                                         -- cycle through tabs
    , ("M-C-S-k", onGroup W.focusUp')                                           -- cycle through tabs
    , ("M-C-m", withFocused (sendMessage . MergeAll))                           -- merge all windows on ws into tabbed
    , ("M-C-u", withFocused (sendMessage . UnMerge))                            -- unmerge tabbed

-- 6.7 Media keys -----------------------------------------------------------------------------
    , ("M-<XF86AudioMute>", spawn "playerctl play-pause")                       -- play/pause (no external kb)
    , ("<XF86AudioPlay", spawn "playerctl play-pause")                          -- play/pause                        
    , ("M-<XF86AudioRaiseVolume>", spawn "playerctl next")                      -- next track (no external kb)
    , ("<XF86AudioNext>", spawn "playerctl next")                               -- next track
    , ("M-<XF86AudioLowerVolume>", spawn "playerctl previous")                  -- prev track (no external kb)
    , ("<XF86AudioPrev>", spawn "playerctl previous")                           -- prev track
    , ("<XF86AudioMute>", spawn "amixer -D pulse set Master toggle")            -- mute
    , ("<XF86AudioLowerVolume>",   spawn "amixer set Master 5%- unmute")        -- raise volume by 5%
    , ("<XF86AudioRaiseVolume>",   spawn "amixer set Master 5%+ unmute")        -- lower volume by 5%
    , ("<xf86MonBrightnessDown>", spawn "brightnessctl s 10%-")                 -- decrease brightness by 10%
    , ("<XF86MonBrightnessUp>", spawn "brightnessctl s +10%")                   -- increase brightness by 10%
    , ("<Print>", spawn "flameshot full -p ~/Pictures/scrots -c")             -- full screenshot
    , ("M-<Print>", spawn "flameshot gui ~/Pictures/scrots")                    -- interactive screenshot

-- 6.8 Productivity ---------------------------------------------------------------------------
    , ("M-C-c", namedScratchpadAction myScratchPads "calendar")                 -- open calcurse (unless already open)
    , ("M-C-d", namedScratchpadAction myScratchPads "diary")                    -- open today's diary
    ]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm,button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    -- mod-button2, Raise the window to the top of the stack
    , ((modm,button2), (\w -> focus w >> windows W.shiftMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm,button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]


-----------------------------------------------------------------------------------------------
-- 7. Main
-----------------------------------------------------------------------------------------------

main = do
    xmproc <- spawnPipe myWsBar
    xmonad 
        $ docks 
        $ dynamicProjects myProjects 
        $ myConfig xmproc
myConfig p = def
    { borderWidth               = myBorderWidth
    , startupHook               = myStartupHook
    , terminal                  = myTerminal
    , modMask                   = myModMask
    , normalBorderColor         = myNormalBorderColor
    , focusedBorderColor        = myFocusedBorderColor
    , workspaces                = myWorkspaces
    , mouseBindings             = myMouseBindings
    , layoutHook                = myLayoutHook
    , logHook                   = myLogHook p
    , manageHook                = myManageHook
    } `additionalKeysP` myKeys
