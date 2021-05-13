----------------------------------------------------------------------------------------------
--
-- .--.--.--------.-----.-----.---.-.--|  |
-- |_   _|        |  _  |     |  _  |  _  |
-- |__.__|__|__|__|_____|__|__|___._|_____|
--
-- Emilia's xmonad config
-- Edited: 2021-05-11
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
-- 4.3 Float layouts
-- 5. Utilities
-- 5.1 Managehook
-- 5.2 Scratchpads
-- 5.3 Float cycling
-- 5.4 Grid select
-- 5.5 Tree select
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
import XMonad.Hooks.ManageHelpers

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
import XMonad.Layout.PerWorkspace
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Minimize

-- 1.4 Actions -------------------------------------------------------------------------------
import XMonad.Actions.Promote                            -- to move focued window to master
import XMonad.Actions.CycleWS as CWS                           -- cycle through workspaces
import XMonad.Actions.WithAll                            -- killAll
import XMonad.Actions.SpawnOn
import XMonad.Actions.DynamicProjects
import XMonad.Actions.GridSelect                         -- experiment with GridSelect
import XMonad.Actions.TreeSelect as TS
import XMonad.Actions.CopyWindow
import XMonad.Actions.Minimize

-- 1.5 Utilities -----------------------------------------------------------------------------
import XMonad.Util.Run                                   -- spawnPipe and hPutStrLn
import XMonad.Util.EZConfig (additionalKeysP)            -- Emacs-style keybindings
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare

-- 1.6 Data ----------------------------------------------------------------------------------
import qualified Data.Map as M
import Data.Monoid
import Data.List
import Data.Tree


----------------------------------------------------------------------------------------------
-- 2. Variables
----------------------------------------------------------------------------------------------

myTerminal :: [Char]
myTerminal                  = "alacritty"

myEditor :: [Char]
myEditor                    = myTerminal ++ " -e vim "

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
myFont                      = "xft:scientifica:pixelsize=16:antialias=true:hinting=true,FontAwesome:pixelsize=14"

myBorderWidth :: Dimension
myBorderWidth               = 3

myFocusedBorderColor :: [Char]
myFocusedBorderColor        = color5

myNormalBorderColor :: [Char]
myNormalBorderColor         = color11

-- 2.3 Xmobar ---------------------------------------------------------------------------------
myLogHook :: [Handle] -> X ()
myLogHook h    = mapM_ (\handle -> dynamicLogWithPP $ wsPP { ppOutput = hPutStrLn handle }) h

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
                          , ppSep                 = " : "
                          , ppWsSep               = " "
                          , ppSort                = fmap
                                  (namedScratchpadFilterOutWorkspace.)
                                  (ppSort def)
                          , ppLayout              = const ""
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
                , projectDirectory = "/media/nas/home/10-19_Education/13_Bachelors_Degree/13.28_Thesis_CS"
                , projectStartHook = Just $ do
                    spawnOn (myWorkspaces !! 0) "zotero"
                    spawnOn (myWorkspaces !! 0) (myTerminal ++ " -e vim")
                }
            , Project
                { projectName = myWorkspaces !! 1
                , projectDirectory = "/media/nas/home/20-29_Work/22_TA/22.03_Numerical_Analysis"
                , projectStartHook = Just $ do
                    spawnOn (myWorkspaces !! 1) "qutebrowser ':session-load TA' --nowindow"
                    spawnOn (myWorkspaces !! 1) (myTerminal ++ " -e fff /media/nas/home/20-29_Work/22_TA")
                    spawnOn (myWorkspaces !! 1) "zotero"
                }
             , Project
                { projectName = myWorkspaces !! 2
                , projectDirectory = "$HOME"
                , projectStartHook = Just $ do
                    spawnOn (myWorkspaces !! 2) "qutebrowser ':session-load work' --nowindow"
                }
             , Project
                 { projectName = myWorkspaces !! 3
                 , projectDirectory = "/media/nas/home"
                 , projectStartHook = Just $ do
                     spawnOn (myWorkspaces !! 3) "zotero"
                     spawnOn (myWorkspaces !! 3) "qutebrowser ':session-load thesis' --nowindow"
                 }
             , Project
                 { projectName = myWorkspaces !! 4
                 , projectDirectory = "/media/nas/home"
                 , projectStartHook = Just $ do
                     spawnOn (myWorkspaces !! 4) "discord"
                     spawnOn (myWorkspaces !! 4) "thunderbird"
                 }
             , Project
                 { projectName = myWorkspaces !! 5
                 , projectDirectory = "$HOME"
                 , projectStartHook = Nothing
                 }
             , Project
                 { projectName = myWorkspaces !! 6
                 , projectDirectory = "/media/nas/home"
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
    activateProject (myProjects !! 7)
    
-- 4.2 Layouts --------------------------------------------------------------------------------
tall = renamed [Replace "tall"]
     $ ResizableTall 1 (3/100) (6/10) []

threecol = renamed [Replace "threecol"]
         $ ThreeCol 1 (3/100) (1/2)

bsp = renamed [Replace "bsp"]
    $ emptyBSP

-- Tabbed layout theme
myTabConfig :: Theme
myTabConfig = def { inactiveColor           = color11
                    , inactiveBorderColor   = color8
                    , inactiveTextColor     = color12
                    , activeColor           = color5
                    , activeBorderColor     = color5
                    , activeTextColor       = color0
                    , fontName              = myFont
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
             $ minimize
             myDefaultLayout

-- 4.3 Floating layouts -----------------------------------------------------------------------
myFloatLayouts :: [W.RationalRect]
myFloatLayouts = [ rtRect 0.41
                 , ltRect 0.4
                 , W.RationalRect 0.30 0.03 0.39 0.96
                 , W.RationalRect 0.15 0.20 0.30 0.60
                 , centeredRect 0.29 0.2
                 ]

-----------------------------------------------------------------------------------------------
-- 5. Utilities
-----------------------------------------------------------------------------------------------

-- 5.1 Managehook -----------------------------------------------------------------------------
myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
    [ className =? "Anki"                       --> doFloat
    , className =? "gimp"                       --> doFloat
    , className =? "VirtualBox Manager"         --> doFloat
    , className =? "VirtualBox Machine"         --> doFloat
    , className =? "matplotlib"                 --> doFloat
    , className =? "TelegramDesktop"            --> doFloat
    , className =? "mpv"                        --> doFloat
    , className =? "libreoffice-startcenter"    --> doFloat
    , className =? "Lxappearance"               --> doFloat
    , className =? "Arandr"                     --> doFloat
    , className =? "Xscreensaver-settings"      --> doFloat
    , className =? "Blueman-manager"            --> doFloat
    , className =? "zoom"                       --> doFloat
    , title     =? "Picture-in-Picture"         --> doFloat
    ]
    <+> composeOne
    [ currentWs =? "msc"               -?> doCenterFloat
    , currentWs =? "mul"               -?> doCenterFloat
    ]
    <+> manageDocks 
    <+> manageSpawn 
    <+> namedScratchpadManageHook myScratchPads

-- 5.2 Scratchpads ----------------------------------------------------------------------------
myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "htop"     "xterm -e htop"
                                (title =? "htop")
                                (customFloating $ W.RationalRect 0.7 0.1 0.25 0.2)
                , NS "fff"      "xterm -e fff"
                                (title =? "fff")
                                (customFloating $ centeredRect 0.15 0.25)
                , NS "pcmanfm"  "pcmanfm"
                                (className =? "Pcmanfm")
                                (customFloating $ W.RationalRect 0.35 0.35 0.3 0.3)
                , NS "emacs"    "emacs"
                                (className =? "Emacs")
                                (customFloating $ centeredRect 0.3 0.6)
                , NS "telegram" "telegram-desktop"
                                (className =? "TelegramDesktop")
                                (customFloating $ W.RationalRect 0.8 0.3 0.2 0.3)
                ]
  where

-- 5.3 Float cycling -------------------------------------------------------------------------
cycleFloat :: [W.RationalRect] -> Window -> WindowSet -> WindowSet
cycleFloat recs w s = 
    maybe (W.sink w) (W.float w) mRec s
        where
            mRec = find ( (< width) . rationalWidth) recs
            width = maybe 1 rationalWidth (w `M.lookup` W.floating s)

rationalWidth :: W.RationalRect -> Rational
rationalWidth (W.RationalRect _ _ w _) = w

centeredRect :: Rational -> Rational -> W.RationalRect
centeredRect w h = W.RationalRect x y w h
    where
        x :: Rational
        x = (1 - w) / 2

        y :: Rational
        y = (1 - h) / 2

ltRect :: Rational -> W.RationalRect
rtRect w = W.RationalRect 0.01 0.03 w 0.96

rtRect :: Rational -> W.RationalRect
ltRect w = W.RationalRect (1 - w - 0.01) 0.03 w 0.96
        
-- 5.4 Grid select ---------------------------------------------------------------------------
myGoColorizer :: Window -> Bool -> X (String, String)
myGoColorizer = colorRangeFromClassName
        (0xd8, 0xd5, 0xdd)      -- lowest inactive bg
        (0xbf, 0xb9, 0xc6)      -- highest inactive bg
        (0xbb, 0x99, 0xb4)      -- active bg
        (0x72, 0x67, 0x7e)      -- inactive fg
        (0xfb, 0xf1, 0xf2)      -- active fg

myBringColorizer :: Window -> Bool -> X (String, String)
myBringColorizer = colorRangeFromClassName
        (0xd8, 0xd5, 0xdd)      -- lowest inactive bg
        (0xbf, 0xb9, 0xc6)      -- highest inactive bg
        (0x69, 0xa9, 0xa7)      -- active bg        
        (0x72, 0x67, 0x7e)      -- inactive fg
        (0xfb, 0xf1, 0xf2)      -- active fg

gsWnSelConfig colorizer = (buildDefaultGSConfig myGoColorizer)
    { gs_cellheight     = 40
    , gs_cellwidth      = 200
    , gs_cellpadding    = 16
    , gs_originFractX   = 0.5
    , gs_originFractY   = 0.3
    , gs_font           = myFont
    }

gsWnBringConfig colorizer = (buildDefaultGSConfig myBringColorizer)
    { gs_cellheight     = 40
    , gs_cellwidth      = 200
    , gs_cellpadding    = 16
    , gs_originFractX   = 0.5
    , gs_originFractY   = 0.3
    , gs_font           = myFont
    }

gsPopupConfig = def
    { gs_cellheight   = 30
    , gs_cellwidth    = 200
    , gs_cellpadding  = 8
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = myFont
    }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = gsPopupConfig

-- 5.5 Tree select ---------------------------------------------------------------------------
treeActions :: [Tree (TS.TSNode (X ()))]
treeActions = [ Node (TS.TSNode "Session" "" (return ()))
                     [ Node (TS.TSNode "Shutdown" "Good night!" (spawn "poweroff")) []
                     , Node (TS.TSNode "Restart" "See you soon!" (spawn "reboot")) []
                     , Node (TS.TSNode "Suspend" "Break time!" (spawn "sLockscreenctl suspend")) []
                     , Node (TS.TSNode "Lock" "brb." (spawn "sLockscreenctl lock")) []
                     ]
             , Node (TS.TSNode "Productivity" "" (return ()))
                     [ Node (TS.TSNode "Zotero" "Your personal research assistant" (spawn "zotero")) []
                     , Node (TS.TSNode "Anki" "Study flashcards" (spawn "anki")) []
                     , Node (TS.TSNode "Emacs" "Doom emacs" (spawn "emacs")) []
                     , Node (TS.TSNode "LibreOffice" "Office productivity suite" (spawn "libreoffice")) []
                     , Node (TS.TSNode "Typora" "Minimalist markdown editor" (spawn "typora")) []
                     , Node (TS.TSNode "Vimwiki" "Personal wiki for Vim" (spawn (myEditor ++ "-c VimwikiIndex"))) []
                     ]
              , Node (TS.TSNode "Development" "" (return ()))
                     [ Node (TS.TSNode "Code" "Code editor redefined" (spawn "code")) []
                     , Node (TS.TSNode "Cantor" "FOSS mathematics application" (spawn "cantor")) []
                     , Node (TS.TSNode "Spyder" "Scientific Python IDE" (spawn "spyder")) []
                     , Node (TS.TSNode "RStudio" "R IDE" (spawn "rstudio-bin")) []
                     , Node (TS.TSNode "Geogebra" "Create mathematical figures" (spawn "geogebra")) []
                     ]
              , Node (TS.TSNode "Utilities" "" (return ()))
                     [ Node (TS.TSNode "FFF" "Fucking fast filemanager" (spawn (myTerminal ++ " -e fff"))) []
                     , Node (TS.TSNode "PCManFM" "Graphical filemanager" (spawn "pcmanfm")) []
                     , Node (TS.TSNode "htop" "Simple, interactive process viewer" (spawn (myTerminal ++ " -e htop"))) []
                     , Node (TS.TSNode "VirtualBox" "Oracle VM VirtualBox" (spawn "virtualbox")) []
                     ]
              , Node (TS.TSNode "Settings" "" (return ()))
                     [ Node (TS.TSNode "LxAppearance" "Desktop independent theme switcher" (spawn "lxappearance")) []
                     , Node (TS.TSNode "ARandR" "XRandR GUI" (spawn "arandr")) []
                     , Node (TS.TSNode "Xscreensaver" "Xscreensaver settings" (spawn "xscreensaver-settings")) []
                     , Node (TS.TSNode "Blueman manager" "Bluetooth settings" (spawn "blueman-manager")) []
                     , Node (TS.TSNode "Font manager" "Font manager and organizer" (spawn "font-manager")) []
                     , Node (TS.TSNode "GColor2" "Color picker" (spawn "gcolor2")) []
                     ]
              , Node (TS.TSNode "Dots" "" (return ()))
                     [ Node (TS.TSNode "vim" "The true text editor" (spawn (myEditor ++ "~/.config/vim/vimrc"))) []
                     , Node (TS.TSNode "bashrc" "The bourne again shell" (spawn (myEditor ++ "~/.config/bash/bashrc"))) []
                     , Node (TS.TSNode "xmonad" "XMonad configuration" (spawn (myEditor ++ "~/.config/xmonad/xmonad.hs"))) []
                     , Node (TS.TSNode "xmobar" "XMobar configuration" (spawn (myEditor ++ "~/.config/xmonad/xmobarrc"))) []
                     , Node (TS.TSNode "alacritty" "Alacritty terminal emulator" (spawn (myEditor ++ "~/.config/alacritty/alacritty.yml"))) []
                     , Node (TS.TSNode "kitty" "Kitty terminal emulator" (spawn (myEditor ++ "~/.config/kitty/kitty.conf"))) []
                     ]
              , Node (TS.TSNode "Internet" "" (return ()))
                     [ Node (TS.TSNode "Qutebrowser" "Minimal, keyboard-focused browser" (spawn "qutebrowser")) []
                     , Node (TS.TSNode "Firefox" "Firefox browser" (spawn "firefox")) []
                     ]
              , Node (TS.TSNode "Multimedia" "" (return ()))
                     [ Node (TS.TSNode "Idagio" "Classical music player" (spawn "idagio")) []
                     , Node (TS.TSNode "QuodLibet" "Music player" (spawn "quodlibet")) []
                     , Node (TS.TSNode "Spotify" "Digital music service" (spawn "spotify")) []
                     , Node (TS.TSNode "Cava" "Music visualizer" (spawn (myTerminal ++ " -e cava"))) []
                     ]
              , Node (TS.TSNode "Communication" "" (return ()))
                     [ Node (TS.TSNode "Telegram" "Messaging client for personal" (spawn "telegram-desktop")) []
                     , Node (TS.TSNode "Thunderbird" "Email client" (spawn "thunderbird")) []
                     , Node (TS.TSNode "Discord" "Chat client for uni and work" (spawn "discord")) []
                     , Node (TS.TSNode "Weechat" "IRC client" (spawn (myTerminal ++ " -e weechat"))) []
                     , Node (TS.TSNode "Zoom" "Video conferencing tool" (spawn "zoom")) []
                     ]
          ]

tsConfig :: TS.TSConfig a
tsConfig = TS.TSConfig
    { TS.ts_hidechildren   = True
    , TS.ts_background     = 0xc0c0c0c0
    , TS.ts_font           = myFont
    , TS.ts_node           = (0xfff2f1f4, 0xffbfb9c6)
    , TS.ts_nodealt        = (0xfff2f1f4, 0xffa59daf)
    , TS.ts_highlight      = (0xfffbf1f2, 0xffd57e85)
    , TS.ts_extra          = 0xffd57e85
    , TS.ts_node_width     = 200
    , TS.ts_node_height    = 30
    , TS.ts_originX        = 0
    , TS.ts_originY        = 0
    , TS.ts_indent         = 50
    , TS.ts_navigate       = tsNavigation
    }

tsNavigation = M.fromList
    [ ((0, xK_Escape), TS.cancel)
    , ((0, xK_Return), TS.select)
    , ((0, xK_space),  TS.select)
    , ((0, xK_Up),     TS.movePrev)
    , ((0, xK_Down),   TS.moveNext)
    , ((0, xK_Left),   TS.moveParent)
    , ((0, xK_Right),  TS.moveChild)
    , ((0, xK_k),      TS.movePrev)
    , ((0, xK_j),      TS.moveNext)
    , ((0, xK_h),      TS.moveParent)
    , ((0, xK_l),      TS.moveChild)
    ]

----------------------------------------------------------------------------------------------
-- 6. Keybindings
----------------------------------------------------------------------------------------------
myKeys :: [([Char], X ())]
myKeys =
-- 6.1 General -------------------------------------------------------------------------------
    [ ("M-<Return>", spawn myTerminal)                                          -- open a terminal
    , ("M-<Esc>", spawn "xmonad --restart")                                     -- restart xmonad
    , ("M-S-<Esc>", spawn "xmonad --recompile")                                 -- recompile xmonad
    , ("M-p", spawn "dmenu_run -nf '#FBF1F2' -nb '#8B8198' -sb '#BB99B4' -sf '#585062' -fn 'scientifica:pixelsize=16'")
    
-- 6.2 Navigation ----------------------------------------------------------------------------
    , ("M-j", windows W.focusDown)                                              -- move focus up
    , ("M-k", windows W.focusUp)                                                -- move focus down
    , ("M-S-j", windows W.swapDown)                                             -- swap focused with next
    , ("M-S-k", windows W.swapUp)                                               -- swap focused with previous
    , ("M-<Backspace>", promote)                                                -- promote focused to master
    , ("M-q", kill)                                                             -- kill focused
    , ("M-S-q", killAll)                                                        -- kill workspace
    , ("M-<Tab>", nextNonEmptyWS)                                               -- move to next workspace
    , ("M-S-<Tab>", prevNonEmptyWS)                                             -- move to previous workspace
    , ("M-0", nextNonEmptyWS)                                                   -- move focused to next empty workspace
    , ("M-w", switchProjectPrompt myPromptTheme)                                -- switch project
    , ("M-S-w", shiftToProjectPrompt myPromptTheme)                             -- move window to project
    , ("M-S-r", renameProjectPrompt myPromptTheme)                              -- rename current project
    , ("M-g", goToSelected $ gsWnSelConfig myGoColorizer)                       -- go to window
    , ("M-b", bringSelected $ gsWnBringConfig myBringColorizer)                 -- bring window
    , ("M-a", treeselectAction tsConfig treeActions)
    , ("M-S-c", killAllOtherCopies)                                             -- kill all copies of window
    , ("M-s", prevScreen)                                                       -- focus next monitor
    , ("M-d", nextScreen)                                                       -- focus previous monitor

-- 6.3 Keypad navigation --------------------------------------------------------------------
-- this part is not relevant anymore, after updating workspaces and new keyboard, but leaving it for now
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
    , ("M-f", withFocused $ windows . cycleFloat myFloatLayouts)
    , ("M-m", withFocused minimizeWindow)                                       -- minimize window
    , ("M-S-m", withLastMinimized maximizeWindowAndFocus)                       -- un-minimize last minimized

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
    , ("M-<XF86AudioMute>", spawn "playerctl -i Bose_QC35_II play-pause")       -- play/pause (no external kb)
    , ("<XF86AudioPlay", spawn "playerctl -i Bose_QC35_II play-pause")          -- play/pause                        
    , ("M-<XF86AudioRaiseVolume>", spawn "playerctl -i Bose_QC35_II next")      -- next track (no external kb)
    , ("<XF86AudioNext>", spawn "playerctl -i Bose_QC35_II next")               -- next track
    , ("M-<XF86AudioLowerVolume>", spawn "playerctl -i Bose_QC35_II previous")  -- prev track (no external kb)
    , ("<XF86AudioPrev>", spawn "playerctl -i Bose_QC35_II previous")           -- prev track
    , ("<XF86AudioMute>", spawn "amixer -D pulse set Master toggle")            -- mute
    , ("<XF86AudioLowerVolume>",   spawn "amixer set Master 5%- unmute")        -- raise volume by 5%
    , ("<XF86AudioRaiseVolume>",   spawn "amixer set Master 5%+ unmute")        -- lower volume by 5%
    , ("<xf86MonBrightnessDown>", spawn "brightnessctl s 10%-")                 -- decrease brightness by 10%
    , ("<XF86MonBrightnessUp>", spawn "brightnessctl s +10%")                   -- increase brightness by 10%
    , ("<Print>", spawn "flameshot full -p ~/Pictures/scrots -c")             -- full screenshot
    , ("M-<Print>", spawn "flameshot gui ~/Pictures/scrots")                    -- interactive screenshot

-- 6.8 Productivity ---------------------------------------------------------------------------
    , ("M-C-f", namedScratchpadAction myScratchPads "fff")
    , ("M-C-g", namedScratchpadAction myScratchPads "pcmanfm")
    , ("M-C-d", namedScratchpadAction myScratchPads "emacs")
    , ("M-C-p", namedScratchpadAction myScratchPads "htop")
    , ("M-C-c", namedScratchpadAction myScratchPads "telegram")
    , ("M-S-p", spawn "touch ~/.cache/pomodoro_session")
    , ("M-S-l", spawn "echo '50 10' > ~/.cache/pomodoro_session")
    , ("M-S-x", spawn "rm ~/.cache/pomodoro_session")
    ]
    ++
    [("M-c " ++ (show i), windows $ copy ws) | (i,ws) <- zip [1..9] myWorkspaces]   -- copy window to workspace

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

-- Functions for avoiding empty workspaces (from https://github.com/altercation/dotfiles-tilingwm/)
notSP = (return $ ("NSP" /=) . W.tag) :: X (WindowSpace -> Bool)
shiftAndView dir = findWorkspace getSortByIndex dir (WSIs notSP) 1
        >>= \t -> (windows . W.shift $ t) >> (windows . W.greedyView $ t)
shiftAndView' dir = findWorkspace getSortByIndexNoSP dir HiddenNonEmptyWS 1
        >>= \t -> (windows . W.shift $ t) >> (windows . W.greedyView $ t)
nextNonEmptyWS = findWorkspace getSortByIndexNoSP Next HiddenNonEmptyWS 1
        >>= \t -> (windows . W.view $ t)
prevNonEmptyWS = findWorkspace getSortByIndexNoSP Prev HiddenNonEmptyWS 1
        >>= \t -> (windows . W.view $ t)
getSortByIndexNoSP =
        fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex


-----------------------------------------------------------------------------------------------
-- 7. Main
-----------------------------------------------------------------------------------------------

main = do
    n <- countScreens
    xmprocs <- mapM (\i -> spawnPipe $ myWsBar ++ " -x " ++ show i) [0..n-1]
    xmonad 
        $ docks 
        $ dynamicProjects myProjects 
        $ myConfig xmprocs
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
