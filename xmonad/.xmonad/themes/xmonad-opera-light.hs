----------------------------------------------------------------------------------------------
--                                      __
-- .--.--.--------.-----.-----.---.-.--|  |
-- |_   _|        |  _  |     |  _  |  _  |
-- |__.__|__|__|__|_____|__|__|___._|_____|
--
-- Emilia's xmonad config
-- Author: Emilia Dunfelt, edun@dunfelt.se
----------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------
-- Modules
----------------------------------------------------------------------------------------------

-- Basics --------------------------------------------------------------------------------
import XMonad                                            -- core libraries
import System.IO                                         -- for xmobar
import qualified XMonad.StackSet as W                    -- window stack manipulation

-- Hooks ---------------------------------------------------------------------------------
import XMonad.ManageHook
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks                          -- don't cover the bar with windows
import XMonad.Hooks.SetWMName                            -- needed for JetBrains IDEs
import XMonad.Hooks.EwmhDesktops                         -- recognize windows i.e. in Zoom
import XMonad.Hooks.UrgencyHook                          -- highlight urgent windows

-- Layout --------------------------------------------------------------------------------
import XMonad.Layout.Simplest
import XMonad.Layout.Minimize
import XMonad.Layout.Maximize
import XMonad.Layout.Circle
import XMonad.Layout.Accordion
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Spacing
import XMonad.Layout.WindowNavigation                    -- window navigation (needed for tabs)
import XMonad.Layout.ResizableTile                       -- change window width/height
import XMonad.Layout.Tabbed                              -- tabbed layout
import XMonad.Layout.SubLayouts                          -- nested layouts (tabs everywhere)
import qualified XMonad.Layout.Groups as G               -- create layout groups
import XMonad.Layout.Groups.Helpers                      -- rearrange and move windows and groups
import XMonad.Layout.NoBorders                           -- remove borders
import XMonad.Layout.IndependentScreens                  -- find the number of screens (for xmobar)
import XMonad.Layout.MultiToggle as MT                   -- apply layout transformers
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))

-- Actions -------------------------------------------------------------------------------
import XMonad.Actions.CycleWS as CWS                     -- cycle through workspaces
import XMonad.Actions.WithAll                            -- killAll
import XMonad.Actions.SpawnOn
import XMonad.Actions.DynamicProjects
import XMonad.Actions.TreeSelect as TS
import XMonad.Actions.GridSelect
import XMonad.Actions.WindowMenu                         -- use GridSelect to display window options
import XMonad.Actions.CopyWindow                         -- copy windows
import XMonad.Actions.Minimize                           -- minimize windows
import XMonad.Actions.Search                             -- search via prompt
import XMonad.Actions.TagWindows                         -- tags to quickly move focus

-- Utilities -----------------------------------------------------------------------------
import XMonad.Util.Run                                   -- spawnPipe and hPutStrLn
import XMonad.Util.EZConfig (additionalKeysP)            -- Emacs-style keybindings
import XMonad.Util.NamedScratchpad                       -- scratchpad apps
import XMonad.Util.WorkspaceCompare                      -- sort workspaces
import XMonad.Util.SpawnOnce                             -- spawn stuff on initial launch

-- Prompt --------------------------------------------------------------------------------
import XMonad.Prompt                                     -- graphical prompts
import XMonad.Prompt.AppendFile                          -- launch prompt to write to file

-- Data ----------------------------------------------------------------------------------
import qualified Data.Map as M
import Data.Monoid
import Data.List
import Data.Tree


----------------------------------------------------------------------------------------------
-- Variables
----------------------------------------------------------------------------------------------

myTerminal :: [Char]
myTerminal                  = "kitty"

myEditor :: [Char]
myEditor                    = "emacs "

myModMask :: KeyMask
myModMask                   = mod4Mask

mySearchBrowser :: FilePath
mySearchBrowser             = "/bin/firefox"

-- Colors --------------------------------------------------------------------------------
fg      = "#2a2a2a"
bg      = "#fafafa"

bg00    = "#fafafa"
bg01    = "#f5f5f5"
bg02    = "#eeeeee"
bg03    = "#e0e0e0"
bg04    = "#bdbdbd"
bg05    = "#9e9e9e"
bg06    = "#757575"
bg07    = "#616161"
bg08    = "#454545"

red     = "#99324b"
orange  = "#ac4426"
yellow  = "#9a7500"
green   = "#4f894c"
cyan    = "#398eac"
blue    = "#3b6ea8"
purple  = "#842879"
teal    = "#29838d"

-- Theme ----------------------------------------------------------------------------------
myFont :: [Char]
myFont                      = "xft:Iosevka Curly:pixelsize=13:antialias=true:hinting=true,FontAwesome:pixelsize=14"

myBorderWidth :: Dimension
myBorderWidth               = 3

myFocusedBorderColor :: [Char]
myFocusedBorderColor        = blue

myNormalBorderColor :: [Char]
myNormalBorderColor         = bg00

-- Xmobar ---------------------------------------------------------------------------------
myLogHook :: [Handle] -> X ()
myLogHook h    = mapM_ (\handle -> dynamicLogWithPP $ wsPP { ppOutput = hPutStrLn handle }) h

myWsBar :: [Char]
myWsBar        = "xmobar ~/.config/xmonad/xmobarrc"

-- PP settings ---------------------------------------------------------------------------------
wsPP :: PP
wsPP           = xmobarPP { ppOrder               = id
                          , ppTitle               = xmobarColor   blue ""  . shorten 50
                          , ppCurrent             = xmobarColor   red "" . wrap "@" ""
                          , ppUrgent              = xmobarColor   orange ""    . wrap "+" ""
                          , ppVisible             = xmobarColor   cyan ""    . wrap ":" ""
                          , ppHiddenNoWindows     = const ""
                          , ppSep                 = " | "
                          , ppWsSep               = " "
                          , ppSort                = fmap
                                  (namedScratchpadFilterOutWorkspace.)
                                  (ppSort def)
                          , ppLayout              = const ""
                          }


-----------------------------------------------------------------------------------------------
-- Workspaces
-----------------------------------------------------------------------------------------------

-- Workspaces -----------------------------------------------------------------------------
myWorkspaces :: [WorkspaceId]
myWorkspaces = [ "tmp"          -- scratch
               , "net"          -- web stuff
               , "res"          -- research tools hub
               , "com"          -- communication
               , "mul"          -- multimedia (games, music, video)
               , "wrk"          -- work/TA
               , "alg-top"
               , "num-thry"
               , "rep-thry"
               ]

-- Projects -------------------------------------------------------------------------------
myProjects :: [Project]
myProjects = [ Project -- net
                { projectName = myWorkspaces !! 1
                , projectDirectory = "~/tmp"
                , projectStartHook = Just $ do
                    spawnOn (myWorkspaces !! 1) "firefox"
                }
             , Project -- res
                { projectName = myWorkspaces !! 2
                , projectDirectory = "~/media/doc/sci"
                , projectStartHook = Just $ do
                    spawnOn (myWorkspaces !! 2) "zotero"
                }
            , Project -- com
                { projectName = myWorkspaces !! 3
                , projectDirectory = "~/tmp"
                , projectStartHook = Just $ do
                    spawnOn (myWorkspaces !! 3) "evolution"
                    spawnOn (myWorkspaces !! 3) "pidg"
                }
            , Project -- mul
                { projectName = myWorkspaces !! 4
                , projectDirectory = "~/media"
                , projectStartHook = Just $ do
                    spawnOn (myWorkspaces !! 4) "qmmp"
                }
            , Project -- tmp
                { projectName = myWorkspaces !! 0
                , projectDirectory = "~/current"
                , projectStartHook = Nothing
                }
            , Project -- wrk
                { projectName = myWorkspaces !! 5
                , projectDirectory = "~/current"
                , projectStartHook = Just $ do
                    spawnOn (myWorkspaces !! 5) "emacs"
                    spawnOn (myWorkspaces !! 5) "discord"
                }
            , Project -- alg-top
                { projectName = myWorkspaces !! 6
                , projectDirectory = "~/current/14.06-algtopo"
                , projectStartHook = Nothing
                }
            , Project -- num-thry
                { projectName = myWorkspaces !! 7
                , projectDirectory = "~/current/14.07-numtheory"
                , projectStartHook = Nothing
                }
            , Project -- rep-thry
                { projectName = myWorkspaces !! 8
                , projectDirectory = "~/current/14.05-representations"
                , projectStartHook = Nothing
                }
            ]
                
-- Prompt ---------------------------------------------------------------------------------
myPromptTheme :: XPConfig
myPromptTheme = def
    { font = myFont
    , bgColor = bg02
    , fgColor = bg08
    , fgHLight = bg02
    , bgHLight = blue
    , borderColor = bg01
    , promptBorderWidth = 0
    , height = 20
    , position = Top
    }


-----------------------------------------------------------------------------------------------
-- Layouts
-----------------------------------------------------------------------------------------------

-- Startup --------------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
    spawn     "xrdb ~/.Xresources &"
    spawn     "feh --bg-tile /home/e/media/pic/walls/tiles/bw/tilerincewind.png &"
    spawnOnce "dex -ae xmonad &"
    spawnOnce "xsetroot -cursor_name left_ptr &"
    spawnOnce "dunst &"
    spawnOnce "tlp start &"
    spawnOnce "stalonetray -c /home/e/.config/stalonetray/stalonetrayrc &"
    spawnOnce "xscreensaver &"
    setWMName "LG3D"

-- Layouts --------------------------------------------------------------------------------
tall = ResizableTall 1 (3/100) (6/10) []
tallAccordion = G.group Accordion (Tall 1 (3/100) (6/10))

-- Tabbed layout theme
myTabConfig :: Theme
myTabConfig = def { inactiveColor           = bg02
                    , inactiveBorderColor   = bg02
                    , inactiveTextColor     = bg04
                    , activeColor           = bg01
                    , activeBorderColor     = bg01
                    , activeTextColor       = bg07
                    , fontName              = myFont
                    }
    
myDefaultLayout = tall
              ||| tallAccordion
              ||| Circle
              ||| emptyBSP

myLayoutHook = avoidStruts 
             $ configurableNavigation noNavigateBorders 
             $ addTabs shrinkText myTabConfig 
             $ subLayout [0] (Simplest) 
             $ smartBorders
             $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
             $ spacingRaw True (Border 5 5 5 5) True (Border 5 5 5 5) True
             $ minimize
             $ maximize
             myDefaultLayout

-- Floating layouts -----------------------------------------------------------------------
myFloatLayouts :: [W.RationalRect]
myFloatLayouts = [ rtRect 0.41
                 , ltRect 0.4
                 , W.RationalRect 0.30 0.03 0.39 0.96
                 , W.RationalRect 0.15 0.20 0.30 0.60
                 , centeredRect 0.29 0.2
                 ]

-----------------------------------------------------------------------------------------------
-- Utilities
-----------------------------------------------------------------------------------------------

-- Managehook -----------------------------------------------------------------------------
myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
    [ className =? "Anki"                       --> doFloat
    , className =? "gimp"                       --> doFloat
    , className =? "VirtualBox Manager"         --> doFloat
    , className =? "VirtualBox Machine"         --> doFloat
    , className =? "Matplotlib"                 --> doCenterFloat
    , className =? "TelegramDesktop"            --> doFloat
    , className =? "mpv"                        --> doFloat
    , className =? "libreoffice-startcenter"    --> doFloat
    , className =? "Lxappearance"               --> doFloat
    , className =? "Arandr"                     --> doFloat
    , className =? "Xscreensaver-settings"      --> doFloat
    , className =? "Blueman-manager"            --> doFloat
    , className =? "zoom"                       --> doFloat
    , className =? "Quodlibet"                  --> doFloat
    , className =? "xpad"                       --> doFloat
    , title     =? "Picture-in-Picture"         --> doFloat
    , title     =? "Terminal - "                --> doCenterFloat
    , className =? "qutebrowser"                --> doRectFloat (W.RationalRect 0.4 0.2 0.5 0.7)
    ]
    <+> composeOne
    [ currentWs =? "mul"                        -?> doCenterFloat ]
    <+> manageDocks 
    <+> manageSpawn 
    <+> namedScratchpadManageHook myScratchPads

-- Scratchpads ----------------------------------------------------------------------------
myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "htop"     (myTerminal ++ " --name=htop -e htop")
                                (resource =? "htop")
                                (customFloating $ W.RationalRect 0.59 0.03 0.4 0.3)
                , NS "fff"      (myTerminal ++ " --name=fff -e fff")
                                (resource =? "fff")
                                (customFloating $ centeredRect 0.15 0.25)
                , NS "thunar"  "thunar"
                                (className =? "Thunar")
                                (customFloating $ W.RationalRect 0.35 0.35 0.3 0.3)
                , NS "telegram" "telegram-desktop"
                                (className =? "TelegramDesktop")
                                (customFloating $ W.RationalRect 0.8 0.3 0.2 0.3)
                , NS "irc"      (myTerminal ++ " --name=weechat -e weechat")
                                (resource =? "weechat")
                                (customFloating $ centeredRect 0.5 0.4)
                , NS "vimwiki"  (myTerminal ++ " --name=vimwiki -e vim -c VimwikiIndex")
                                (resource =? "vimwiki")
                                (customFloating $ centeredRect 0.3 0.7)
                , NS "sp"       (myTerminal ++ " --name=scratchpad")
                                (resource =? "scratchpad")
                                (customFloating $ W.RationalRect 0.59 0.68 0.4 0.3)
                , NS "ql"       "quodlibet"
                                (className =? "Quodlibet")
                                (customFloating $ centeredRect 0.25 0.3)
                , NS "radio"    (myTerminal ++ " --name=pyradio -e pyradio")
                                (resource =? "pyradio")
                                (customFloating $ W.RationalRect 0.02 0.03 0.15 0.2)
                , NS "weylus"   "weylus"
                                (title =? "Weylus - 0.11.4")
                                (customFloating $ centeredRect 0.25 0.3)
                ]

-- Float cycling -------------------------------------------------------------------------
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
        
-- Grid select ---------------------------------------------------------------------------
myGoColorizer :: Window -> Bool -> X (String, String)
myGoColorizer = colorRangeFromClassName
        (0xbd, 0xbd, 0xbd)      -- lowest inactive bg
        (0xfa, 0xfa, 0xfa)      -- highest inactive bg
        (0x3b, 0x6e, 0xa8)      -- active bg
        (0x61, 0x61, 0x61)      -- inactive fg
        (0xfa, 0xfa, 0xfa)      -- active fg

myBringColorizer :: Window -> Bool -> X (String, String)
myBringColorizer = colorRangeFromClassName
        (0xbd, 0xbd, 0xbd)      -- lowest inactive bg
        (0xfa, 0xfa, 0xfa)      -- highest inactive bg
        (0x3b, 0x6e, 0xa8)      -- active bg
        (0x61, 0x61, 0x61)      -- inactive fg
        (0xfa, 0xfa, 0xfa)      -- active fg

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

-- Tree select ---------------------------------------------------------------------------
treeActions :: [Tree (TS.TSNode (X ()))]
treeActions = [ Node (TS.TSNode "Session" "" (return ()))
                     [ Node (TS.TSNode "Shutdown" "Good night!" (spawn "poweroff")) []
                     , Node (TS.TSNode "Restart" "See you soon!" (spawn "reboot")) []
                     , Node (TS.TSNode "Suspend" "Break time!" (spawn "sLockscreenctl suspend")) []
                     , Node (TS.TSNode "Lock" "brb." (spawn "sLockscreenctl lock")) []
                     ]
             , Node (TS.TSNode "Productivity" "" (return ()))
                     [ Node (TS.TSNode "Zotero" "Your personal research assistant" (spawn "zotero")) []
                     , Node (TS.TSNode "Emacs" "Doom emacs" (spawn "emacs")) []
                     , Node (TS.TSNode "Vim" "" (spawn myEditor)) []
                     , Node (TS.TSNode "Scrivener" "Creative writing assistant" (spawn "scrivener")) []
                     , Node (TS.TSNode "Calibre" "Ebook management" (spawn "calibre")) []
                     , Node (TS.TSNode "Xournal++" "Take notes and annotate documents" (spawn "xournalpp")) []
                     , Node (TS.TSNode "Anki" "Study flashcards" (spawn "anki")) []
                     , Node (TS.TSNode "LibreOffice" "Office productivity suite" (spawn "libreoffice")) []
                     , Node (TS.TSNode "Vimwiki" "Personal wiki for Vim" (spawn (myEditor ++ "-c VimwikiIndex"))) []
                     ]
              , Node (TS.TSNode "Development" "" (return ()))
                     [ Node (TS.TSNode "Cantor" "FOSS mathematics application" (spawn "cantor")) []
                     , Node (TS.TSNode "Geany" "Small and lightweight IDE" (spawn "geany")) []
                     , Node (TS.TSNode "Geogebra" "Create mathematical figures" (spawn "geogebra-classic")) []
                     , Node (TS.TSNode "RStudio" "R IDE" (spawn "rstudio-bin")) []
                     ]
              , Node (TS.TSNode "Utilities" "" (return ()))
                     [ Node (TS.TSNode "FFF" "Fucking fast filemanager" (spawn (myTerminal ++ " -e fff"))) []
                     , Node (TS.TSNode "Thunar" "Fast file manager" (spawn "thunar")) []
                     , Node (TS.TSNode "htop" "Simple, interactive process viewer" (spawn (myTerminal ++ " -e htop"))) []
                     , Node (TS.TSNode "VirtualBox" "Oracle VM VirtualBox" (spawn "virtualbox")) []
                     ]
              , Node (TS.TSNode "Settings" "" (return ()))
                     [ Node (TS.TSNode "LxAppearance" "Desktop independent theme switcher" (spawn "lxappearance")) []
                     , Node (TS.TSNode "ARandR" "XRandR GUI" (spawn "arandr")) []
                     , Node (TS.TSNode "Xscreensaver" "Xscreensaver settings" (spawn "xscreensaver-settings")) []
                     , Node (TS.TSNode "Blueman manager" "Bluetooth settings" (spawn "blueman-manager")) []
                     , Node (TS.TSNode "Font manager" "Font manager and organizer" (spawn "gnome-font-viewer")) []
                     , Node (TS.TSNode "GColor3" "Color picker" (spawn "gcolor3")) []
                     ]
              , Node (TS.TSNode "Dots" "" (return ()))
                     [ Node (TS.TSNode "vim" "The true text editor" (spawn (myEditor ++ "~/.config/vim/vimrc"))) []
                     , Node (TS.TSNode "kitty" "Kitty terminal emulator" (spawn (myEditor ++ "~/.config/kitty/kitty.conf"))) []
                     , Node (TS.TSNode "xmonad" "XMonad configuration" (spawn (myEditor ++ "~/.config/xmonad/xmonad.hs"))) []
                     , Node (TS.TSNode "xmobar" "XMobar configuration" (spawn (myEditor ++ "~/.config/xmonad/xmobarrc"))) []
                     , Node (TS.TSNode "bashrc" "The bourne again shell" (spawn (myEditor ++ "~/.bashrc"))) []
                     ]
              , Node (TS.TSNode "Internet" "" (return ()))
                     [ Node (TS.TSNode "Qutebrowser" "Minimal, keyboard-focused browser" (spawn "qutebrowser")) []
                     , Node (TS.TSNode "Firefox" "Firefox browser" (spawn "firefox")) []
                     , Node (TS.TSNode "Brave" "Brave browser" (spawn "brave")) []
                     ]
              , Node (TS.TSNode "Multimedia" "" (return ()))
                     [ Node (TS.TSNode "QuodLibet" "Music player" (spawn "quodlibet")) []
                     , Node (TS.TSNode "Spotify" "Digital music service" (spawn "spotify")) []
                     , Node (TS.TSNode "gPodder" "Podcast player" (spawn "gpod")) []
                     ]
              , Node (TS.TSNode "Communication" "" (return ()))
                     [ Node (TS.TSNode "Telegram" "Messaging client for personal" (spawn "telegram-desktop")) []
                     , Node (TS.TSNode "Evolution" "Mail and calendar" (spawn "evolution")) []
                     , Node (TS.TSNode "Discord" "Chat client for uni and work" (spawn "discord")) []
                     , Node (TS.TSNode "Weechat" "IRC client" (spawn (myTerminal ++ " -e weechat"))) []
                     , Node (TS.TSNode "Zoom" "Video conferencing tool" (spawn "zoom")) []
                     ]
              , Node (TS.TSNode "Games" "" (return ()))
                     [ Node (TS.TSNode "Runelite" "Old School Runescape launcher" (spawn "runelite")) []
                     ]
          ]

tsConfig :: TS.TSConfig a
tsConfig = TS.TSConfig
    { TS.ts_hidechildren   = True
    , TS.ts_font           = myFont
    , TS.ts_background     = 0xffbdbdbd
    , TS.ts_node           = (0xff454545, 0xffbdbdbd)
    , TS.ts_nodealt        = (0xff454545, 0xffbdbdbd)
    , TS.ts_highlight      = (0xffeeeeee, 0xff29838d)
    , TS.ts_extra          = 0xfffafafa
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
-- Search engines
----------------------------------------------------------------------------------------------
mySearch :: [(String, SearchEngine)]
mySearch = [ ("g", google)
           , ("w", wikipedia)
           , ("a", alpha)
           , ("m", mathworld)
           , ("d", duckduckgo)
           , ("s", scholar)
           ]

----------------------------------------------------------------------------------------------
-- Keybindings
----------------------------------------------------------------------------------------------
myKeys :: [([Char], X ())]
myKeys =
-- General -------------------------------------------------------------------------------
    [ ("M-<Return>", spawn myTerminal)                                          -- open a terminal
    , ("M-<Esc>", spawn "xmonad --restart")                                     -- restart xmonad
    , ("M-S-<Esc>", spawn "xmonad --recompile")                                 -- recompile xmonad
    , ("M-r", spawn "dmenu_run -nf '#616161' -nb '#eeeeee' -sb '#3b6ea8' -sf '#fafafa' -fn 'Iosevka Curly:pixelsize=14'")
    , ("M-f", spawn "edm")
    , ("M-S-p", spawn "unicode_dmenu")                                          -- select unicode symbol
    
-- Navigation ----------------------------------------------------------------------------
    , ("M-j", focusDown)                                              -- move focus up
    , ("M-k", focusUp)                                                -- move focus down
    , ("M-p", nextScreen)                                             -- move focus to next screen
    , ("M-S-j", swapDown)                                             -- swap focused with next
    , ("M-S-k", swapUp)                                               -- swap focused with previous
    , ("M-g j", focusGroupDown)                                       -- move group focus up
    , ("M-g k", focusGroupUp)                                         -- move group focus down
    , ("M-g h", moveToGroupDown True)                                 -- move window to next group
    , ("M-g l", moveToGroupUp True)                                   -- move window to previous group
    , ("M-g g", splitGroup)                                           -- create new group
    , ("M-<Backspace>", swapMaster)                                   -- promote focused to master
    , ("M-q", kill)                                                   -- kill focused
    , ("M-S-q", killAll)                                              -- kill workspace
    , ("M-<Tab>", nextNonEmptyWS)                                     -- move to next workspace
    , ("M-S-<Tab>", prevNonEmptyWS)                                   -- move to previous workspace
    , ("M-w", switchProjectPrompt myPromptTheme)                      -- switch project
    , ("M-S-w", shiftToProjectPrompt myPromptTheme)                   -- move window to project
    , ("M-S-r", renameProjectPrompt myPromptTheme)                    -- rename current project
    , ("M-S-d", changeProjectDirPrompt myPromptTheme)                 -- change project home directory
    , ("M-S-g", goToSelected $ gsWnSelConfig myGoColorizer)           -- go to window
    , ("M-S-b", bringSelected $ gsWnBringConfig myBringColorizer)     -- bring window
    , ("M-a", treeselectAction tsConfig treeActions)                  -- open TS menu
    , ("M-S-c", killAllOtherCopies)                                   -- kill all copies of window

-- Layout --------------------------------------------------------------------------------
    , ("M-,", spawn "autorandr -c")                                          -- reload monitor config
    , ("M-<Space>", sendMessage NextLayout)                                     -- next layout
    , ("M-S-f", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)     -- fullscreen view
    , ("M-S-<Space>", withFocused $ windows . cycleFloat myFloatLayouts)        -- float cycle window
    , ("M-m", withFocused minimizeWindow)                                       -- minimize window
    , ("M-S-m", withLastMinimized maximizeWindowAndFocus)                       -- un-minimize last minimized
    , ("M-o", windowMenu)                                                       -- open window menu

-- Resizing ------------------------------------------------------------------------------
    , ("M-h", sendMessage Shrink)                                               -- shrink horizontally
    , ("M-l", sendMessage Expand)                                               -- expand horizontally
    , ("M-n", sendMessage MirrorShrink)                                         -- shrink vertically
    , ("M-u", sendMessage MirrorExpand)                                         -- expand vertically
    , ("M-<Left>",    sendMessage $ ExpandTowards L)
    , ("M-<Right>",   sendMessage $ ExpandTowards R)
    , ("M-<Up>",      sendMessage $ ExpandTowards U)
    , ("M-<Down>",    sendMessage $ ExpandTowards D)

-- Tab-management ------------------------------------------------------------------------
    , ("M-C-h", sendMessage $ pullGroup L)                                      -- merge with left
    , ("M-C-j", sendMessage $ pullGroup D)                                      -- merge with below
    , ("M-C-k", sendMessage $ pullGroup U)                                      -- merge with above
    , ("M-C-l", sendMessage $ pullGroup R)                                      -- merge with right
    , ("M-C-m", withFocused (sendMessage . MergeAll))                           -- merge all windows on ws into tabbed
    , ("M-C-u", withFocused (sendMessage . UnMerge))                            -- unmerge tabbed

-- Keypad ---------------------------------------------------------------------------------
    , ("<KP_Right>", nextNonEmptyWS)
    , ("<KP_Left>", prevNonEmptyWS)
    , ("<KP_Up>", focusUp)
    , ("<KP_Down>", focusDown)
    , ("<KP_Begin>", nextScreen)
    , ("<KP_Enter>", spawn "dmenu_run -nf '#616161' -nb '#eeeeee' -sb '#3b6ea8' -sf '#fafafa' -fn 'Iosevka Curly:pixelsize=14'")
    , ("<KP_Home>", switchProjectPrompt myPromptTheme)
    , ("<KP_Prior>", shiftToProjectPrompt myPromptTheme)
    , ("<KP_Delete>", sendMessage NextLayout)
    , ("<KP_Insert>", withFocused $ windows . cycleFloat myFloatLayouts)
    , ("<KP_Divide>", focusUpTaggedGlobal "!")
    , ("<KP_Multiply>", focusUpTaggedGlobal "!!")
    , ("<KP_Subtract>", focusUpTaggedGlobal "!!!")
    , ("<KP_End>", windowMenu)
    , ("<KP_Next>", bringSelected $ gsWnBringConfig myBringColorizer)

-- Media keys -----------------------------------------------------------------------------
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
    , ("<Print>", spawn "flameshot screen -p ~/Pictures/scrots -c")             -- full screenshot
    , ("M-<Print>", spawn "flameshot gui -p ~/Pictures/scrots")                 -- interactive screenshot

-- Scratchpads ---------------------------------------------------------------------------
    , ("M-s f", namedScratchpadAction myScratchPads "fff")
    , ("M-s g", namedScratchpadAction myScratchPads "thunar")
    , ("M-s v", namedScratchpadAction myScratchPads "vimwiki")
    , ("M-s p", namedScratchpadAction myScratchPads "htop")
    , ("M-s t", namedScratchpadAction myScratchPads "telegram")
    , ("M-s i", namedScratchpadAction myScratchPads "irc")
    , ("M-s s", namedScratchpadAction myScratchPads "sp")
    , ("M-s m", namedScratchpadAction myScratchPads "ql")
    , ("M-s r", namedScratchpadAction myScratchPads "radio")
    , ("M-s w", namedScratchpadAction myScratchPads "weylus")

-- Tag navigation ------------------------------------------------------------------------
    -- , ("M-f 1", withFocused (addTag "!"))
    -- , ("M-f 2", withFocused (addTag "!!"))
    -- , ("M-f 3", withFocused (addTag "!!!"))
    -- , ("M-f j", focusUpTaggedGlobal "!")
    -- , ("M-f k", focusUpTaggedGlobal "!!")
    -- , ("M-f l", focusUpTaggedGlobal "!!!")
    -- , ("M-f a", tagPrompt myPromptTheme (\s -> withFocused (addTag s)))
    -- , ("M-f t", tagPrompt myPromptTheme (\s -> focusUpTaggedGlobal s))
    -- , ("M-f d", tagDelPrompt myPromptTheme)

-- Search prompts ------------------------------------------------------------------------
    , ("M-d w", promptSearchBrowser myPromptTheme mySearchBrowser wikipedia)
    , ("M-d d", promptSearchBrowser myPromptTheme mySearchBrowser duckduckgo)
    , ("M-d a", promptSearchBrowser myPromptTheme mySearchBrowser alpha)
    , ("M-d g", promptSearchBrowser myPromptTheme mySearchBrowser google)
    , ("M-d m", promptSearchBrowser myPromptTheme mySearchBrowser mathworld)
    , ("M-d s", promptSearchBrowser myPromptTheme mySearchBrowser scholar)

-- Productivity --------------------------------------------------------------------------
    , ("M-S-n", appendFilePrompt myPromptTheme "/home/edun/LOG")
    , ("M-S-s", spawn "echo '25 5' > ~/.cache/pomodoro_session")
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
-- Main
-----------------------------------------------------------------------------------------------

main = do
    n <- countScreens
    xmprocs <- mapM (\i -> spawnPipe $ myWsBar ++ " -x " ++ show i) [0..n-1]
    xmonad 
        $ withUrgencyHook NoUrgencyHook
        $ ewmh
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
