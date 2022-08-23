--                          _ 
--  _ _ _____ ___ ___ ___ _| |
-- |_'_|     | . |   | .'| . |
-- |_,_|_|_|_|___|_|_|__,|___|
-- 

----------------------------------------------------------------------------------------------
-- Modules
----------------------------------------------------------------------------------------------

-- Basics --------------------------------------------------------------------------------
import XMonad                                            -- core libraries
import System.IO                                         -- for xmobar
import qualified XMonad.StackSet as W                    -- window stack manipulation
import Graphics.X11.ExtraTypes.XF86

-- Hooks ---------------------------------------------------------------------------------
import XMonad.ManageHook
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks                          -- don't cover the bar with windows
import XMonad.Hooks.SetWMName                            -- needed for JetBrains IDEs
import XMonad.Hooks.EwmhDesktops                         -- recognize windows i.e. in Zoom
import XMonad.Hooks.UrgencyHook                          -- highlight urgent windows
-- import XMonad.Hooks.Rescreen (need to switch to v. 0.17)

-- Layout --------------------------------------------------------------------------------
import XMonad.Layout.ThreeColumns
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Spacing
import XMonad.Layout.Minimize                            -- minimize windows
import XMonad.Layout.ResizableTile                       -- change window width/height
import XMonad.Layout.Groups.Helpers                      -- rearrange and move windows and groups
import XMonad.Layout.NoBorders                           -- remove borders
import XMonad.Layout.Tabbed                              -- tabbed layout
import XMonad.Layout.Simplest                            -- needed for tabbed layout
import XMonad.Layout.WindowNavigation                    -- window navigation (needed for tabs)
import XMonad.Layout.SubLayouts                          -- nested layouts (tabs everywhere)
import XMonad.Layout.IndependentScreens                  -- find the number of screens (for xmobar)
import XMonad.Layout.MultiToggle as MT                   -- apply layout transformers
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))

-- Actions -------------------------------------------------------------------------------
import XMonad.Actions.CycleWS as CWS                     -- cycle through workspaces
import XMonad.Actions.WithAll                            -- killAll
import XMonad.Actions.SpawnOn                            -- spawn on dedicated ws
import XMonad.Actions.DynamicProjects                    -- project workspaces + prompt
import XMonad.Actions.GridSelect                         -- bring/go to windows
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


----------------------------------------------------------------------------------------------
-- Variables
----------------------------------------------------------------------------------------------

myTerminal :: [Char]
myTerminal                  = "kitty"

myEditor :: [Char]
myEditor                    = "vim "

myModMask :: KeyMask
myModMask                   = mod4Mask

mySearchBrowser :: FilePath
mySearchBrowser             = "/bin/qutebrowser"

-- Colors --------------------------------------------------------------------------------
fg      = "#8B8198"
bg      = "#FBF1F2"

bg00    = "#FBF1F2"
bg01    = "#f2f1f4"
bg02    = "#d8d5dd"
bg03    = "#bfb9c6"
bg04    = "#a59daf"
bg05    = "#8b8198"
bg06    = "#72677E"
bg07    = "#585062"

red     = "#D57E85"
orange  = "#EBB790"
yellow  = "#DCB16C"
green   = "#A3B367"
cyan    = "#69A9A7"
blue    = "#7297B9"
magenta = "#BB99B4"
purple  = "#BAA58C"

-- Theme ----------------------------------------------------------------------------------
myFont :: [Char]
myFont                      = "xft:Terminus (TTF):pixelsize=14:antialias=true:hinting=true"

myBorderWidth :: Dimension
myBorderWidth               = 3

myFocusedBorderColor :: [Char]
myFocusedBorderColor        = red

myNormalBorderColor :: [Char]
myNormalBorderColor         = bg02

-- Xmobar ---------------------------------------------------------------------------------
myLogHook :: [Handle] -> X ()
myLogHook h    = mapM_ (\handle -> dynamicLogWithPP $ wsPP { ppOutput = hPutStrLn handle }) h

myWsBar :: [Char]
myWsBar        = "xmobar ~/.config/xmonad/xmobarrc"

-- PP settings ---------------------------------------------------------------------------------
wsPP :: PP
wsPP           = xmobarPP { ppOrder               = id
                          , ppTitle               = xmobarColor   bg04 ""  . shorten 50
                          , ppCurrent             = xmobarColor   green "" . wrap "@" ""
                          , ppUrgent              = xmobarColor   red ""    . wrap "+" ""
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
               , "res"          -- research hub
               , "mul"          -- multimedia (games, music, video)
               , "wrk"          -- work/TA
               , "14.02"
               , "14.09"
               , "14.10"
               , "14.11"
               , "sec"
               ]

-- Projects -------------------------------------------------------------------------------
myProjects :: [Project]
myProjects = [ Project -- tmp
                { projectName = myWorkspaces !! 0
                , projectDirectory = "~/current"
                , projectStartHook = Nothing
                }
             , Project -- net
                { projectName = myWorkspaces !! 1
                , projectDirectory = "~/tmp"
                , projectStartHook = Just $ do
                    spawnOn (myWorkspaces !! 1) "qutebrowser"
                }
             , Project -- res
                { projectName = myWorkspaces !! 2
                , projectDirectory = "~/media/doc/sci"
                , projectStartHook = Just $ do
                    spawnOn (myWorkspaces !! 2) (myTerminal ++ " -e xapers view")
                }
            , Project -- mul
                { projectName = myWorkspaces !! 3
                , projectDirectory = "~/media"
                , projectStartHook = Just $ do
                    spawnOn (myWorkspaces !! 3) (myTerminal ++ " -e ncmpcpp")
                }
            , Project -- wrk
                { projectName = myWorkspaces !! 4
                , projectDirectory = "~/current"
                , projectStartHook = Just $ do
                    spawnOn (myWorkspaces !! 4) myEditor
                }
            , Project -- num-thry
                { projectName = myWorkspaces !! 5
                , projectDirectory = "~/current/14.07-numtheory"
                , projectStartHook = Nothing
                }
            ]
                
-- Prompt ---------------------------------------------------------------------------------
myPromptTheme :: XPConfig
myPromptTheme = def
    { font = myFont
    , bgColor = bg01
    , fgColor = bg06
    , bgHLight = red
    , fgHLight = bg01
    , borderColor = bg01
    , promptBorderWidth = 0
    , height = 20
    , position = Bottom
    }


-----------------------------------------------------------------------------------------------
-- Layouts
-----------------------------------------------------------------------------------------------

-- Startup --------------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
    spawn     "xrdb ~/.Xresources &"
    spawn     "feh --bg-fill /home/e/media/pic/walls/city.jpg &"
    spawnOnce "dex -ae xmonad &"
    spawnOnce "xsetroot -cursor_name left_ptr &"
    spawnOnce "dunst &"
    spawnOnce "trayer --edge top --align right --monitor primary --widthtype request --heighttype pixel --height 22 &"
    spawnOnce "xscreensaver &"
    spawnOnce "mpd-mpris &"
    setWMName "LG3D"

-- Layouts --------------------------------------------------------------------------------
tall = ResizableTall 1 (3/100) (6/10) []
three = ThreeColMid 1 (3/100) (1/2)

myDefaultLayout = tall
              ||| three
              ||| emptyBSP

-- Tabbed layout theme
myTabConfig :: Theme
myTabConfig = def { inactiveColor           = bg02
                    , inactiveBorderColor   = bg
                    , inactiveTextColor     = bg04
                    , activeColor           = bg01
                    , activeBorderColor     = bg01
                    , activeTextColor       = bg05
                    , fontName              = myFont
                    }

-- Hook
myLayoutHook = avoidStruts 
             $ configurableNavigation noNavigateBorders 
             $ addTabs shrinkText myTabConfig 
             $ subLayout [0] (Simplest) 
             $ smartBorders
             $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
             $ spacingRaw True (Border 5 5 5 5) True (Border 5 5 5 5) True
             $ minimize
             myDefaultLayout

-- Floating layouts 
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
    , className =? "VirtualBox Manager"         --> doFloat
    , className =? "Matplotlib"                 --> doCenterFloat
    , className =? "R_x11"                      --> doCenterFloat
    , title     =? "Terminal - "                --> doCenterFloat
    , className =? "Gcolor3"                    --> doCenterFloat
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
    [ currentWs =? "mul"                        -?> doCenterFloat ]
    <+> manageDocks 
    <+> manageSpawn 
    <+> namedScratchpadManageHook myScratchPads

-- Scratchpads ----------------------------------------------------------------------------
myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "htop"     (myTerminal ++ " --name=htop -e htop")
                                (resource =? "htop")
                                (customFloating $ W.RationalRect 0.59 0.03 0.4 0.3)
                , NS "ranger"   (myTerminal ++ " --name=ranger -e ranger")
                                (resource =? "ranger")
                                (customFloating $ centeredRect 0.30 0.30)
                , NS "thunar"  "thunar"
                                (className =? "Thunar")
                                (customFloating $ W.RationalRect 0.35 0.35 0.3 0.3)
                , NS "telegram" "telegram-desktop"
                                (className =? "TelegramDesktop")
                                (customFloating $ W.RationalRect 0.8 0.3 0.2 0.3)
                , NS "irc"      (myTerminal ++ " --name=weechat -e weechat")
                                (resource =? "weechat")
                                (customFloating $ centeredRect 0.3 0.5)
                , NS "vimwiki"  (myTerminal ++ " --name=vimwiki -e vim -c VimwikiIndex")
                                (resource =? "vimwiki")
                                (customFloating $ centeredRect 0.3 0.7)
                , NS "sp"       (myTerminal ++ " --name=scratchpad")
                                (resource =? "scratchpad")
                                (customFloating $ W.RationalRect 0.59 0.68 0.4 0.3)
                , NS "mpd"      (myTerminal ++ " --name=ncmpcpp -e ncmpcpp")
                                (resource =? "ncmpcpp")
                                (customFloating $ centeredRect 0.3 0.2)
                , NS "radio"    (myTerminal ++ " --name=pyradio -e pyradio")
                                (resource =? "pyradio")
                                (customFloating $ W.RationalRect 0.02 0.03 0.15 0.2)
                , NS "weylus"   "weylus"
                                (title =? "Weylus - 0.11.4")
                                (customFloating $ centeredRect 0.25 0.3)
                , NS "task"     (myTerminal ++ " --name=task -e taskwarrior-tui")
                                (resource =? "task")
                                (customFloating $ centeredRect 0.25 0.3)
                , NS "mail"     (myTerminal ++ " --name=neomutt -e neomutt")
                                (resource =? "neomutt")
                                (customFloating $ centeredRect 0.6 0.7)
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
myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
        (0x8b, 0x81, 0x98)      -- lowest inactive bg
        (0xbf, 0xb9, 0xc6)      -- highest inactive bg
        (0xd5, 0x7e, 0x85)      -- active bg
        (0xfb, 0xf1, 0xf2)      -- inactive fg
        (0xf2, 0xf1, 0xf4)      -- active fg

gsConfig colorizer = (buildDefaultGSConfig colorizer)
    { gs_cellheight     = 40
    , gs_cellwidth      = 200
    , gs_cellpadding    = 16
    , gs_originFractX   = 0.5
    , gs_originFractY   = 0.5
    , gs_font           = myFont
    }

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
    , ("M-S-.", spawn "dunstctl history-pop")                                   -- show notification history
    , ("M-p p", spawn "rofi -show")                                             -- rofi
    , ("M-p f", spawn "rofi -modi filebrowser -show")                           -- file select
    , ("M-p q", spawn "rofi -show p -modi p:rofi-power-menu")                   -- power menu
    , ("M-p b", spawn "buku-menu")                                              -- bookmark select
    , ("M-p v", spawn "ytfzf -D")                                               -- search youtube
    , ("M-p d", spawn "papis rofi")                                             -- find papers
    
-- Navigation ----------------------------------------------------------------------------
    , ("M-j", focusDown)                                                        -- move focus up
    , ("M-k", focusUp)                                                          -- move focus down
    , ("M-e", nextScreen)
    , ("M-S-j", swapDown)                                                       -- swap focused with next
    , ("M-S-k", swapUp)                                                         -- swap focused with previous
    , ("M-<Backspace>", swapMaster)                                             -- promote focused to master
    , ("M-q", kill)                                                             -- kill focused
    , ("M-S-q", killAll)                                                        -- kill workspace
    , ("M-<Tab>", nextNonEmptyWS)                                               -- move to next workspace
    , ("M-S-<Tab>", prevNonEmptyWS)                                             -- move to previous workspace
    , ("M-w", switchProjectPrompt myPromptTheme)                                -- switch project
    , ("M-S-w", shiftToProjectPrompt myPromptTheme)                             -- move window to project
    , ("M-S-r", renameProjectPrompt myPromptTheme)                              -- rename current project
    , ("M-S-d", changeProjectDirPrompt myPromptTheme)                           -- change project home directory
    , ("M-S-g", goToSelected $ gsConfig myColorizer)                            -- go to window
    , ("M-S-b", bringSelected $ gsConfig myColorizer)                           -- bring window
    , ("M-c x", killAllOtherCopies)                                             -- kill all copies of window

-- Layout --------------------------------------------------------------------------------
    , ("M-,", spawn "autorandr -c")                                             -- reload monitor config
    , ("M-<Space>", sendMessage NextLayout)                                     -- next layout
    , ("M-S-f", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)     -- fullscreen view
    , ("M-S-t", toggleWindowSpacingEnabled >> toggleScreenSpacingEnabled)       -- toggle gaps
    , ("M-S-<Space>", withFocused $ windows . cycleFloat myFloatLayouts)        -- float cycle window
    , ("M-m", withFocused minimizeWindow)                                       -- minimize window
    , ("M-S-m", withLastMinimized maximizeWindowAndFocus)                       -- un-minimize last minimized
    , ("M-b", withFocused $ windows . W.sink)

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

-- Media keys -----------------------------------------------------------------------------
    , ("<XF86AudioPlay>", spawn "playerctl play-pause")
    , ("<XF86AudioNext>", spawn "playerctl next")                               -- next track
    , ("<XF86AudioPrev>", spawn "playerctl previous")                           -- prev track
    , ("<XF86AudioMute>", spawn "amixer -D pulse set Master toggle")
    , ("<XF86AudioLowerVolume>",   spawn "amixer -D pulse set Master 5%- unmute")
    , ("<XF86AudioRaiseVolume>",   spawn "amixer -D pulse set Master 5%+ unmute")
    , ("M-<XF86AudioMute>", spawn "playerctl play-pause")       -- play/pause (no external kb)
    , ("M-<XF86AudioLowerVolume>", spawn "playerctl previous")  -- prev track (no external kb)
    , ("M-<XF86AudioRaiseVolume>", spawn "playerctl next")      -- next track (no external kb)
    , ("<xf86MonBrightnessDown>", spawn "brightnessctl s 10%-")
    , ("<XF86MonBrightnessUp>", spawn "brightnessctl s +10%")
    , ("<Print>", spawn "xfce4-screenshooter -s ~/media/pic/scrots -f")         -- full screenshot
    , ("M-<Print>", spawn "xfce4-screenshooter -s ~/media/pic/scrots -r")       -- interactive screenshot

-- Scratchpads ---------------------------------------------------------------------------
    , ("M-s f", namedScratchpadAction myScratchPads "ranger")
    , ("M-s g", namedScratchpadAction myScratchPads "thunar")
    , ("M-s v", namedScratchpadAction myScratchPads "vimwiki")
    , ("M-s p", namedScratchpadAction myScratchPads "htop")
    , ("M-s t", namedScratchpadAction myScratchPads "telegram")
    , ("M-s i", namedScratchpadAction myScratchPads "irc")
    , ("M-s s", namedScratchpadAction myScratchPads "sp")
    , ("M-s m", namedScratchpadAction myScratchPads "mpd")
    , ("M-s r", namedScratchpadAction myScratchPads "radio")
    , ("M-s w", namedScratchpadAction myScratchPads "weylus")
    , ("M-s d", namedScratchpadAction myScratchPads "task")
    , ("M-s n", namedScratchpadAction myScratchPads "mail")

-- Tag navigation ------------------------------------------------------------------------
    , ("M-t 1", withFocused (addTag "!"))
    , ("M-t 2", withFocused (addTag "!!"))
    , ("M-t 3", withFocused (addTag "!!!"))
    , ("M-t j", focusUpTaggedGlobal "!")
    , ("M-t k", focusUpTaggedGlobal "!!")
    , ("M-t l", focusUpTaggedGlobal "!!!")
    , ("M-t a", tagPrompt myPromptTheme (\s -> withFocused (addTag s)))
    , ("M-t t", tagPrompt myPromptTheme (\s -> focusUpTaggedGlobal s))
    , ("M-t d", tagDelPrompt myPromptTheme)

-- Search prompts ------------------------------------------------------------------------
    , ("M-d w", promptSearchBrowser myPromptTheme mySearchBrowser wikipedia)
    , ("M-d d", promptSearchBrowser myPromptTheme mySearchBrowser duckduckgo)
    , ("M-d a", promptSearchBrowser myPromptTheme mySearchBrowser alpha)
    , ("M-d g", promptSearchBrowser myPromptTheme mySearchBrowser google)
    , ("M-d m", promptSearchBrowser myPromptTheme mySearchBrowser mathworld)
    , ("M-d s", promptSearchBrowser myPromptTheme mySearchBrowser scholar)

-- Productivity --------------------------------------------------------------------------
    , ("M-g s", spawn "echo '25 5' > ~/.cache/pomodoro_session")
    , ("M-g l", spawn "echo '50 10' > ~/.cache/pomodoro_session")
    , ("M-g x", spawn "rm ~/.cache/pomodoro_session")
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
    -- xmprocs <- mapM (\i -> spawnPipe $ myWsBar ++ " -x " ++ show i) [0..n-1]
    xmprocs <- mapM (\i -> spawnPipe $ myWsBar ++ " -x " ++ show i) [0]
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
