----------------------------------------------------------------------------------------------
--
-- .--.--.--------.-----.-----.---.-.--|  |
-- |_   _|        |  _  |     |  _  |  _  |
-- |__.__|__|__|__|_____|__|__|___._|_____|
--
-- Emilia's xmonad config
-- Edited: 2021-04-03
-- Author: Emilia Dunfelt, edun@dunfelt.se
--
-- Structure:
-- 1. Modules
-- 1.1 Basics
-- 1.2 Hooks
-- 1.3 Layout
-- 1.4 Actions
-- 1.5 Utilities
-- 2. Variables
-- 2.1 Theme
-- 2.2 Xmobar
-- 2.3 Colors
-- 3. Workspaces
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
import XMonad                                             -- core libraries
import qualified XMonad.StackSet as W                     -- window stack manipulation
import XMonad.Layout.WindowNavigation                     -- window navigation

-- 1.2 Hooks ---------------------------------------------------------------------------------
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks                           -- don't cover the bar with windows
import Data.Monoid
import XMonad.ManageHook

-- 1.3 Layout --------------------------------------------------------------------------------
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

-- 1.5 Utilities -----------------------------------------------------------------------------
import XMonad.Util.Run                                   -- spawnPipe and hPutStrLn
import XMonad.Util.EZConfig (additionalKeysP)            -- Emacs-style keybindings
import System.IO
import qualified Data.Map as M
import XMonad.Hooks.SetWMName                            -- needed for JetBrains IDEs
import XMonad.Util.NamedScratchpad


----------------------------------------------------------------------------------------------
-- 2. Variables
----------------------------------------------------------------------------------------------

myTerminal :: [Char]
myTerminal                  = "kitty"

myModMask :: KeyMask
myModMask                   = mod4Mask

-- 2.1 Theme ----------------------------------------------------------------------------------
myFont :: [Char]
myFont                      = "xft:Anonymice Nerd Font:pixelsize=14:antialias=true:hinting=true,FontAwesome:pixelsize=14"

myBorderWidth :: Dimension
myBorderWidth               = 3

myFocusedBorderColor :: [Char]
myFocusedBorderColor        = "#bb99b4"

myNormalBorderColor :: [Char]
myNormalBorderColor         = "#d8d5dd"

-- 2.2 Xmobar ---------------------------------------------------------------------------------
myLogHook :: Handle -> X ()
myLogHook h    = dynamicLogWithPP $ wsPP { ppOutput = hPutStrLn h }

myWsBar :: [Char]
myWsBar        = "xmobar ~/.config/xmonad/xmobarrc"

-- 2.3 Colors ---------------------------------------------------------------------------------
wsPP :: PP
wsPP           = xmobarPP { ppOrder               = \(ws:l:t:_)   -> [ws]
                            , ppCurrent             = xmobarColor   "#bb99b4" ""
                            , ppUrgent              = xmobarColor   "#d57e85" ""
                            , ppVisible             = xmobarColor   "#bfb9c6" "" 
                            , ppHidden              = xmobarColor   "#bfb9c6" "" 
                            , ppHiddenNoWindows     = xmobarColor   "#d8d5dd" ""
                            , ppSep                 = ""
                            , ppWsSep               = "    "
                            }


-----------------------------------------------------------------------------------------------
-- 3. Workspaces
-----------------------------------------------------------------------------------------------

myWorkspaces = [" \xf120 ", " \xf269 ", " \xf001 ", " \xf0e6 ", " \xf085 ", " \xf19c "]


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
    , className =? "discord"            --> doShift ( myWorkspaces !! 3 )
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
    spawnCal  = myTerminal ++ " -e calcurse"
    findCal   = resource =? "calcurse"
    manageCal = customFloating $ W.RationalRect (1/6) (1/6) (1/4) (1/4)

    spawnDiary  = myTerminal ++ " -e vim -c VimwikiMakeDiaryNote"
    findDiary   = title =? "vim"
    manageDiary = customFloating $ W.RationalRect (1/6) (1/6) (1/4) (1/4)


----------------------------------------------------------------------------------------------
-- 6. Keybindings
----------------------------------------------------------------------------------------------

myKeys :: [([Char], X ())]
myKeys =
-- 6.1 General -------------------------------------------------------------------------------
    [ ("M-<Return>", spawn myTerminal)                                          -- open a terminal
    , ("M-b", spawn "firefox")                                                  -- start firefox
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
    , ("M-<Tab>", nextWS)                                                       -- move to next workspace
    , ("M-S-<Tab>", prevWS)                                                     -- move to previous workspace
    , ("M-0", moveTo Next EmptyWS)                                              -- move focused to next empty workspace

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
    wsBar <- spawnPipe myWsBar
    xmonad $ docks def                                              -- some space for the bar
        { borderWidth               = myBorderWidth
        , startupHook               = myStartupHook
        , terminal                  = myTerminal
        , modMask                   = myModMask
        , normalBorderColor         = myNormalBorderColor
        , focusedBorderColor        = myFocusedBorderColor
        , workspaces                = myWorkspaces
        , mouseBindings             = myMouseBindings
        , layoutHook                = myLayoutHook
        , logHook                   = myLogHook wsBar
        , manageHook                = myManageHook
        } `additionalKeysP` myKeys
