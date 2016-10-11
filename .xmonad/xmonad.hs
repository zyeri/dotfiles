-- {{{ imports

import System.IO
import System.Exit

import XMonad
import XMonad.Core
import XMonad.Config

import XMonad.Layout.Spacing
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ThreeColumns

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicBars
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.Place

import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect

import XMonad.Util.Loggers
import XMonad.Util.Run
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.SpawnOnce

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- }}}
-- {{{ options

-- {{{ core stuff

myTerminal              = "/usr/bin/termite"
myModMask               = mod4Mask
myBorderWidth           = 1
myNormalBorderColor     = "#282A2E"
myFocusedBorderColor    = "#373B41"
myFocusFollowsMouse     = False

-- clickable workspaces (xmobar)
myWorkspaces = clickable $ map show [1..9]
    where clickable l = [ "<action=`xdotool key super+" ++ show (n) ++ "`>" ++ ws ++ "</action>" | (i,ws) <- zip [1..9] l, let n = i ]


-- }}}
-- {{{ layouts

myLayouts = tiled |||
            Mirror tiled |||
            threecol |||
            Grid |||
            Full
    where
        -- default tiling algorithm partitions the screen into two panes
        tiled = Tall nmaster delta ratio

        threecol = ThreeColMid nmaster delta ratio

        -- the default number of windows in the master pane
        nmaster = 1

        -- percent of the screen occupied by the master pane (60%)
        ratio = 3/5

        -- percent of the screen to increment by when resizing panes
        delta = 3/100

-- layout to use for the workspace containing chat clients
imLayout = withIM (30/100) (ClassName "telegram-desktop") Grid

-- }}}
-- {{{ gridselect configuration

myGSConfig = defaultGSConfig
    { gs_cellheight     = 128
    , gs_cellwidth      = 256
    , gs_cellpadding    = 5
    }

-- }}}

-- }}}
-- {{{ keybinds

myAdditionalKeys =
    [ ("<XF86MonBrightnessUp>",     spawn "light -A 10")
    , ("<XF86MonBrightnessDown>",   spawn "light -U 10")
    , ("<XF86AudioRaiseVolume>",    spawn "pactl set-sink-volume 1 +2%")
    , ("<XF86AudioLowerVolume>",    spawn "pactl set-sink-volume 1 -2%")
    , ("<XF86AudioMute>",           spawn "pactl set-sink-mute 1 toggle")
    , ("<XF86AudioPlay>",           spawn "mpc toggle")
    , ("<XF86AudioNext>",           spawn "mpc next")
    , ("<XF86AudioPrev>",           spawn "mpc prev")
    , ("<XF86AudioStop>",           spawn "mpc stop")
    , ("<XF86Sleep>",               spawn "systemctl suspend")
    , ("M-p",                       spawn "dmenu_run -fn 'Roboto Mono-10' -o .85 -q -i -h 20")
    , ("M-b",                       sendMessage ToggleStruts)
    , ("M-]",                       nextWS)
    , ("M-[",                       prevWS)
    , ("M-g",                       goToSelected $ myGSConfig)
    ]

-- }}}
-- {{{ hooks

-- {{{ manage hook

myManageHook = (composeAll . concat $
    [ [ className =? "chromium"             --> doShift (myWorkspaces !! 0)
      , className =? "google-chrome"        --> doShift (myWorkspaces !! 0)
      , className =? "qutebrowser"          --> doShift (myWorkspaces !! 0)
      , className =? "emacs"                --> doShift (myWorkspaces !! 2)
      , className =? "gvim"                 --> doShift (myWorkspaces !! 2)
      , className =? "telegram-desktop"     --> doShift (myWorkspaces !! 7)
      , className =? "Slack"                --> doShift (myWorkspaces !! 7)
      , className =? "discord"              --> doShift (myWorkspaces !! 7)
      , className =? "VirtualBox"           --> doCenterFloat
      , resource  =? "stalonetray"          --> doIgnore
      ]
    , [ className =? c --> doFullFloat | c <- myFullFloats ]

    -- Google Chrome app windows
    , [ title =? t      --> doFloat | t <- myChromeApps ]

    -- misc.
    , [ isFullscreen                      --> doFullFloat
      , isDialog                          --> doCenterFloat
      , manageDocks
      ]
    ])
        where
            myFullFloats = [ "mpv" , "Vlc" ]
            myChromeApps = [ "Authy" ]

-- }}}
-- {{{ log hook

myLogHook h = dynamicLogWithPP $ xmobarPP
    { ppTitle       = xmobarColor "#c5c8c6" "" . shorten 80
    , ppOutput      = hPutStrLn h
    , ppLayout      = (\layout -> case layout of 
                            "SmartSpacing 5 Tall"           -> "Tall"
                            "SmartSpacing 5 Mirror Tall"    -> "Mirror Tall"
                            "SmartSpacing 5 ThreeCol"       -> "ThreeCol"
                            "SmartSpacing 5 Grid"           -> "Grid"
                            "SmartSpacing 5 Full"           -> "Full")
    }

-- }}}
-- {{{ startup hook

-- startup applications
myStartupHook = do
    -- start dropbox
    spawnOnce "/usr/bin/dropbox"

    -- start the notification daemon
    spawnOnce "/usr/bin/dunst"

    -- restore the wallpaper
    spawnOnce "/usr/bin/nitrogen --restore"

    -- start the compositing engine
    spawnOnce "/usr/bin/compton --config ~/.config/compton.conf"

-- }}}
-- {{{ layout hook

-- combine the layouts together to make the layoutHook
myLayoutHook = avoidStruts 
             $ onWorkspace (myWorkspaces !! 7) imLayout
             $ smartSpacing 5
             $ smartBorders
             $ myLayouts

-- }}}

-- }}}
-- {{{ main

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobarrc"

    xmonad $ withUrgencyHook NoUrgencyHook $ ewmh defaultConfig
        { terminal              = myTerminal
        , modMask               = myModMask
        , borderWidth           = myBorderWidth
        , normalBorderColor     = myNormalBorderColor
        , focusedBorderColor    = myFocusedBorderColor
        , focusFollowsMouse     = myFocusFollowsMouse
        , workspaces            = myWorkspaces

        -- hooks
        , handleEventHook       = fullscreenEventHook
        , layoutHook            = myLayoutHook
        , startupHook           = setWMName "LG3D"  <+> myStartupHook
        , manageHook            = manageDocks       <+> myManageHook
        , logHook               = myLogHook xmproc  <+> ewmhDesktopsLogHook
        } `additionalKeysP` myAdditionalKeys

-- }}}

-- vim: foldenable foldmethod=marker
