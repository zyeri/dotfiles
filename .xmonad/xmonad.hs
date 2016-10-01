-- xmonad.hs
import System.IO
import System.Exit

import XMonad
import XMonad.Core

import XMonad.Layout.Spacing
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimplestFloat

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.Place

import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- config
myTerminal              = "/usr/bin/termite"

myModMask               = mod4Mask

myBorderWidth           = 2

myNormalBorderColor     = "#282A2E"

myFocusedBorderColor    = "#373B41"

myWorkspaces = clickable $ map show [1..9]
    where clickable l = [ "<action=`xdotool key super+" ++ show (n) ++ "`>" ++ ws ++ "</action>" | (i,ws) <- zip [1..9] l, let n = i ]

-- layouts
myLayouts = tiled ||| Mirror tiled ||| Grid ||| Full
    where
        -- default tiling algorithm partitions the screen into two panes
        tiled = Tall nmaster delta ratio

        -- The default number of windows in the master pane
        nmaster = 1

        -- Default proportion of the screen occupied by the master pane
        ratio = 1/2

        -- Percent of the screen to increment by when resizing panes
        delta = 2/100

-- layout to use for the workspace containing chat clients
imLayout = withIM (25/100) (ClassName "telegram-desktop") Grid

-- combine everything together to make the layoutHook
myLayoutHook = avoidStruts 
             $ smartSpacing 10 
             $ smartBorders 
             $ onWorkspace (myWorkspaces !! 7) imLayout 
             $ myLayouts 

-- TODO: Make this section a little (and by a little I mean a lot) neater
myManageHook = (composeAll . concat $
    [ [ manageHook defaultConfig
      , className =? "chromium"           --> doShift (myWorkspaces !! 0)
      , className =? "google-chrome"      --> doShift (myWorkspaces !! 0)
      , className =? "qutebrowser"        --> doShift (myWorkspaces !! 0)
      , className =? "emacs"              --> doShift (myWorkspaces !! 2)
      , className =? "gvim"               --> doShift (myWorkspaces !! 2)
      , className =? "telegram-desktop"   --> doShift (myWorkspaces !! 7)
      , className =? "Slack"              --> doShift (myWorkspaces !! 7)
      , className =? "discord"            --> doShift (myWorkspaces !! 6)
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

-- GridSelect configuration
myGSConfig = defaultGSConfig 
    { gs_cellheight     = 128 
    , gs_cellwidth      = 256
    , gs_cellpadding    = 5
    }

-- TODO: I'd eventually like to use lemonbar and/or dzen2 instead of
-- xmobar so it can be more easily configured and customized.
myLogHook h = dynamicLogWithPP $ xmobarPP
    { ppTitle       = xmobarColor "#c5c8c6" "" . shorten 80
    , ppOutput      = hPutStrLn h
    }

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
    , ("M-p",                       spawn "dmenu_run -fn 'Roboto Mono-10:medium' -o .85 -q -i -h 20")
    , ("M-b",                       sendMessage ToggleStruts)
    , ("M-]",                       nextWS)
    , ("M-[",                       prevWS)
    , ("M-g",                       goToSelected $ myGSConfig)
    ]


main = do
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobarrc"

    xmonad $ ewmh defaultConfig
        { terminal              = myTerminal
        , modMask               = myModMask
        , borderWidth           = myBorderWidth
        , normalBorderColor     = myNormalBorderColor
        , focusedBorderColor    = myFocusedBorderColor

        -- hooks and layouts
        , manageHook            = myManageHook
        , handleEventHook       = docksEventHook <+> handleEventHook defaultConfig
        , logHook               = myLogHook xmproc
        , layoutHook            = myLayoutHook
        , focusFollowsMouse     = False

        -- Custom workspaces
        , workspaces            = myWorkspaces
        } `additionalKeysP` myAdditionalKeys
