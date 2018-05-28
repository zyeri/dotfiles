{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import XMonad hiding ((|||))
import XMonad.Layout hiding ((|||))
import XMonad.Operations

-- layouts
import XMonad.Layout.IM
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Fullscreen

import XMonad.Layout.Spacing
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace

import XMonad.Layout.LayoutCombinators
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances

-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ServerMode
import XMonad.Hooks.DynamicBars
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.XPropManage
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.DynamicProperty

-- actions
import XMonad.Actions.Submap
import XMonad.Actions.CycleWS
import XMonad.Actions.Promote
import XMonad.Actions.Commands
import XMonad.Actions.TagWindows
import XMonad.Actions.RotSlaves
import XMonad.Actions.GridSelect
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.DynamicWorkspaceGroups
import XMonad.Actions.CopyWindow

-- floating keys
import XMonad.Actions.FloatKeys
import XMonad.Actions.FloatSnap

-- prompt
import XMonad.Prompt
import XMonad.Prompt.Pass
import XMonad.Prompt.Workspace
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.ConfirmPrompt

-- util
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.WorkspaceCompare
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad

-- misc
import System.IO
import System.Exit
import System.Environment (getArgs)
import Control.Monad (when)

import Data.List
import Data.Ratio ((%))
import Data.Monoid

import Graphics.X11.Types
import Graphics.X11.Xinerama
import Graphics.X11.ExtraTypes.XF86

import qualified Data.Set as S
import qualified Data.Map as M

import qualified XMonad.StackSet as W
import qualified XMonad.Util.Timer as T
import qualified XMonad.Util.ExtensibleState as XS

-- | The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
workspaces' :: [WorkspaceId]
workspaces' = map show [ 1..9  :: Int ]

-- | modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
modMask' :: KeyMask
modMask' = mod4Mask

-- | Width of the window border in pixels.
--
borderWidth' :: Dimension
borderWidth' = 2

-- | Border colors for unfocused and focused windows, respectively.
--
normalBorderColor', focusedBorderColor' :: String
normalBorderColor'  = "#1d1f21"
focusedBorderColor' = "#969896"

------------------------------------------------------------------------
-- Window rules

-- | Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
--  xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
manageHook' :: ManageHook
manageHook' = composeAll
    [ appName =? "discord" --> doShift (workspaces' !! 8)
    , appName =? "google-chrome" --> doShift (workspaces' !! 0)
    , appName =? "telegram-desktop" --> doShift (workspaces' !! 8)

    --  make these windows float around the center of the screen
    , appName =? "imv" --> doFullFloat
    , appName =? "zeal" --> doCenterFloat
    , appName =? "arandr" --> doCenterFloat
    , appName =? "synergy" --> doCenterFloat
    , appName =? "pinentry" --> doCenterFloat
    , appName =? "nitrogen" --> doCenterFloat
    , appName =? "keepassxc" --> doCenterFloat
    , appName =? "pavucontrol" --> doCenterFloat
    , appName =? "gcr-prompter" --> doCenterFloat
    , appName =? "nm-connection-editor" --> doCenterFloat

    , className =? "mpv" --> doFullFloat

    -- dialog windows
    , windowRole =? "Preferences"          --> doCenterFloat
    , windowRole =? "GtkFileChooserDialog" --> doCenterFloat

    -- misc hooks
    , isDialog     --> doCenterFloat
    , isFullscreen --> doFullFloat
    , className =? "Dunst"          --> doIgnore
    , className =? "stalonetray"    --> doIgnore
    , resource  =? "desktop_window" --> doIgnore

    -- avoid the master pane
    , fmap not isDialog --> doF avoidMaster
    , namedScratchpadManageHook namedScratchpads'
    , scratchpadHook'
    ]

avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
     W.Stack t [] (r:rs) ->  W.Stack t [r] rs
     otherwise           -> c

windowRole :: XMonad.Query String
windowRole = stringProperty "WM_WINDOW_ROLE"

------------------------------------------------------------------------
-- Logging

-- | Perform an arbitrary action on each internal state change or X event.
-- Examples include:
--
--      * do nothing
--
--      * log the state to stdout
--
-- See the 'DynamicLog' extension for examples.
--

logHook' :: X ()
logHook' = multiPP focusedPP unfocusedPP

-- | Make a workspace clickable
clickable :: [Char] -> [Char]
clickable w  = "<action=$(xdotool key super+" ++ w ++ ")>" ++ w ++ "</action>"

-- | Command to launch Xmobar with
xmobarCommand :: Show a => a -> [Char]
xmobarCommand s = "/usr/bin/xmobar $HOME/.xmobarrc --screen=" ++ show s

-- | Create a satusbar
statusBarCreator :: DynamicStatusBar
statusBarCreator (S sid) = do
    spawnPipe . xmobarCommand $ sid

-- | Remove a statusbar
statusBarDestroyer :: DynamicStatusBarCleanup
statusBarDestroyer = do return ()

-- | How workspaces should be sorted on the statusbar
workspaceSort :: X WorkspaceSort
workspaceSort = (. namedScratchpadFilterOutWorkspace) <$> getSortByIndex

-- | Focused screen
focusedPP :: PP
focusedPP = def
    { ppCurrent         = xmobarColor "#81A2BE" "" . clickable . wrap "[" "]"
    , ppVisible         = xmobarColor "#969896" "" . clickable . wrap "(" ")"
    , ppHidden          = xmobarColor "#5F819D" "" . clickable
    , ppUrgent          = xmobarColor "#cc6666" "" . clickable . wrap " " " "
    , ppHiddenNoWindows = xmobarColor "#969896" "" . clickable
    , ppSort            = workspaceSort
    , ppTitle           = shorten 32
    }

-- | Unfocused screen
unfocusedPP :: PP
unfocusedPP = def
    { ppCurrent         = xmobarColor "#81A2BE" "" . clickable . wrap "[" "]"
    , ppVisible         = xmobarColor "#969896" "" . clickable . wrap "(" ")"
    , ppHidden          = xmobarColor "#5F819D" "" . clickable
    , ppUrgent          = xmobarColor "#cc6666" "" . clickable . wrap " " " "
    , ppHiddenNoWindows = xmobarColor "#969896" "" . clickable
    , ppSort            = workspaceSort
    , ppTitle           = shorten 32
    }

------------------------------------------------------------------------
-- Event handling

-- | Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards.
-- To combine event hooks, use mappend or mconcat from Data.Monoid.
handleEventHook' :: Event -> X All
handleEventHook' = do
    serverModeEventHookCmd' defaultCommands'
    <+> dynStatusBarEventHook statusBarCreator statusBarDestroyer

-- | Perform an arbitrary action at xmonad startup.
startupHook' :: X ()
startupHook' = do
    dynStatusBarStartup statusBarCreator statusBarDestroyer

------------------------------------------------------------------------
-- Extensible layouts
--
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--

-- | The available layouts.  Note that each layout is separated by |||, which
-- denotes layout choice.
layoutHook' = avoidStruts
            $ onWorkspace (workspaces' !! 8) telegram
            $ layouts
    where
        -- layout definitions
        layouts'      = resizable
                    ||| grid
                    ||| threecol
                    ||| threemid

        -- add toggle modifiers
        layouts       = mirror' $ borders' $ fullscreen' $ layouts'

        -- toggle modifiers
        mirror'       = mkToggle (single MIRROR)
        borders'      = mkToggle (single NOBORDERS)
        fullscreen'   = mkToggle (single NBFULL)

        -- rename a layout
        named n       = renamed [Replace n]

        delta         = (3%100)
        ratio         = toRational (2/(1 + sqrt 5 :: Double))

        -- spacing and gaps
        spacing'      = spacing 5
        gaps'         = gaps [(U, 10), (D, 10), (L, 10), (R, 10)]

        -- ResizableTall layout
        resizable     = named "Tall"
                      $ spacing'
                      $ gaps'
                      $ ResizableTall 1 delta ratio []

        -- ThreeCol layout
        threecol      = named "ThreeCol"
                      $ spacing'
                      $ gaps'
                      $ ThreeCol 1 (3%100) (1%2)

        -- ThreeColMid layout
        threemid      = named "ThreeColMid"
                      $ spacing'
                      $ gaps'
                      $ ThreeColMid 1 (3%100) (1%2)

        -- Grid layout
        grid          = named "Grid"
                      $ spacing'
                      $ gaps'
                      $ Grid

        -- layout for telegram
        telegramMatch = ClassName "TelegramDesktop"
        telegram'     = withIM (35%100) telegramMatch Grid
        telegram      = named "Grid"
                      $ reflectHoriz
                      $ spacing'
                      $ gaps'
                      $ telegram'

------------------------------------------------------------------------
-- Key bindings:

-- | The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
terminal' :: String
terminal' = "/usr/bin/termite"

-- | Whether focus follows the mouse pointer.
focusFollowsMouse' :: Bool
focusFollowsMouse' = False

-- | Whether a mouse click select the focus or is just passed to the window
clickJustFocuses' :: Bool
clickJustFocuses' = True

-- | The xmonad key bindings. Add, modify or remove key bindings here.
--
-- (The comment formatting character is used when generating the manpage)
--
keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' = \c -> keybindings' c `M.union` mkKeymap c keymap'

-- toggle floating for current window
toggleFloat w = windows (\s -> if M.member w (W.floating s)
                          then W.sink w s
                          else (W.float w (W.RationalRect (1%5) (1%5) (3%5) (3%5)) s))

keymap' :: [(String, X ())]
keymap' =
    [ ("M-<Return>", promote)
    -- quit, or restart
    , ("M-S-q", confirmPrompt promptConfig "Quit XMonad" $ io (exitWith ExitSuccess))

    -- volume controls
    , ("<XF86AudioMute>",        spawn "pactl set-sink-mute 1 toggle")
    , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume 1 +2%")
    , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume 1 -2%")

    , ("<XF86AudioStop>", spawn "mpc stop")
    , ("<XF86AudioNext>", spawn "mpc next")
    , ("<XF86AudioPrev>", spawn "mpc prev")
    , ("<XF86AudioPlay>", spawn "mpc toggle")

    -- backlight controls
    , ("<XF86MonBrightnessUp>",   spawn "light -A 1.5%")
    , ("<XF86MonBrightnessDown>", spawn "light -U 1.5%")

    -- launch a terminal (urxvt)
    , ("C-M-<Return>", spawn "urxvt")

    -- lock the screen
    , ("M-C-l", spawn "betterlockscreen -l")

    -- move between workspaces
    , ("M-]", moveTo Next (WSIs notNSP'))
    , ("M-[", moveTo Prev (WSIs notNSP'))

    , ("M-,", sendMessage (IncMasterN (-1)))
    , ("M-.", sendMessage (IncMasterN 1))

    , ("M-M1-h", sendMessage MirrorExpand)
    , ("M-M1-l", sendMessage MirrorShrink)

    , ("M-<Up>", sendMessage MirrorExpand)
    , ("M-<Down>", sendMessage MirrorShrink)
    , ("M-<Left>", sendMessage Shrink)
    , ("M-<Right>", sendMessage Expand)

    -- toggles
    , ("M-t f", sendMessage $ Toggle NBFULL)
    , ("M-t m", sendMessage $ Toggle MIRROR)
    , ("M-t b", sendMessage $ Toggle NOBORDERS)
    , ("M-t g", sendMessage ToggleGaps)
    , ("M-t s", sendMessage ToggleStruts)

    -- sink the current floating window
    , ("M-s t", withFocused $ windows . W.sink)

    -- float the current tiled window
    , ("M-s f", withFocused toggleFloat)

    -- scratchpads
    , ("M-s v", namedScratchpadAction namedScratchpads' "pavucontrol")
    , ("M-s k", namedScratchpadAction namedScratchpads' "keepassxc")
    , ("M-s m", namedScratchpadAction namedScratchpads' "arandr")
    , ("M-m", namedScratchpadAction namedScratchpads' "emacs")

    -- launchers
    , ("M-p",   spawn "rofi -show run")
    , ("M-o",   spawn "rofi -show combi")
    ]

-- default keys
keybindings' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keybindings' conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm .|. shiftMask, xK_c ), kill)

    , ((modm, xK_q), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad
    , ((modm, xK_space ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)

    -- make focused window always visible
    , ((modm, xK_v), windows copyToAll)

    -- toggle window state back
    , ((modm .|. shiftMask, xK_v), killAllOtherCopies)

    -- move focus up or down the window stack
    , ((modm, xK_j), windows W.focusDown)
    , ((modm, xK_k), windows W.focusUp)

    -- modifying the window order
    , ((modm .|. shiftMask, xK_j), windows W.swapDown)
    , ((modm .|. shiftMask, xK_k), windows W.swapUp)

    -- resizing the master/slave ratio
    , ((modm, xK_h), sendMessage Shrink)
    , ((modm, xK_l), sendMessage Expand)
    ]
    ++
        [((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f)) | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..], (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++
        zip (zip (repeat (mod4Mask)) [xK_1..xK_9]) (map (withNthWorkspace W.greedyView) [0..])
    ++
        zip (zip (repeat (mod4Mask .|. shiftMask)) [xK_1..xK_9]) (map (withNthWorkspace W.shift) [0..])
    ++
        [ ((modm .|. controlMask, k), windows $ swapWithCurrent i) | (i, k) <- zip workspaces' [xK_1 ..]]

-- | Mouse bindings: default actions bound to mouse events
mouseBindings' :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
mouseBindings' (XConfig {XMonad.modMask = modm}) = M.fromList
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
    , ((modm, button2), windows . (W.shiftMaster .) . W.focusWindow)
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
    ]

-- | List of commands for ServerMode
-- TODO: add more commands to this to make xmonadctl a little more useful?
defaultCommands' :: X [(String, X ())]
defaultCommands' = do
    return $
        [ ("next-ws",                moveTo Next (WSIs notNSP'))
        , ("prev-ws",                moveTo Prev (WSIs notNSP'))

        -- grid commands
        , ("grid-selected-focus",    goToSelected $ gridSelectConfig colorizer')
        , ("grid-selected-bring",    bringSelected $ gridSelectConfig colorizer')
        ]

-- | Named scratchpads
namedScratchpads' :: [NamedScratchpad]
namedScratchpads' =
    [ NS "keepassxc"   "keepassxc"   (appName =? "keepassxc")   floating'
    , NS "pavucontrol" "pavucontrol" (appName =? "pavucontrol") floating'
    , NS "arandr"      "arandr"      (appName =? "arandr")      floating'
    , NS "emacs"       "emacs"       (appName =? "emacs")       nonFloating
    ]
        where floating' = customFloating $ W.RationalRect 0.2 0.2 0.6 0.6

-- | Hook for managing scratchpads
scratchpadHook' :: ManageHook
scratchpadHook' = scratchpadManageHook $ W.RationalRect l t w h
  where
    h = 0.25 -- terminal height
    w = 1 -- terminal width
    t = 0
    l = 1 - w

-- | Custom urgency hook
urgencyHook' = withUrgencyHook NoUrgencyHook

-- | Check if a workspace is the named scratchpad workspace
notNSP' :: X (WindowSpace -> Bool)
notNSP' = (return $ ("NSP" /=) . W.tag) :: X (WindowSpace -> Bool)

-- | Add a tag to a window
addTagHook :: String -> ManageHook
addTagHook tag = do
    w <- ask
    liftX $ addTag tag w
    idHook

-- | GridSelect colorizer
colorizer' :: Window -> Bool -> X (String, String)
colorizer' = colorRangeFromClassName
    (0x00,0x00,0x00) -- lowest inactive bg
    (0x1C,0x1C,0x1C) -- highest inactive bg
    (0x44,0xAA,0xCC) -- active bg
    (0xBB,0xBB,0xBB) -- inactive fg
    (0x00,0x00,0x00) -- active fg

-- | GridSelect configuration
gridSelectConfig :: t -> GSConfig Window
gridSelectConfig colorizer = (buildDefaultGSConfig colorizer')
    { gs_font        = "xft:Iosevka:size=10:style=regular:antialias=true"
    , gs_cellpadding = 10
    , gs_cellheight  = 50
    , gs_cellwidth   = 200
    }

-- | Prompt configuration
promptConfig :: XPConfig
promptConfig = def
    { font                = "xft:Iosevka:size=12:style=regular:antialias=true"
    , position            = CenteredAt (1%2) (1%2)
    , historyFilter       = deleteConsecutive
    , historySize         = 256
    , bgColor             = "#1d1f21"
    , fgColor             = "#c5c8c6"
    , height              = 32
    , showCompletionOnTab = True
    , maxComplRows        = Just 8
    , promptBorderWidth   = 4
    , changeModeKey       = xK_grave
    }

-- | XMonad
main :: IO ()
main = do
    xmonad $ fullscreenSupport $ urgencyHook' $ docks $ ewmh $ def
        { terminal              = terminal'
        , modMask               = modMask'
        , borderWidth           = borderWidth'
        , normalBorderColor     = normalBorderColor'
        , focusedBorderColor    = focusedBorderColor'
        , focusFollowsMouse     = focusFollowsMouse'
        , clickJustFocuses      = clickJustFocuses'
        , workspaces            = workspaces'
        , layoutHook            = layoutHook'
        , manageHook            = manageHook'
        , handleEventHook       = handleEventHook'
        , startupHook           = startupHook'
        , keys                  = keys'
        , mouseBindings         = mouseBindings'
        , logHook               = logHook'
        }
