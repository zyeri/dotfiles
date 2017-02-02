-- xmonad.hs

import XMonad hiding ( (|||) )
import XMonad.Layout hiding ( (|||) )
import XMonad.Operations

-- Layout 
import XMonad.Layout.IM
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.Reflect
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.PerWorkspace
import XMonad.Layout.LayoutCombinators
import qualified XMonad.Layout.Renamed as R

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicBars
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ServerMode
import XMonad.Hooks.UrgencyHook

-- Actions
import XMonad.Actions.CycleWS
import XMonad.Actions.RotSlaves
import XMonad.Actions.Commands
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.Commands
import qualified XMonad.Actions.FlexibleResize as FR
import XMonad.Actions.FloatSnap

-- import XMonad.Actions.CopyWindow
-- import XMonad.Actions.Submap
-- import XMonad.Actions.GridSelect
-- import XMonad.Actions.CopyWindow
-- import XMonad.Actions.WindowMenu

-- Util
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig
import XMonad.Util.WorkspaceCompare
import XMonad.Util.NamedScratchpad

import qualified XMonad.Util.ExtensibleState as XS

-- Prompt
import XMonad.Prompt
import XMonad.Prompt.Workspace

-- other
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import Data.Monoid
import Data.Ratio ((%))
import System.IO
import System.Exit
import Graphics.X11.Types
import Graphics.X11.ExtraTypes.XF86

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8


-- | The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
-- workspaces' = map show [1 .. 9 :: Int]
workspaces' :: [WorkspaceId]
workspaces' = map show [ 1..9  :: Int] 

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
borderWidth' = 1

-- | Border colors for unfocused and focused windows, respectively.
--
normalBorderColor', focusedBorderColor' :: String
normalBorderColor'  = "#1d1f21"
focusedBorderColor' = "#969896"

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
-- --
avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ modifyFunc
    where modifyFunc c@(W.Stack t [] (r:rs)) = W.Stack t [r] rs
          modifyFunc c@_                     = c

manageHook' :: ManageHook
manageHook' = manageHooks'
          <+> namedScratchpadManageHook scratchpads
          <+> manageDocks 
          <+> transience'

manageHooks' :: ManageHook
manageHooks' = composeAll . concat $
    [ [ className   =? c    --> doShift (workspaces' !! 0) | c <- browsers ]
    , [ className   =? c    --> doShift (workspaces' !! 7) | c <- chatClients ]
    , [ className   =? c    --> doCenterFloat | c <- scratchpadFloats ]
    , [ className   =? c    --> doCenterFloat | c <- centerFloats ]
    , [ windowRole  =? d    --> doCenterFloat | d <- dialogs ]
    , [ fmap not isDialog   --> doF avoidMaster
      , isDialog            --> doCenterFloat
      , isFullscreen        --> doFullFloat 
      ] ]
        where browsers          = [ "Google-chrome" ]
              chatClients       = [ "TelegramDesktop", "discord", "Slack" ]
              centerFloats      = [ "Arandr", "VirtualBox" ]
              dialogs           = [ "GtkFileChooserDialog" ]
              scratchpadFloats  = [ "Zeal" ]
              windowRole        = stringProperty "WM_WINDOW_ROLE"

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

-- make a workspace clickable
clickable :: [Char] -> [Char]
clickable w  = "<action=`xdotool key super+" ++ w ++ "`>" ++ w ++ "</action>"

xmobarCommand :: Show a => a -> [Char]
xmobarCommand s = "/usr/bin/xmobar ~/.xmonad/xmobarrc --screen=" ++ show s

statusBarCreator :: DynamicStatusBar
statusBarCreator (S sid) = do 
    spawnPipe . xmobarCommand $ sid

statusBarDestroyer :: DynamicStatusBarCleanup
statusBarDestroyer = do return ()

focusedStatusBarPP :: PP
focusedStatusBarPP = def 
    { ppCurrent         = xmobarColor "#81A2BE" "" . wrap "[" "]" . clickable
    , ppHidden          = xmobarColor "#707880" "" . clickable
    , ppHiddenNoWindows = wrap "" "" . clickable
    , ppTitle           = shorten 40
    , ppVisible         = wrap "(" ")" . clickable
    , ppUrgent          = xmobarColor "#1d1f21" "#cc6666"
    , ppWsSep           = " "
    , ppSep             = " :: "
    , ppOrder           = \(ws:l:_:_) -> [ws, l]
    }

unfocusedStatusBarPP :: PP
unfocusedStatusBarPP = def
    { ppCurrent     = wrap "[" "]" 
    , ppWsSep       = " "
    }

logHook' = do
    multiPP focusedStatusBarPP unfocusedStatusBarPP

------------------------------------------------------------------------
-- Event handling

-- | Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards.
-- To combine event hooks, use mappend or mconcat from Data.Monoid.
handleEventHook' :: Event -> X All
handleEventHook' =
    serverModeEventHookCmd' defaultCommands'
    <+> fullscreenEventHook
    <+> docksEventHook
    <+> dynStatusBarEventHook statusBarCreator statusBarDestroyer

-- | Perform an arbitrary action at xmonad startup.
startupHook' :: X ()
startupHook' = do
    dynStatusBarStartup statusBarCreator statusBarDestroyer

    -- run startup script
    spawnOnce "/home/alex/bin/xmonad-startup.sh"

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

layoutHook' = avoidStruts . smartBorders
            . spacing 10  . gaps [(U, 15), (D, 20), (L, 20), (R, 20)]
            $ onWorkspace(workspaces' !! 7) telegramLayout 
            $ layouts
    where
        layouts = resizable
              ||| Mirror resizable
              ||| noBorders Full

        -- resizable Tall layout
        resizable   = R.renamed [R.Replace "Tall"]      $ ResizableTall nmaster delta ratio []

        -- The default number of windows in the master pane
        nmaster     = 1

        -- Default proportion of screen occupied by master pane
        ratio       = toRational (2/(1 + sqrt 5 :: Double))

        -- Percent of screen to increment by when resizing panes
        delta       = 3/100

        -- use 35% of the screen for telegram
        telegramLayout = R.renamed [R.Replace "IM" ] $ reflectHoriz $ withIM (35%100) (ClassName "TelegramDesktop") ( reflectHoriz $ resizable ||| Mirror resizable)

-- | The xmonad key bindings. Add, modify or remove key bindings here.
--
-- (The comment formatting character is used when generating the manpage)
-- myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
-- myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) -- %! Launch terminal
    , ((modm,               xK_p     ), spawn "dmenu_run") -- %! Launch dmenu
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun") -- %! Launch gmrun
    , ((modm .|. shiftMask, xK_c     ), kill) -- %! Close the focused window

    , ((modm,               xK_space ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default

    , ((modm,               xK_n     ), refresh) -- %! Resize viewed windows to the correct size

    -- move focus up or down the window stack
    , ((modm,               xK_Tab   ), windows W.focusDown) -- %! Move focus to the next window
    , ((modm .|. shiftMask, xK_Tab   ), windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modm,               xK_j     ), windows W.focusDown) -- %! Move focus to the next window
    , ((modm,               xK_k     ), windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modm,               xK_m     ), windows W.focusMaster  ) -- %! Move focus to the master window

    -- modifying the window order
    , ((modm,               xK_Return), windows W.swapMaster) -- %! Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  ) -- %! Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    ) -- %! Swap the focused window with the previous window

    -- resizing the master/slave ratio
    , ((modm,               xK_h     ), sendMessage Shrink) -- %! Shrink the master area
    , ((modm,               xK_l     ), sendMessage Expand) -- %! Expand the master area

    -- floating layer support
    , ((modm,               xK_t     ), withFocused $ windows . W.sink) -- %! Push window back into tiling

    -- increase or decrease number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area

    -- quit, or restart
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess)) -- %! Quit xmonad
    , ((modm              , xK_q     ), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad<Paste>

    , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume 1 +2%")
    , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume 1 -2%")
    -- media keys
    , ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute 1 toggle")
    , ((0, xF86XK_AudioNext), spawn "mpc -q next")
    , ((0, xF86XK_AudioPrev), spawn "mpc -q prev")
    , ((0, xF86XK_AudioPlay), spawn "mpc toggle")
    , ((0, xF86XK_AudioStop), spawn "mpc stop")
    --
    -- backlight
    , ((0, xF86XK_MonBrightnessUp),     spawn "light -A 1.5%")
    , ((0, xF86XK_MonBrightnessDown),   spawn "light -U 1.5%")

    -- toggle struts 'n gaps
    , ((modm, xK_b), sendMessage $ ToggleStruts)
    , ((modm, xK_u), sendMessage $ ToggleGaps)

    -- lock the screen
    , ((controlMask .|. mod1Mask, xK_l), spawn "/home/alex/bin/lock")

    -- resize windows vertically
    , ((modm .|. shiftMask, xK_l), sendMessage MirrorShrink)
    , ((modm .|. shiftMask, xK_h), sendMessage MirrorExpand)

    -- rotate workspaces
    , ((modm .|. shiftMask, xK_bracketleft ), rotAllUp)
    , ((modm .|. shiftMask, xK_bracketright), rotAllDown)

    -- move between workspaces
    , ((modm, xK_bracketleft  ), moveTo Prev (WSIs notNSP))
    , ((modm, xK_bracketright ), moveTo Next (WSIs notNSP))

    -- move between screens 
    , ((modm, xK_Right),    nextScreen)
    , ((modm, xK_Left),     prevScreen)

    -- shift windows between screens
    , ((modm .|. shiftMask, xK_Right),  shiftNextScreen)
    , ((modm .|. shiftMask, xK_Left),   shiftPrevScreen)

    , ((mod1Mask .|. controlMask .|. shiftMask, xK_Right), shiftToNext >> nextWS)
    , ((mod1Mask .|. controlMask .|. shiftMask, xK_Left), shiftToPrev >> prevWS)


    -- swap current workspace with the previously displayed workspace
    , ((modm, xK_z), namedScratchpadAction scratchpads "zeal")

    , ((modm .|. shiftMask, xK_m ), workspacePrompt def (windows . W.shift))

    , ((modm, xK_p), spawn "rofi -show run")
    , ((modm .|. controlMask, xK_x), spawn "/home/alex/bin/lock")
    ]
        ++ 
        [((mod4Mask .|. mask, key), f sc)
          | (key, sc) <- zip [xK_w, xK_e] [0..]
          , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]
        ++
        zip (zip (repeat (mod4Mask)) [xK_1..xK_9]) (map (withNthWorkspace W.greedyView) [0..])
        ++
        zip (zip (repeat (mod4Mask .|. shiftMask)) [xK_1..xK_9]) (map (withNthWorkspace W.shift) [0..])


mouseBindings' (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow  w     >> afterDrag (snapMagicMove (Just 50) (Just 50) w )))
    , ((modm, button2), (\w -> focus w >> windows W.swapMaster   >> afterDrag (snapMagicResize [L,R,U,D] (Just 50) (Just 50) w )))
    , ((modm, button3), (\w -> focus w >> FR.mouseResizeWindow w >> afterDrag (snapMagicResize [L,R,U,D] (Just 50) (Just 50) w )))
    ]
--named scratchpads
scratchpads = [ NS "zeal" "zeal" (className =? "Zeal") defaultFloating ]

-- Filter out the NSP workspace
notNSP = (return $ ("NSP" /=) . W.tag) :: X (WindowSpace -> Bool)

-- custom command list
defaultCommands' :: X [(String, X ())]
defaultCommands' = do
    return $ otherCommands
  where
      otherCommands =
          [ 
            ("next-ws", moveTo Next (WSIs notNSP))
          , ("prev-ws", moveTo Prev (WSIs notNSP))
          ]

main :: IO ()
main = do

    xmonad $ withUrgencyHook NoUrgencyHook $  ewmh $ def
        { terminal              = terminal'
        , modMask               = modMask'
        , borderWidth           = borderWidth'
        , normalBorderColor     = normalBorderColor'
        , focusedBorderColor    = focusedBorderColor'
        , focusFollowsMouse     = focusFollowsMouse'
        , clickJustFocuses      = clickJustFocuses'
        , workspaces            = workspaces'
        , layoutHook            = layoutHook'
        , logHook               = logHook' 
        , manageHook            = manageHook'
        , handleEventHook       = handleEventHook'
        , startupHook           = startupHook'
        , keys                  = keys'
        , mouseBindings         = mouseBindings'
        } 
