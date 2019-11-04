{-#LANGUAGE NamedFieldPuns#-}

import XMonad
import XMonad.Actions.SpawnOn
import qualified XMonad.Hooks.EwmhDesktops as E
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.FadeInactive
import XMonad.Layout.Fullscreen
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Spacing
import qualified XMonad.StackSet as W
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Config.Xfce
import XMonad.Config.Desktop

import qualified Data.Map as M
import Data.Maybe

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Graphics.X11.ExtraTypes

import System.Exit
import System.IO

-----------------------------------------------------------------------
-- Workspaces
-- Default number of workspaces

myWorkspaces :: [String]
myWorkspaces = clickable workspaceLabels
 where
  clickable l =

    [ "<action=xdotool key super+"
        ++ show i
        ++ " button=1>"
        ++ ws
        ++ "</action>"
    | (i, ws) <- zip ([1 .. 9]) l
    ]
  workspaceLabels = zipWith makeLabel [1 .. 9 :: Int] icons
  makeLabel index icon = show index ++ ":<fn=1>" ++ icon : "</fn>"
  icons =
    [ '\xf269'
    , '\xf120'
    , '\xf121'
    , '\xf128'
    , '\xf128'
    , '\xf128'
    , '\xf128'
    , '\xf128'
    , '\xf1b6'
	]

-----------------------------------------------------------------------
-- ManageHook
-- Manages how windows tile

-- doFloat lets windows float
-- doShift sends window to another workspace

-- Something doesn't work correctly,
-- Is it manageSpawn??? composeAll???
myManageHook :: ManageHook
myManageHook = manageSpawn <+> composeAll
	[ isDialog --> placeHook (fixed (0.5, 0.5))
	, className =? "Gimp" --> doFloat
	, resource =? "desktop_window" --> doIgnore
	, className =? "spotify" --> doShift "9"
	, className =? "xfrun4" --> doFloat
	, className =? "xfce4-appfinder" --> doFloat
	--, className =? "Xfce4-panel" --> doFloat
	, className =? "Wrapper-2.0" --> doFloat
	, className =? "Xfce4-popup-whiskermenu" --> doFloat
	, className =? "Xfce4-settings-manager" --> doFloat
	, className =? "Steam" --> doFloat
	, className =? "dmenu" --> doFloat
	, className =? "Navigator" --> doShift "1"
	, isFullscreen --> (doF W.focusDown <+> doFullFloat)
	, manageDocks]

------------------------------------------------------------------------
-- Colors and borders
myNormalBorderColor :: String
myNormalBorderColor = "#663399"

myFocusedBorderColor :: String
myFocusedBorderColor = "#FFD300"

-- Color of current workspace in xmobar.
--xmobarCurrentWorkspaceColor :: String
--xmobarCurrentWorkspaceColor = "#Af745f"

-- Width of the window border in pixels.
myBorderWidth :: Dimension
myBorderWidth = 0

-----------------------------------------------------------------------
-- StartupHook
-- Spawning Compton in order to get background, while app is in front

myStartupHook :: X ()
myStartupHook = spawn "compton --backend glx --xrender-sync --xrender-sync-fence -fcCz -l -17 -t -17"

-----------------------------------------------------------------------
-- Mousebindings for making draggable windows

myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig { modMask } = M.fromList
  [
    -- Set the window to floating mode and move by dragging
    ((modMask, button1), \w -> focus w >> mouseMoveWindow w)
  ,

    -- Raise the window to the top of the stack
    ((modMask, button2), \w -> focus w >> windows W.swapMaster)
  ,

    -- Set the window to floating mode and resize by dragging
	((modMask, button3), \w -> focus w >> mouseResizeWindow w)
  ]

-----------------------------------------------------------------------
-- Keybindings

myKeyBindings :: [((KeyMask, KeySym), X ())]
myKeyBindings = [
				-- Take screenshot of master window???
				((controlMask, xK_Print), spawn "sleep 0.2; scrot -s -e 'mv $f ~/Pictures/Screenshots/'")

				-- Take screenshot of workspace
				, ((0, xK_Print), spawn "scrot -e 'mv $f ~/Pictures/Screenshots/'")

				-- Toggles media
				, ((mod4Mask, xK_aring), spawn "playerctl play-pause")

				-- Media next
				,((mod4Mask .|. shiftMask, xK_aring), spawn "playerctl next")

				-- Media previous
				, ((mod4Mask .|. shiftMask .|. controlMask, xK_aring), spawn "playerctl previous")

				-- Start firefox
				, ((mod4Mask, xK_f), spawnOn "1" "firefox")

				-- Start spotify
				, ((mod4Mask .|. shiftMask, xK_s), spawnOn "9" "spotify")

				-- Start visual studio code
				, ((mod4Mask, xK_v), spawnOn "2" "code")

				-- Start discord
				, ((mod4Mask, xK_d), spawnOn "8" "discord")
				]

-----------------------------------------------------------------------
main = do
--	xmproc <- spawn "xfce4-panel"
    xmonad $ xfceConfig
		{ modMask = mod4Mask
		, terminal = "xfce4-terminal"
		, startupHook = myStartupHook <+> startupHook xfceConfig
		, manageHook = myManageHook
		, normalBorderColor = myNormalBorderColor
		, focusedBorderColor = myFocusedBorderColor
		--, workspaces = myWorkspaces
		, mouseBindings = myMouseBindings <+> mouseBindings xfceConfig
		} `additionalKeys` myKeyBindings
