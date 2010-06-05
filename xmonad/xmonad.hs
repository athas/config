import qualified Data.Map as M

import Control.Arrow
import Data.Bits
import Data.List
import Data.Maybe

import Foreign.Storable

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Actions.GridSelect
import XMonad.Actions.Submap
import XMonad.Hooks.DynamicLog
import XMonad.Util.NamedWindows

main = xmonad =<< statusBar "xmobar" athasPP toggleStrutsKey myConfig
       
myConfig = defaultConfig {
             modMask = mod4Mask
           , focusFollowsMouse = False
           , terminal = "urxvt"
           , keys = newKeys
           }

athasPP :: PP
athasPP = xmobarPP { ppCurrent = xmobarColor "white" "black"
                   , ppTitle = xmobarColor "#111111" "" . shorten 120
                   }

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )
       
prefix = (controlMask, xK_t)

gsConfig = gsmenuGSConfig

prefixMap conf =
    [((m , k), windows $ f i)
     | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
     , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [ ((0, xK_k), kill)
    , ((shiftMask, xK_k), withFocused $ forceKill)
    , ((0, xK_c), spawn $ XMonad.terminal conf)
    , ((0, xK_f), withFocused $ windows . W.sink)
    , ((0, xK_t), withFocused $ sendPress prefix)
    , ((controlMask, xK_t), goToSelected gsConfig)
    , ((0, xK_b), goToSelected gsConfig)
    , ((0, xK_s), spawn "ogg123 /home/athas/sadtrombone.ogg")
    , ((0, xK_v), spawn "ogg123 /home/athas/saddestviolin.ogg")
    , ((shiftMask, xK_e), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    , ((controlMask, xK_r), spawn "surfsession resume")
    , ((0, xK_g), spawn "surf") ]

forceKill :: Window -> X ()
forceKill w = withDisplay $ \d -> io $ do
  protocols <- getWMProtocols d w
  killClient d w >> return ()

newKeys x = M.insert prefix mappings $ keys defaultConfig x
    where stdmap = M.mapKeys (first rmMod) $ keys defaultConfig x
          rmMod m = m .&. complement (modMask x)
          mappings = submap $ M.union (M.fromList $ prefixMap x) stdmap

sendPress :: (ButtonMask, KeySym) -> Window -> X ()
sendPress (km, ks) w = withDisplay $ \d -> do
  root <- asks theRoot
  io $ allocaXEvent $ \ev -> do
    kc <- keysymToKeycode d ks
    setKeyEvent ev w root none km kc True
    pokeByteOff ev 0 keyPress
    sendEvent d w False keyPressMask ev
    sync d False
