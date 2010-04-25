import qualified Data.Map as M

import Control.Arrow
import Data.Bits

import Foreign.Storable

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Actions.GSCompat
import XMonad.Actions.Submap

main = xmonad defaultConfig {
         modMask = mod4Mask
       , focusFollowsMouse = False
       , terminal = "urxvt"
       , keys = newKeys
       }
       
prefix = (controlMask, xK_t)
       
prefixMap conf =
    [((m , k), windows $ f i)
     | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
     , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [ ((0, xK_k), kill)
    , ((0, xK_c), spawn $ XMonad.terminal conf)
    , ((0, xK_f), withFocused $ windows . W.sink)
    , ((0, xK_t), withFocused $ sendPress prefix)
    , ((0, xK_b), goToSelected defaultGSConfig)
    , ((0, xK_s), spawn "ogg123 /home/athas/sadtrombone.ogg")
    , ((0, xK_v), spawn "ogg123 /home/athas/saddestviolin.ogg")
    , ((shiftMask, xK_e), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")]

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
