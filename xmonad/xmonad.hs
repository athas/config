import qualified Data.Map as M

import Control.Arrow
import Data.Bits
import Data.List
import Data.Maybe

import Foreign.Storable

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Actions.GridSelect hiding (gridselectWindow,
                                         withSelectedWindow,
                                         goToSelected)
import XMonad.Actions.Submap
import XMonad.Util.NamedWindows

main = xmonad defaultConfig {
         modMask = mod4Mask
       , focusFollowsMouse = False
       , terminal = "urxvt"
       , keys = newKeys
       }
       
prefix = (controlMask, xK_t)

gsConfig = defaultGSConfig { gs_selectfun = gsmenu 
                           , gs_originFractX = 0.5 }

gridselectWindow :: GSConfig Window -> X (Maybe Window)
gridselectWindow gsconf = do
  focus <- gets (W.peek . windowset)
  wm    <- windowMap
  case focus of
    Just fw -> gridselect gsconf $ filter ((/=fw) . snd) wm
    _       -> gridselect gsconf wm

withSelectedWindow :: (Window -> X ()) -> GSConfig Window -> X ()
withSelectedWindow callback conf = do
    mbWindow <- gridselectWindow conf
    case mbWindow of
        Just w -> callback w
        Nothing -> return ()

goToSelected :: GSConfig Window -> X ()
goToSelected = withSelectedWindow $ windows . W.focusWindow

windowMap :: X [(String,Window)]
windowMap = do
    ws <- gets windowset
    wins <- mapM keyValuePair (W.allWindows ws)
    return wins
 where keyValuePair w = flip (,) w `fmap` decorateName' w
       
decorateName' :: Window -> X String
decorateName' w = do
  fmap show $ getName w
       
prefixMap conf =
    [((m , k), windows $ f i)
     | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
     , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [ ((0, xK_k), kill)
    , ((0, xK_c), spawn $ XMonad.terminal conf)
    , ((0, xK_f), withFocused $ windows . W.sink)
    , ((0, xK_t), withFocused $ sendPress prefix)
    , ((controlMask, xK_t), goToSelected gsConfig)
    , ((0, xK_b), goToSelected gsConfig)
    , ((0, xK_s), spawn "ogg123 /home/athas/sadtrombone.ogg")
    , ((0, xK_v), spawn "ogg123 /home/athas/saddestviolin.ogg")
    , ((shiftMask, xK_e), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    , ((0, xK_g), spawn "surf") ]

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
