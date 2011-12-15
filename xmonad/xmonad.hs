{-# LANGUAGE TypeSynonymInstances #-}
import qualified Data.Map as M

import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Maybe

import Foreign.Storable

import Codec.Binary.UTF8.String

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWindows
import GSMenuPick
import XMonad.Actions.Submap
import XMonad.Actions.TagWindows
import XMonad.Hooks.DynamicLog
import XMonad.Util.NamedWindows

main :: IO ()
main = xmonad =<< statusBar "xmobar" athasPP toggleStrutsKey myConfig

myConfig :: XConfig (Choose Tall (Choose (Mirror Tall) Full))
myConfig = defaultConfig {
             modMask = mod4Mask
           , focusFollowsMouse = False
           , borderWidth = 1
           , terminal = "urxvt"
           , keys = newKeys
           }

athasPP :: PP
athasPP = xmobarPP { ppCurrent = xmobarColor "white" "black"
                   , ppTitle = xmobarColor "#111111" "" . shorten 120
                   }

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )

prefix :: (KeyMask, KeySym)
prefix = (controlMask, xK_t)

utf8StringProperty :: String -> Query String
utf8StringProperty p = ask >>= (\w -> liftX $ withDisplay $ \d -> fmap (fromMaybe "") $ getUtf8StringProperty d w p)

getUtf8StringProperty :: Display -> Window -> String -> X (Maybe String)
getUtf8StringProperty d w p = do
  a  <- getAtom p
  md <- io $ getWindowProperty8 d a w
  return $ fmap (decode . map fromIntegral) md

class AthasTags a where
  athasTags :: a -> X [String]
  athasTags _ = return []

instance AthasTags Window where
  athasTags w = do cn <- runQuery className w
                   liftM2 (++) (progTags cn) ((cn:) <$> getTags w)
    where progTags "grani" = (:[]) <$> drop 7 <$> runQuery (utf8StringProperty "_GRANI_URI") w
          progTags _ = return []

gsConfig :: GSConfig Window
gsConfig = defaultGSConfig { gs_tags = athasTags }

windowMap :: X [(String,Window)]
windowMap = do
    ws <- gets windowset
    wins <- mapM keyValuePair (W.allWindows ws)
    return wins
 where keyValuePair w = flip (,) w `fmap` decorateName' w

decorateName' :: Window -> X String
decorateName' w = do
  fmap show $ getName w

gridselectValue :: X [(String, a)] -> GSConfig a -> X (Maybe a)
gridselectValue m gsconf = m >>= gridselect gsconf

withSelectedValue :: X [(String, a)] -> (a -> X ())
                  -> GSConfig a -> X ()
withSelectedValue m callback conf = do
  maybe (return ()) callback =<< gridselectValue m conf

similarToWinMap :: Window -> X [(String,Window)]
similarToWinMap w = do
  cn <- runQuery className w
  filterM (liftM (==cn) . runQuery className . snd) =<< windowMap

goToSelectedFrom :: X [(String,Window)] -> GSConfig Window -> X ()
goToSelectedFrom m = withSelectedValue m $ windows . W.focusWindow

prefixMap :: XConfig l -> [((KeyMask, KeySym), X ())]
prefixMap conf =
    [((m , k), windows $ f i)
     | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
     , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [ ((0, xK_k), kill)
    , ((0, xK_BackSpace), spawn "swarp 20000 20000")
    , ((shiftMask, xK_k), withFocused $ forceKill)
    , ((0, xK_c), spawn $ XMonad.terminal conf)
    , ((0, xK_f), withFocused $ windows . W.sink)
    , ((0, xK_t), withFocused $ sendPress prefix)
    , ((0, xK_b), goToSelected gsConfig)
    , ((controlMask, xK_b), withFocused $ \w ->
        goToSelectedFrom (similarToWinMap w) gsConfig)
    , ((controlMask, xK_t), cycleRecentWindows [xK_Control_L] xK_t xK_t)
    , ((0, xK_s), spawn "ogg123 /home/athas/sadtrombone.ogg")
    , ((0, xK_v), spawn "ogg123 /home/athas/saddestviolin.ogg")
    , ((shiftMask, xK_e), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    , ((controlMask, xK_r), spawn "grani-session resume")
    , ((0, xK_g), spawn "url=\"$(grani-field)\" && grani \"$url\"")
    , ((0, xK_e), spawn "file=\"$(filesel)\" && emacsclient -n \"$file\"") ]

forceKill :: Window -> X ()
forceKill w = withDisplay $ \d -> io $ do
  killClient d w >> return ()

newKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
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
