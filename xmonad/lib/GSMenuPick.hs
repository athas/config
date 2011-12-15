{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

module GSMenuPick (
    -- * Usage
    -- $usage

    -- ** Customizing
    -- *** Using a common GSConfig
    -- $commonGSConfig

    -- *** Custom keybindings
    -- $keybindings

    -- * Configuration
    GSConfig(..),
    Response,
    inXmonad,
    inGsmenu,
    defaultGSConfig,
    buildDefaultGSConfig,

    -- * Variations on 'gridselect'
    gridselect,
    gsmenu,
    gridselectWindow,
    withSelectedWindow,
    bringSelected,
    goToSelected,
    gridselectWorkspace,
    spawnSelected,
    runSelectedAction,

    -- * Colorizers
    HasColorizer(defaultColorizer, defaultActions),
    fromClassName,
    stringColorizer,
    colorRangeFromClassName,
    ) where
import Control.Monad.State
import Data.Char
import Data.Either
import Data.List as L
import Data.Maybe
import Data.Word (Word8)
import System.Exit
import System.IO
import System.Process
import System.Random (mkStdGen, genRange, next)
import Text.Printf
import XMonad hiding (liftX)
import XMonad.Actions.TagWindows
import XMonad.Actions.WindowBringer (bringWindow)
import XMonad.StackSet as W
import XMonad.Util.Font
import XMonad.Util.NamedWindows
import qualified Data.Map as M

-- $usage
--
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- >    import GSMenuPick
--
-- Then add a keybinding, e.g.
--
-- >    , ((modm, xK_g), goToSelected defaultGSConfig)
--
-- This module also supports displaying arbitrary information in a grid and letting
-- the user select from it. E.g. to spawn an application from a given list, you
-- can use the following:
--
-- >   , ((modm, xK_s), spawnSelected defaultGSConfig ["xterm","gmplayer","gvim"])

-- $commonGSConfig
--
-- It is possible to bind a @gsconfig@ at top-level in your configuration. Like so:
--
-- > -- the top of your config
-- > {-# LANGUAGE NoMonomorphismRestriction #-}
-- > import XMonad
-- > ...
-- > gsconfig1 = defaultGSConfig { gs_cellheight = 30, gs_cellwidth = 100 }
--
-- An example where 'buildDefaultGSConfig' is used instead of 'defaultGSConfig'
-- in order to specify a custom colorizer is @gsconfig2@ (found in
-- "XMonad.Actions.GridSelect#Colorizers"):
--
-- > gsconfig2 colorizer = (buildDefaultGSConfig colorizer) { gs_cellheight = 30, gs_cellwidth = 100 }
--
-- > -- | A green monochrome colorizer based on window class
-- > greenColorizer = colorRangeFromClassName
-- >                      black            -- lowest inactive bg
-- >                      (0x70,0xFF,0x70) -- highest inactive bg
-- >                      black            -- active bg
-- >                      white            -- inactive fg
-- >                      white            -- active fg
-- >   where black = minBound
-- >         white = maxBound
--
-- Then you can bind to:
--
-- >     ,((modm, xK_g), goToSelected  $ gsconfig2 myWinColorizer)
-- >     ,((modm, xK_p), spawnSelected $ spawnSelected defaultColorizer)

data GSConfig a = GSConfig {
    gs_cellheight :: Integer
  , gs_cellwidth :: Integer
  , gs_cellpadding :: Integer
  , gs_colorizer :: a -> Bool -> X (String, String)
  , gs_tags :: a -> X [String]
  , gs_font :: String
  , gs_originFractX :: Double
  , gs_originFractY :: Double
  , gs_extension :: String
  , gs_actions :: [(String, Response a)]
}

type Response a = (String, a -> String -> X (Maybe a))

inGsmenu :: String -> Response a
inGsmenu s = (s, \_ _ -> return Nothing)

inXmonad :: (a -> String -> X (Maybe a)) -> Response a
inXmonad f = ("", f)

-- | That is 'fromClassName' if you are selecting a 'Window', or
-- 'defaultColorizer' if you are selecting a 'String'. The catch-all instance
-- @HasColorizer a@ uses the 'focusedBorderColor' and 'normalBorderColor'
-- colors.
class HasColorizer a where
    defaultColorizer :: a -> Bool -> X (String, String)
    defaultTags :: a -> X [String]
    defaultTags _ = return []
    defaultActions :: [(String, Response a)]
    defaultActions = []

instance HasColorizer Window where
    defaultColorizer = fromClassName
    defaultTags w = liftM2 (:) (runQuery className w) (getTags w)
    defaultActions = [( "<C-d>"
                      , ( "grid.removeValue(grid.selected[\"value\"])"
                        , \win _ -> killWindow win >> return Nothing))]

instance HasColorizer String where
    defaultColorizer = stringColorizer

instance HasColorizer a where
    defaultColorizer _ isFg =
        let getColor = if isFg then focusedBorderColor else normalBorderColor
        in asks $ flip (,) "black" . getColor . config

-- | A basic configuration for 'gridselect', with the colorizer chosen based on the type.
--
-- If you want to replace the 'gs_colorizer' field, use 'buildDefaultGSConfig'
-- instead, to avoid ambiguous type variables.
defaultGSConfig :: HasColorizer a => GSConfig a
defaultGSConfig = buildDefaultGSConfig defaultColorizer defaultTags defaultActions

-- FIXME probably move that into Utils?
-- Conversion scheme as in http://en.wikipedia.org/wiki/HSV_color_space
hsv2rgb :: Fractional a => (Integer,a,a) -> (a,a,a)
hsv2rgb (h,s,v) =
    let hi = div h 60 `mod` 6 :: Integer
        f = ((fromInteger h/60) - fromInteger hi)
        q = v * (1-f)
        p = v * (1-s)
        t = v * (1-(1-f)*s)
    in case hi of
         0 -> (v,t,p)
         1 -> (q,v,p)
         2 -> (p,v,t)
         3 -> (p,q,v)
         4 -> (t,p,v)
         5 -> (v,p,q)
         _ -> error "The world is ending. x mod a >= a."

-- | Default colorizer for Strings
stringColorizer :: String -> Bool -> X (String, String)
stringColorizer s active =
    let seed x = toInteger (sum $ map ((*x).fromEnum) s) :: Integer
        (r,g,b) = hsv2rgb (seed 83 `mod` 360,
                           fromInteger (seed 191 `mod` 1000)/2500+0.4,
                           fromInteger (seed 121 `mod` 1000)/2500+0.4)
    in if active
         then return ("#faff69", "black")
         else return ('#' : concatMap (twodigitHex.(round :: Double -> Word8).(*256)) [r, g, b], "white")

-- | Colorize a window depending on its className.
fromClassName :: Window -> Bool -> X (String, String)
fromClassName w active = runQuery className w >>= flip defaultColorizer active

twodigitHex :: Word8 -> String
twodigitHex = printf "%02x"

-- | A colorizer that picks a color inside a range,
-- and depending on the window's class.
colorRangeFromClassName :: (Word8, Word8, Word8) -- ^ Beginning of the color range
                        -> (Word8, Word8, Word8) -- ^ End of the color range
                        -> (Word8, Word8, Word8) -- ^ Background of the active window
                        -> (Word8, Word8, Word8) -- ^ Inactive text color
                        -> (Word8, Word8, Word8) -- ^ Active text color
                        -> Window -> Bool -> X (String, String)
colorRangeFromClassName startC endC activeC inactiveT activeT w active =
    do classname <- runQuery className w
       if active
         then return (rgbToHex activeC, rgbToHex activeT)
         else return (rgbToHex $ mix startC endC
                  $ stringToRatio classname, rgbToHex inactiveT)
    where rgbToHex :: (Word8, Word8, Word8) -> String
          rgbToHex (r, g, b) = '#':twodigitHex r
                               ++twodigitHex g++twodigitHex b

-- | Creates a mix of two colors according to a ratio
-- (1 -> first color, 0 -> second color).
mix :: (Word8, Word8, Word8) -> (Word8, Word8, Word8)
        -> Double -> (Word8, Word8, Word8)
mix (r1, g1, b1) (r2, g2, b2) r = (mix' r1 r2, mix' g1 g2, mix' b1 b2)
    where  mix' a b = truncate $ (fi a * r) + (fi b * (1 - r))

-- | Generates a Double from a string, trying to
-- achieve a random distribution.
-- We create a random seed from the sum of all characters
-- in the string, and use it to generate a ratio between 0 and 1
stringToRatio :: String -> Double
stringToRatio "" = 0
stringToRatio s = let gen = mkStdGen $ sum $ map fromEnum s
                      range = (\(a, b) -> b - a) $ genRange gen
                      randomInt = foldr1 combine $ replicate 20 next
                      combine f1 f2 g = let (_, g') = f1 g in f2 g'
                  in fi (fst $ randomInt gen) / fi range

gridselect :: GSConfig a -> [(String,a)] -> X (Maybe a)
gridselect = gsmenu

gridselectWindow :: GSConfig Window -> X (Maybe Window)
gridselectWindow gsconfig = windowMap >>= gsmenu gsconfig

-- | Brings up a 2D grid of windows in the center of the screen, and one can
-- select a window with cursors keys. The selected window is then passed to
-- a callback function.
withSelectedWindow :: (Window -> X ()) -> GSConfig Window -> X ()
withSelectedWindow callback conf = do
    mbWindow <- gridselectWindow conf
    case mbWindow of
        Just w -> callback w
        Nothing -> return ()

windowMap :: X [(String,Window)]
windowMap = do ws <- gets windowset
               mapM keyValuePair (W.allWindows ws)
 where keyValuePair w = flip (,) w `fmap` decorateName' w

decorateName' :: Window -> X String
decorateName' w = fmap show $ getName w

-- | Builds a default gs config from a colorizer function.
buildDefaultGSConfig :: (a -> Bool -> X (String,String))
                     -> (a -> X [String]) -> [(String, Response a)] -> GSConfig a
buildDefaultGSConfig col tags acts =
  GSConfig 50 130 10 col tags "xft:Sans-8" (1/2) (1/2) ""
             (acts ++ [("<Return>", inXmonad $ \x _ -> return $ Just x)])

-- | Brings selected window to the current workspace.
bringSelected :: GSConfig Window -> X ()
bringSelected = withSelectedWindow $ \w -> do
    windows (bringWindow w)
    XMonad.focus w
    windows W.shiftMaster

-- | Switches to selected window's workspace and focuses that window.
goToSelected :: GSConfig Window -> X ()
goToSelected = withSelectedWindow $ windows . W.focusWindow

-- | Select an application to spawn from a given list
spawnSelected :: GSConfig String -> [String] -> X ()
spawnSelected conf lst = gridselect conf (zip lst lst) >>= flip whenJust spawn

-- | Select an action and run it in the X monad
runSelectedAction :: GSConfig (X ()) -> [(String, X ())] -> X ()
runSelectedAction conf actions = do
  selectedActionM <- gridselect conf actions
  fromMaybe (return ()) selectedActionM

-- | Select a workspace and view it using the given function
-- (normally 'W.view' or 'W.greedyView')
--
-- Another option is to shift the current window to the selected workspace:
--
-- > gridselectWorkspace (\ws -> W.greedyView ws . W.shift ws)
gridselectWorkspace :: GSConfig WorkspaceId ->
                          (WorkspaceId -> WindowSet -> WindowSet) -> X ()
gridselectWorkspace conf viewFunc = withWindowSet $ \ws -> do
  let wss = map W.tag $ W.hidden ws ++ map W.workspace (W.current ws : W.visible ws)
  gridselect conf (zip wss wss) >>= flip whenJust (windows . viewFunc)

mkKv :: GSConfig a -> Int -> (String, a) -> X String
mkKv gsconfig i (name, v) = do
  (bg, fg) <- gs_colorizer gsconfig v False
  ts       <- gs_tags gsconfig v
  return $ kvToStr [ ("name", name : ts), ("fg", [fg]), ("bg", [bg])
                   , ("tags", ts), ("value", [show i])]
  where kvToStr = unwords . map fieldToStr
        fieldToStr (_, []) = ""
        fieldToStr (k, vs) = k ++ "=" ++ unwords (map escape vs)
        escape s = "\"" ++ foldr esc "" s ++ "\""
        esc '"' s = "\"\"" ++ s
        esc c s    = c : s

-- | Brings up a 2D grid of elements in the center of the screen, and
-- one can select an element with cursors keys. The selected element
-- is returned.  Uses external gsmenu program.
gsmenu :: GSConfig a -> [(String,a)] -> X (Maybe a)
gsmenu gsconfig ellist = do
  withDisplay $ \dpy ->
    io $ do ungrabKeyboard dpy currentTime
            sync dpy False
  kvs <- zipWithM (mkKv gsconfig) [0..] ellist
  asChoice $ withGsmenu $ \input output pid -> do
    io $ hPutStr input $ unlines kvs
    interactWith output pid $ \l ->
      case reads l of
        [(i :: Int, s)] ->
          case liftM snd $ ifind i ellist of
            Just x -> let (command, s') = break isSpace $ dropWhile isSpace s
                      in do
                        io $ hPutStr stderr $ show (command, M.keys responses)
                        case M.lookup command responses of
                           Just r  -> r x (dropWhile isSpace s')
                           Nothing -> return Nothing
            Nothing -> return Nothing
        _ -> return Nothing
  where ifind 0 (x:_)  = Just x
        ifind n (_:xs) = ifind (n-1) xs
        ifind _ _      = Nothing
        asChoice = liftM (either (const Nothing) Just)
        withGsmenu = externalProgram "gsmenu"
                     [ "-e", gs_extension gsconfig
                     , "-e", unlines $ map (fst . snd) actions]
        responses = M.fromList $ zip (map fst actions) (map (snd . snd) actions)
        actions = zipWith action [0..] $ gs_actions gsconfig
        action (i::Int) (k, (ingsmenu, inxmonad)) =
          (command
          , (k ++ " { print grid.selected[\"value\"], \""
             ++ command ++ "\"; " ++ ingsmenu ++ "; next; }", inxmonad))
          where command ="internal__" ++ show i ++ "__"

spawnPipe2 :: MonadIO m => FilePath -> [String]
           -> m (Either Int (Handle, Handle, ProcessHandle))
spawnPipe2 prog args = do
  (Just intput, Just output, _, proc') <- 
    liftIO $ createProcess $ (proc prog args)
      { cwd       = Nothing
      , env       = Nothing
      , std_in    = CreatePipe
      , std_out   = CreatePipe
      , std_err   = CreatePipe
      , close_fds = True }
  code <- liftIO $ getProcessExitCode proc'
  case code of
    Just (ExitFailure e) -> return $ Left e
    _ -> do liftIO $ hSetBuffering intput NoBuffering
            return $ Right (intput, output, proc')

externalProgram :: MonadIO m => FilePath -> [String]
                -> (Handle -> Handle -> ProcessHandle -> m (Either ExitCode a))
                -> m (Either ExitCode a)
externalProgram prog args f = do
  p <- spawnPipe2 prog args
  case p of Left e   -> return $ Left $ ExitFailure e
            Right (input, output, pid) -> f input output pid

interactWith :: MonadIO m => Handle -> ProcessHandle
             -> (String -> m (Maybe a)) -> m (Either ExitCode a)
interactWith out pid f = loop
  where loop = do
          eof <- io $ hIsEOF out
          if not eof
            then do
              v <- f =<< io (hGetLine out)
              case v of Nothing -> loop
                        Just v' -> do
                          io $ terminateProcess pid
                          return $ Right v'
            else liftM Left $ io $ waitForProcess pid
