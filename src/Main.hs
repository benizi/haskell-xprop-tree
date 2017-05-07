module Main where

import qualified Control.Arrow as Arr
import qualified Control.Exception as Ex
import Control.Exception.Safe (handleAny)
import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.List (intersperse)
import Data.Maybe (fromJust, isJust)
import Graphics.X11.Xlib
  ( Atom
  , Display
  , defaultScreen
  , openDisplay
  , rootWindow
  )
import Graphics.X11.Xlib.Atom (internAtom)
import Graphics.X11.Xlib.Extras
  ( TextProperty
  , getTextProperty
  , getWindowProperty32
  , queryTree
  , wcTextPropertyToTextList
  , xSetErrorHandler
  )
import Graphics.X11.Types (Window)
import System.Environment (getArgs)

-- |Return all X11 windows in the root window's tree, including the root.
allWindows :: Display -> IO [Window]
allWindows dpy = do
  root <- rootWin dpy
  (_,_,windows) <- queryTree dpy root
  return (root:windows)

-- |Helper function to get the root window of the default screen of a Display.
rootWin :: Display -> IO Window
rootWin dpy = rootWindow dpy (defaultScreen dpy)

getPropList :: Display -> String -> Window -> IO [String]
getPropList dpy name w = do
  atom <- internAtom dpy name False
  prop <- Ex.handle empty $ getTextProperty dpy w atom >>= asTextList
  return prop
    where
      asTextList = wcTextPropertyToTextList dpy >>= return
      empty = mempty :: Ex.SomeException -> IO [String]

-- |Dumb data type to allow passing either an Atom or a String.
data StringAtom = AnAtom Atom
                | AString String

-- |Given a Display and a String or an Atom, resolve it to an X11 Atom.
resolve :: Display -> StringAtom -> IO Atom
resolve _ (AnAtom a) = return a
resolve d (AString s) = internAtom d s False

-- |Get the first String value of the given property for a Window.
getPropMaybeT :: Display -> StringAtom -> Window -> MaybeT IO String
getPropMaybeT dpy propID w = do
  atom <- liftIO $ resolve dpy propID
  prop <- handleAny (\_ -> mzero) $ liftIO $ getTextProperty dpy w atom
  liftIO $ wcTextPropertyToTextList dpy prop >>= return . head

-- |Same as getPropMaybeT, but unwrapped from MaybeT.
getPropMaybe :: Display -> StringAtom -> Window -> IO (Maybe String)
getPropMaybe d a w = runMaybeT $ getPropMaybeT d a w

-- |Return a list of tuples consisting of a Window and its value for the prop.
propPerWindow :: Display -> StringAtom -> [Window] -> IO [(Window,Maybe String)]
propPerWindow dpy propID windows = do
  props <- sequence $ getPropMaybe dpy propID <$> windows
  return $ zip windows props

-- |Same as propPerWindow, but limited to Windows that have a value for the
-- property.
justWindowProp :: Display -> StringAtom -> [Window] -> IO [(Window,String)]
justWindowProp d a ws = do
  wps <- propPerWindow d a ws
  return $ (Arr.second fromJust) <$> filter (isJust . snd) wps

-- |The default text property to fetch if none is passed as an argument.
defaultPropertyName :: String
defaultPropertyName = "WM_COMMAND"

-- |Find the value of a text property for every Window.
main :: IO ()
main = do
  xSetErrorHandler -- ignore all X11-related errors
  dpy <- openDisplay ""
  ws <- allWindows dpy
  args <- getArgs
  let propname = case args of (a:_) -> a ; [] -> defaultPropertyName
  wps <- justWindowProp dpy (AString propname) ws
  let join (win, val) = mconcat . intersperse "\t" $ [(show win), val]
  mapM_ (putStrLn . join) wps
