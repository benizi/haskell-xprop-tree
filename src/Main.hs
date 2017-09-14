module Main where

import qualified Control.Exception as Ex
import Control.Exception.Safe (handleAny, throw)
import Control.Monad (forM_, unless)
import Data.List (intersperse)
import Data.Maybe (catMaybes, fromMaybe)
import Graphics.X11.Xlib
  ( Atom
  , Display
  , defaultScreen
  , openDisplay
  , rootWindow
  )
import qualified Graphics.X11.Xlib.Atom as Atom
import Graphics.X11.Xlib.Atom (getAtomName, internAtom)
import Graphics.X11.Xlib.Extras
  ( TextProperty
  , tp_encoding
  , tp_format
  , getTextProperty
  , getWindowProperty32
  , queryTree
  , rawGetWindowProperty
  , wcTextPropertyToTextList
  , xSetErrorHandler
  )
import Graphics.X11.Types (Window)
import Numeric (showHex)
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

-- |UnhandledFormat is for debugging issues with @TextProperty@ encodings.
data UnhandledFormat = UnhandledFormat String
  deriving Show

instance Ex.Exception UnhandledFormat

-- |Exception handler that discards anything other than format errors.
debugFormats :: Ex.SomeException -> IO [String]
debugFormats e = return $
  case Ex.fromException e of
    Just (UnhandledFormat _) -> [show e]
    _ -> []

-- |Returns a list of strings for the given property (empty if not found).
stringPropList :: Display -> Atom -> Window -> IO [String]
stringPropList d a w = do
  handleAny debugFormats $ getTextProperty d w a >>= \p -> do
    let enc = tp_encoding p
    name <- fromMaybe (show enc) <$> getAtomName d enc
    unpackProperty name enc p
      where
        isNumeric :: String -> Atom -> Bool
        isNumeric name atom
          | atom == Atom.cARDINAL = True
          | atom == Atom.iNTEGER = True
          | name == "TIMESTAMP" = True
          | otherwise = False

        isStringlike :: String -> Atom -> Bool
        isStringlike name atom
          | atom == Atom.sTRING = True
          | name == "COMPOUND_TEXT" = True
          | name == "UTF8_STRING" = True
          | otherwise = False

        unpackProperty :: String -> Atom -> TextProperty -> IO [String]
        unpackProperty name enc p
          | enc == Atom.aTOM = do
              atoms <- fromMaybe mempty <$> rawGetWindowProperty 32 d a w
              catMaybes <$> mapM (getAtomName d) atoms
          | isStringlike name enc = wcTextPropertyToTextList d p
          | isNumeric name enc = do
              nums <- fromMaybe mempty <$> getWindowProperty32 d a w
              return $ show <$> nums
          | enc == Atom.wINDOW = do
              wins <- fromMaybe mempty <$> getWindowProperty32 d a w
              return $ (\n -> "0x" ++ showHex n "") <$> wins
          | otherwise = let message = concat [name, ":", show $ tp_format p]
                         in throw $ Ex.toException $ UnhandledFormat message

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
  prop <- internAtom dpy propname False
  forM_ ws $ \win -> do
    vals <- filter (/= "") <$> stringPropList dpy prop win
    unless (null vals) $ do
      putStrLn $ mconcat . intersperse "\t" $ ["0x" ++ showHex win ""] ++ vals
