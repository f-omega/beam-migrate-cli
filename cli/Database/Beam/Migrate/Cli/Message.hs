{-# LANGUAGE OverloadedStrings #-}

module Database.Beam.Migrate.Cli.Message
    ( module Database.Beam.Migrate.Cli.Message
    , module Prettyprinter ) where

import Database.Beam.Migrate.Cli.Engine.Internal
import Database.Beam.Migrate.Cli.Registry

import Prettyprinter hiding (Pretty(..))
import Prettyprinter.Render.Terminal
import Prettyprinter.Render.String (renderString)

import Control.Exception (Exception, SomeException (..))

import Data.String (fromString)
import Data.Time (LocalTime)
import Data.Text (Text, unpack)
import Data.Int (Int32)

import System.IO (hIsTerminalDevice, Handle, hPutStr, stdout)
import Database.Beam.Migrate.Backend (DdlError)

type Message = Doc AnsiStyle

success, info, errorMessage, warning :: Message -> Message
success = annotate (color Green)
errorMessage = annotate (color Red)
warning = annotate (color Yellow)
info = id --annotate (color White)

dullRed, dullYellow, dullGreen, dullBlue, red, yellow, green, blue :: Message -> Message
dullBlue = annotate (colorDull Blue)
dullYellow = annotate (colorDull Yellow)
dullRed = annotate (colorDull Red)
dullGreen = annotate (colorDull Green)
blue = annotate (color Blue)
green = annotate (color Green)
yellow = annotate (color Yellow)
red = annotate (color Red)

formatExeName :: Message -> Message
formatExeName = annotate (colorDull Green)

bolden :: Message -> Message
bolden = annotate bold

status :: Message -> Message
status = annotate bold

filename :: String -> Message
filename = annotate bold . fromString

hPutMessage :: Handle -> Message -> IO ()
hPutMessage hdl msg = do
  isTerminal <- hIsTerminalDevice hdl
  if isTerminal then hPutDoc hdl msg
     else hPutStr hdl (renderString (layoutSmart defaultLayoutOptions (unAnnotate msg)))

hPutMessageLn :: Handle -> Message -> IO ()
hPutMessageLn hdl msg = hPutMessage hdl (msg <> "\n")

putMessage :: Message -> IO ()
putMessage = hPutMessage stdout

putMessageLn = hPutMessageLn stdout

fromShow :: Show a => a -> Message
fromShow = fromString . show

text :: Text -> Message
text = fromString . Data.Text.unpack

class Pretty a where
    pretty :: a -> Message
    prettyList :: [a] -> Message
    prettyList xs = "[" <> mconcat (punctuate ", " (map pretty xs)) <> "]"

instance Pretty Char where
    pretty = fromShow
    prettyList = fromString

instance Pretty Text where
    pretty = fromString . Data.Text.unpack

instance Pretty SomeException where
    pretty (SomeException e) = annotate (colorDull Red) (fromString (show e))

instance Pretty MigrationName where
    pretty (MigrationName t) = annotate (color Magenta <> bold) (pretty t)

instance Pretty BranchName where
    pretty (BranchName t) = annotate (color Blue <> bold) (pretty t)

instance Pretty LocalTime where
    pretty t = annotate bold (fromString . show $ t)

instance Pretty Int where
    pretty = fromShow

instance Pretty Int32 where
    pretty = fromShow

instance Pretty x => Pretty (Maybe x) where
    pretty Nothing = info "None"
    pretty (Just x) = pretty x

instance Pretty x => Pretty [x] where
    pretty = prettyList

newtype BeamMigrateError = BeamMigrateError Message
    deriving Show
    deriving anyclass Exception
