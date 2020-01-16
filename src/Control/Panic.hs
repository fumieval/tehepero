{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Panic
  (Panic(..)
  , PrettyEx(..)
  , panic
  , unwrap
  , expect
  , withPanic) where

import Control.Monad.Catch
import Data.Fallible
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import GHC.Stack
import System.IO (stderr)
import System.Exit (exitFailure)

data Panic = Panic !CallStack !(Doc AnsiStyle)
  deriving (Show)
instance Exception Panic

class PrettyEx a where
  prettyEx :: a -> Doc AnsiStyle
  prettyExList :: [a] -> Doc AnsiStyle
  prettyExList = foldMap prettyEx

instance (a ~ AnsiStyle) => PrettyEx (Doc a) where
  prettyEx = id

instance PrettyEx () where
  prettyEx _ = "Nothing"

instance PrettyEx Char where
  prettyEx = pretty
  prettyExList = pretty

instance PrettyEx a => PrettyEx [a] where
  prettyEx = annotate (color Cyan) . prettyExList

instance PrettyEx CallStack where
  prettyEx = vsep . map prettyCallSite . getCallStack where
    prettyCallSite (f, loc) = annotate bold (pretty f) <> ", called at " <> prettyEx loc

instance PrettyEx SrcLoc where
  prettyEx SrcLoc {..} = mconcat
      [ annotate (color Blue) (pretty srcLocFile), ":"
      , pretty srcLocStartLine, ":"
      , pretty srcLocStartCol, " in "
      , annotate (color Yellow) $ pretty srcLocPackage, ":", pretty srcLocModule
      ]

instance PrettyEx Panic where
  prettyEx (Panic stack doc) = vsep [annotate (color Red <> bold) "!!!Panic!!!", doc, prettyEx stack]

-- | Throw an error with a pretty-printable message.
panic :: (MonadThrow m, HasCallStack) => Doc AnsiStyle -> m a
panic = throwM . Panic (popCallStack callStack)
{-# INLINE panic #-}

-- | Try to obtain the result of a 'Fallible' value. If 'panic's it fails.
unwrap :: (Fallible f, PrettyEx (Failure f), MonadThrow m, HasCallStack)
  => f a -> m a
unwrap = either (panic . prettyEx) pure . tryFallible
{-# INLINE unwrap #-}

-- | Prepend a message if it fails to obtain the result.
expect :: (Fallible f, PrettyEx (Failure f), MonadThrow m, HasCallStack)
  => Doc AnsiStyle -> f a -> m a
expect e = either (panic . (prefix<>) . prettyEx) pure . tryFallible where
  prefix = e <> ": "
{-# INLINE expect #-}

-- | Add an exception handler for 'Panic' with prettifed output.
withPanic :: IO a -> IO a
withPanic m = m `catch` \e -> do
  hPutDoc stderr $ prettyEx (e :: Panic) <> hardline
  exitFailure