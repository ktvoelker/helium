
module He.Error
  ( module Control.Monad.Except
  , Error()
  , err
  , err'
  , report
  , check
  , checked
  , fatal
  , logExceptT
  , logExceptT'
  , log
  ) where

import Control.Lens
import Control.Monad.Except
import qualified Data.DList as D
import H.Prelude hiding ((<>), empty, show)
import Prelude (show)
import Text.Parsec.Applicative.Types
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass
import System.IO (hPutStr, hPutStrLn, stderr)

newtype Error = Error (D.DList (Maybe SourcePos, Doc))
  deriving (Eq, Monoid, Show)

vSepBy :: Doc -> Doc -> Doc -> Doc
vSepBy sep above below = above $+$ sep $+$ below

instance Pretty Error where
  pPrint (Error es) = case D.toList es of
    [] -> text "No errors"
    es -> foldl (vSepBy empty) empty $ map (uncurry prettyError) es

prettyError :: Maybe SourcePos -> Doc -> Doc
prettyError sp message = case sp of
  Nothing -> message
  Just sp -> prettySourcePos sp $+$ message

prettySourcePos :: SourcePos -> Doc
prettySourcePos sp =
  text "At " <> maybe empty (text . unpack) (sp ^. spName)
  <> text " line " <> text (show $ sp ^. spLine)
  <> text " column " <> text (show $ sp ^. spColumn)
  <> text ":"

err :: Maybe SourcePos -> Text -> Error
err sp xs = Error $ D.singleton (sp, text $ unpack xs)

err' :: Text -> Error
err' = err Nothing

report :: (MonadState (Maybe e) m, Monoid e) => e -> m () 
report e = modify (`mappend` Just e)

check :: (MonadState (Maybe e) m, MonadError e m) => m ()
check = get >>= maybe (return ()) ((put Nothing >>) . throwError)

checked :: (MonadState (Maybe e) m, MonadError e m) => m a -> m a
checked = (<* check)

fatal :: (MonadState (Maybe e) m, Monoid e, MonadError e m) => e -> m a
fatal e = do
  report e
  get >>= maybe (impossible "fatal") ((put Nothing >>) . throwError)

logExceptT :: (MonadIO m, Pretty e) => ExceptT e m a -> (a -> m ()) -> m ()
logExceptT e f =
  runExceptT e
  >>= either (liftIO . hPutStr stderr . render . pPrint) f

logExceptT' :: (MonadIO m, Pretty e) => ExceptT e m () -> m ()
logExceptT' e = logExceptT e return

log :: (MonadIO m) => Text -> m ()
log = liftIO . hPutStrLn stderr . unpack
