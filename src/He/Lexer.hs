
module He.Lexer
  ( lowerAlphas
  , upperAlphas
  , alphas
  , digits
  , underscore
  , Token
  , tokenData
  , TokenType(..)
  , TokenData(..)
  , IdClass
  , LexerSpec(..)
  , IdSpec(..)
  , StringSpec(..)
  , CommentSpec(..)
  , Tokens
  , tokenize
  ) where

import Control.Lens
import qualified Data.List as L
import H.Prelude
import Filesystem.Path.CurrentOS hiding (empty, null)
import Text.Parsec.Applicative hiding (Parser)
import Text.Regex.Applicative

import He.Error
import He.Lexer.Tokens
import He.Lexer.Types

lowerAlphas, upperAlphas, alphas, digits, underscore :: [Char]

lowerAlphas = ['a' .. 'z']

upperAlphas = ['A' .. 'Z']

alphas = lowerAlphas <> upperAlphas

digits = ['0' .. '9']

underscore = ['_']

tokenize
  :: (MonadError Error m, IdClass a)
   => LexerSpec a -> FilePath -> Text -> m (Tokens a)
tokenize spec name xs = case runState (file spec) i of
  (xs, ts)
    | not . null $ ts ^. tsInput
      -> throwError $ err (Just $ ts ^. tsSourcePos) "Unexpected input"
    | curMode (ts ^. tsModeStack) /= LMNormal
      -> throwError $ err' "Unexpected end of input"
    | otherwise
      -> return xs
  where
    i = emptyTokenizerState (encode name) xs

getCurMode :: TokT LexerMode
getCurMode = curMode <$> use tsModeStack

file :: (IdClass a) => LexerSpec a -> TokT (Tokens a)
file spec = skip >> (sequenceWhileJust . L.repeat) (oneToken spec)

skip :: TokT ()
skip = void $ getCurMode >>= withPos . skippable

oneToken
  :: (IdClass a)
  => LexerSpec a -> TokT (Maybe (Token a))
oneToken spec = do
  getCurMode >>= withPos . flip tok spec >>= \case
    Nothing -> return Nothing
    Just (((tt, td), as), pos) -> do
      void $ tsModeStack %= flip (foldl modeAction) as
      skip
      return $ Just (tt, WithSourcePos td pos)

withPos :: Parser a -> TokT (Maybe (a, SourcePos))
withPos p = do
  pos <- use tsSourcePos
  findLongestPrefix (withMatched p) <$> use tsInput >>= \case
    Nothing -> return Nothing
    Just ((val, matched), rest) -> do
      assign tsInput rest
      tsSourcePos %= flip updatePosString matched
      return $ Just (val, pos)

modeAction :: [LexerMode] -> LexerModeAction -> [LexerMode]
modeAction ms (Push m) = m : ms
modeAction [] (Pop _) = error "Empty mode stack"
modeAction (m : ms) (Pop m')
  | m == m' = ms
  | otherwise = error "Popped the wrong mode"

alts :: [TokenParser a] -> TokenParser a
alts = foldr (\a b spec -> a spec <|> b spec) (const empty)

normalToks :: (IdClass a) => TokenParser a
normalToks =
  alts
  [ keywords
  , litBool
  , litFloat
  , litInt
  , litChar
  , ident
  , beginString
  , beginBlockComment
  , beginLineComment
  ]

tok :: (IdClass a) => LexerMode -> TokenParser a
tok LMNormal = normalToks
tok LMString = alts [endString, beginInterp, stringContent]
tok LMInterp = alts [endInterp, beginExtraDelim, normalToks]
tok LMInterpExtraDelim = alts [endExtraDelim, normalToks]
tok LMBlockComment = alts [beginBlockComment, endBlockComment, blockCommentContent]
tok LMLineComment = alts [endLineComment, lineCommentContent]

skippable :: LexerMode -> Parser ()
skippable LMNormal = spaces
skippable LMString = pure ()
skippable LMInterp = spaces
skippable LMInterpExtraDelim = spaces
skippable LMBlockComment = pure ()
skippable LMLineComment = pure ()

