
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
  , StringSpec(..)
  , CommentSpec(..)
  , Tokens
  , tokenize
  ) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import H.Prelude
import Filesystem.Path.CurrentOS (encode)
import Text.Parsec.Applicative hiding (Parser)
import Text.Regex.Applicative

import He.Lexer.Tokens
import He.Lexer.Types
import He.Monad

lowerAlphas, upperAlphas, alphas, digits, underscore :: Set Char

lowerAlphas = S.fromList ['a' .. 'z']

upperAlphas = S.fromList ['A' .. 'Z']

alphas = lowerAlphas <> upperAlphas

digits = S.fromList ['0' .. '9']

underscore = S.singleton '_'

tokenize
  :: (IdClass a)
  => LexerSpec a -> FileMap Text -> MT (FileMap (Tokens a))
tokenize = (sequence .) . (M.mapWithKey . tokenizeFile)

tokenizeFile
  :: (IdClass a)
   => LexerSpec a -> FilePath -> Text -> MT (Tokens a)
tokenizeFile spec name xs = runStateT (file spec) i >>= \case
  (xs, ts)
    | not . null $ tsInput ^$ ts
      -> fatal' . Err ELexer Nothing Nothing . Just . show $ (tsSourcePos ^$ ts, xs)
    | curMode (tsModeStack ^$ ts) /= LMNormal
      -> fatal' . Err ELexer Nothing Nothing . Just . show $ (tsModeStack ^$ ts, xs)
    | otherwise
      -> return xs
  where
    i = emptyTokenizerState (encode name) xs

getCurMode :: TokT LexerMode
getCurMode = curMode <$> access tsModeStack

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
  pos <- access tsSourcePos
  findLongestPrefix (withMatched p) <$> access tsInput >>= \case
    Nothing -> return Nothing
    Just ((val, matched), rest) -> do
      void $ tsInput ~= rest
      void $ tsSourcePos %= flip updatePosString matched
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
skippable LMString = pure ()
skippable _ = spaces

