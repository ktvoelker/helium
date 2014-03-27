
module He.Parser where

import qualified Data.Map as M
import H.Common
import Text.Parsec.Applicative hiding (Parser, parse)
import qualified Text.Parsec.Applicative as P

import He.Annotation
import He.Lexer (TokenType(..), TokenData(..), IdClass(), Tokens, tokenData)
import He.Monad

type Parser s a = P.Parser s (TokenType a) (WithSourcePos TokenData)

instance SourcePosEffect (Parser s a) where
  getSourcePos = getPosition

parse
  :: (Eq a)
  => Parser s a b
  -> FileMap (Tokens a)
  -> MT (FileMap b)
parse = (sequence .) . (M.mapWithKey . parseFile)

parseFile
  :: (Eq a)
  => Parser s a b
  -> FilePath
  -> Tokens a
  -> MT b
parseFile file _ xs = case P.parse file xs of
  Left err -> fatal' . Err EParser Nothing Nothing . Just . show $ err
  Right decl -> return decl

delimit :: (IdClass a) => Text -> Text -> Parser s a b -> Parser s a b
delimit ld rd = between (kw ld) (kw rd)

kw :: (IdClass a) => Text -> Parser s a ()
kw = tok . Keyword

litInt :: (IdClass a) => Parser s a Integer
litInt = intData . tokenData <$> token LitInt

litFloat :: (IdClass a) => Parser s a Rational
litFloat = floatData . tokenData <$> token LitFloat

litChar :: (IdClass a) => Parser s a Char
litChar = charData . tokenData <$> token LitChar

litBool :: (IdClass a) => Parser s a Bool
litBool = boolData . tokenData <$> token LitBool

identifier :: (IdClass a) => a -> Parser s a Text
identifier = (textData . tokenData <$>) . token . Identifier

anyIdentifier :: (IdClass a) => Parser s a (a, Text)
anyIdentifier = fmap f . choice $ map (token . Identifier) [minBound .. maxBound]
  where
    f (Identifier cls, (wspValue ^$) -> TextData name) = (cls, name)
    f _ = undefined

tok :: (Eq a) => TokenType a -> Parser s a ()
tok = (f <$>) . token
  where
    f (_, (wspValue ^$) -> NoData) = ()
    f _ = undefined

beginString :: (IdClass a) => Parser s a ()
beginString = tok BeginString

endString :: (IdClass a) => Parser s a ()
endString = tok EndString

stringContent :: (IdClass a) => Parser s a Text
stringContent = textData . tokenData <$> token StringContent

beginInterp :: (IdClass a) => Parser s a ()
beginInterp = tok BeginInterp

endInterp :: (IdClass a) => Parser s a ()
endInterp = tok EndInterp

beginComment :: (IdClass a) => Parser s a ()
beginComment = tok BeginComment

commentContent :: (IdClass a) => Parser s a Text
commentContent = textData . tokenData <$> token CommentContent

endComment :: (IdClass a) => Parser s a ()
endComment = tok EndComment

