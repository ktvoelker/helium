
{-# LANGUAGE TemplateHaskell #-}
module He.Lexer.Types where

import Control.Lens
import qualified Data.List as L
import H.Prelude
import qualified Data.Text as T
import Prelude (Show(..))
import Text.Parsec.Applicative.Types
import Text.Regex.Applicative

type Parser = RE Char

choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

oneOf :: [Char] -> Parser Char
oneOf = choice . fmap sym

noneOf :: [Char] -> Parser Char
noneOf ps = psym $ not . (`elem` ps)

between :: Parser a -> Parser b -> Parser c -> Parser b
between l p r = l *> p <* r

option :: a -> Parser a -> Parser a
option def p = p <|> pure def

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

count :: Int -> Parser a -> Parser [a]
count n = sequenceA . L.replicate n

digit :: Parser Char
digit = oneOf ['0' .. '9']

hexDigit :: Parser Char
hexDigit = oneOf $ ['0' .. '9'] <> ['a' .. 'f'] <> ['A' .. 'F']

spaces :: Parser ()
spaces = many (psym isSpace) *> pure ()

optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe p = option Nothing (Just <$> p)

char :: Char -> Parser Char
char = sym

anyChar :: Parser Char
anyChar = anySym

data TokenType a =
    Keyword Text
  | Identifier a
  | LitChar
  | LitInt
  | LitFloat
  | LitBool
  | BeginString 
  | StringContent
  | EndString
  | BeginInterp
  | EndInterp
  | BeginComment
  | CommentContent
  | EndComment
  deriving (Eq, Ord)

instance (Show a) => Show (TokenType a) where
  showsPrec p = \case
    Keyword xs     -> ("'" <>) . (T.unpack xs <>) . ("'" <>)
    Identifier a   -> ("ID[" <>) . showsPrec p a . ("]" <>)
    LitChar        -> ("CHAR" <>)
    LitInt         -> ("INT" <>)
    LitFloat       -> ("FLOAT" <>)
    LitBool        -> ("BOOL" <>)
    BeginString    -> ("BEGIN-STR" <>)
    StringContent  -> ("STRING-CONTENT" <>)
    EndString      -> ("END-STR" <>)
    BeginInterp    -> ("BEGIN-INTERP" <>)
    EndInterp      -> ("END-INTERP" <>)
    BeginComment   -> ("BEGIN-COMMENT" <>)
    CommentContent -> ("COMMENT-CONTENT" <>)
    EndComment     -> ("END-COMMENT" <>)

data TokenData =
    NoData
  | TextData  { textData  :: Text     }
  | CharData  { charData  :: Char     }
  | IntData   { intData   :: Integer  }
  | FloatData { floatData :: Rational }
  | BoolData  { boolData  :: Bool     }
  deriving (Eq, Ord, Show)

type Token a = (TokenType a, WithSourcePos TokenData)

tokenData :: Token a -> TokenData
tokenData = (^. wspValue) . snd

type RawToken a = (TokenType a, TokenData)

class (Eq a, Ord a, Enum a, Bounded a, Show a) => IdClass a where

instance IdClass ()

data LexerSpec a =
  LexerSpec
  { sKeywords    :: [Text]
  , sIdentifiers :: [(a, Set Char, Set Char)]
  , sStrings     :: StringSpec
  , sInts        :: Bool
  , sNegative    :: Maybe Text
  , sFloats      :: Bool
  , sBools       :: Maybe (Text, Text)
  , sComments    :: CommentSpec
  } deriving (Eq, Ord, Show)

data StringSpec =
  StringSpec
  { sStringDelim :: Maybe Char
  , sCharDelim   :: Maybe Char
  , sInterpMany  :: Maybe (Char, Char)
  } deriving (Eq, Ord, Show)

data CommentSpec =
  CommentSpec
  { sLineComment  :: Maybe Text
  , sBlockComment :: Maybe (Text, Text)
  } deriving (Eq, Ord, Show)

type Tokens a = [Token a]

data LexerMode =
    LMNormal
  | LMString
  | LMInterp
  | LMInterpExtraDelim
  | LMBlockComment
  | LMLineComment
  deriving (Eq, Ord, Enum, Bounded, Show)

curMode :: [LexerMode] -> LexerMode
curMode = maybe LMNormal id . listToMaybe

emptyModeStack :: [LexerMode]
emptyModeStack = []

data LexerModeAction = Pop LexerMode | Push LexerMode
  deriving (Eq, Ord, Show)

keepMode :: (Functor f) => f a -> f (a, [LexerModeAction])
keepMode = fmap (, [])

type TokenParser a = LexerSpec a -> Parser (RawToken a, [LexerModeAction])

data TokenizerState =
  TokenizerState
  { _tsModeStack :: [LexerMode]
  , _tsSourcePos :: SourcePos
  , _tsInput     :: [Char]
  } deriving (Show)

emptyTokenizerState :: Text -> Text -> TokenizerState
emptyTokenizerState name =
  TokenizerState emptyModeStack (initialPos $ Just name) . T.unpack

makeLenses ''TokenizerState

type TokT = State TokenizerState

