import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as T

word :: Parsec String () String
word = many1 letter

separator :: Parsec String a ()
separator = skipMany1 (space <|> char ',')

sentence :: Parsec String () [String]
sentence = do
  words <- sepBy1 word separator
  end <- oneOf ".?!"
  return words

test1 :: Parsec String () String
test1 = string "(a)" <|> string "(b)"

test2 :: Parsec String () String
test2 = try (string "(a)") <|> string "(b)"

word' :: Parsec String Int String
word' = do
  word <- many1 letter
  updateState (+1)
  return word

separator' :: Parsec String Int ()
separator' = skipMany1 (space <|> char ',')

sentence' :: Parsec String Int [String]
sentence' = do
  words <- sepBy1 word' separator'
  oneOf ".?!"
  return words

wordCount :: Parsec String Int Int
wordCount = do
  words <- sentence'
  c <- getState
  return c

lexer :: T.TokenParser ()
lexer = T.makeTokenParser emptyDef

lexeme :: Parsec String () a -> Parsec String () a
lexeme = T.lexeme lexer

float :: Parsec String () Double
float = T.float lexer
