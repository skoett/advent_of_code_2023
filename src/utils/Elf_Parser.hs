module ElfParser where
import Text.ParserCombinators.Parsec (Parser)

data NumValue = NumValue { num :: Int
                         , index :: Int
                         }
              | StringValue { stringNum :: StringNum 
                            , index :: Int
                            } 
              deriving (Eq, Enum, Ord, Show)

-- newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

parseNumValue :: Parser NumValue
parseNumValue = do s <- ManyOf "onetwothreefourfivesixseveneightnine"
                 return $ case s of
                    "one" -> NumValue {num = 1, index = 0}
                    "two" -> NumValue {num = 2, index = 0}
                    "three" -> NumValue {num = 3, index = 0}
                    "four" -> NumValue {num = 4, index = 0}
                    "five" -> NumValue {num = 5, index = 0}
                    "six" -> NumValue {num = 6, index = 0}
                    "seven" -> NumValue {num = 7, index = 0}
                    "eight" -> NumValue {num = 8, index = 0}
                    "nine" -> NumValue {num = 9, index = 0}