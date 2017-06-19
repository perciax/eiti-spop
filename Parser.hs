module Parser where
----------------- IMPORTS -------------------------------------

import Data.Char -- for using isDigit function
import Types

------------------ PARSER 1  -----------------------------------

data HoneycombLine = Plaster [String] deriving (Read, Show)

stringToHcLine :: String -> HoneycombLine
stringToHcLine s = read s :: HoneycombLine --read :: Read a => String -> a

hcLineToHc :: HoneycombLine -> Honeycomb
hcLineToHc (Plaster []) = []
hcLineToHc (Plaster (x:xs)) = (stringToRow x) : hcLineToHc (Plaster xs)

parseHc1 :: String ->Honeycomb
parseHc1 s = hcLineToHc (stringToHcLine s)

------------------ PARSER 2 -----------------------------------

--hcParser :: Parser Honeycomb
--hcParser = do
--			let row1 = [(Just 'B'),(Just 'B'),(Just 'B')]
--			let row2 = [Nothing,Nothing,(Just 'C')]
--			let ret = [row1,row2]
--			return ret

--beginParser :: Parser String
--beginParser 

--parseHc2 :: Parser a -> String -> a
--parseHc2 p inp = p inp

-- primitive parsers


------------------ COMMON FUNCTIONS ---------------------------

charToHex :: Char -> Hex
charToHex '.' = Nothing
charToHex c = Just c

stringToRow :: String -> Row
stringToRow "" = []
stringToRow (x:xs) = (charToHex x) : stringToRow xs
