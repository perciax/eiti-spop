module Display where

----------------- IMPORTS -------------------------------------

import Data.List
import Types

----------------- DISPLAYING HONEYCOMB ------------------------

-- converts Hex (Maybe Char) to Char
hexToChar :: Hex -> Char
hexToChar Nothing = '*'
hexToChar (Just c) = c

-- charToString 
charToString :: Char -> String
charToString c = [c]

-- converts Row (list of Hexes) to String (uses space as separator)
rowToString :: Row -> Bool -> String
rowToString [] _ = ""
rowToString row oddRow = (if oddRow then " " else "") 
				++ intercalate " " (map charToString (map hexToChar row))

-- converts Honeycomb (list of Rows) to String (uses "\n" as separator) 
hcToString [] _ = ""
hcToString (x:xs) oddRow = (rowToString x oddRow) ++ "\n" 
							++ (hcToString xs (not oddRow))

-- display Honeycomb
displayHc :: Honeycomb -> IO()
displayHc hc = do
				putStrLn "------------------"
				-- second argument is True - first Row is shorter than second
				putStrLn (hcToString hc True) 
				putStrLn "------------------"

