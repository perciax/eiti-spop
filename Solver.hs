module Solver where

-------------------- IMPORTS ----------------------------------------
import Types

-------------------- SOLVER -----------------------------------------

-- fills first Hex with Nothing value
fillNextNothing :: Honeycomb -> Maybe Honeycomb
fillNextNothing hc = Nothing

-- replaces hex in honeycomb
replaceInHc :: (Int,Int) -> Hex -> Honeycomb -> Honeycomb
replaceInHc (r, i) hex hc = replaceAtIndex r (replaceAtIndex i hex (hc !! r)) hc

-- replaces element on list
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex index elem list = take index list ++ [elem] ++ drop (index + 1) list

-------------------- HEX GETTERS ------------------------------------
-- gets hex from indexed place
getHex :: (Int, Int) -> Honeycomb -> Hex
getHex (r, i) hc = (hc !! r) !! i

-- gets hex from right (next in row)
getHexR :: (Int, Int) -> Honeycomb -> Hex
getHexR (r, i) hc = if i < (length (hc !! r) - 1) then (hc !! r) !! (i+1)
					else Nothing

-- gets hex from left (previous in row)
getHexL :: (Int, Int) -> Honeycomb -> Hex
getHexL (r, i) hc = if i == 0 then Nothing
					else (hc !! r) !! (i-1)

-- gets hex from up right (from previous row)
getHexUR :: (Int, Int) -> Honeycomb -> Hex
getHexUR (r, i) hc 	| r <= 0 							= Nothing
					| (odd r) && (i==length(hc !! r)-1)	= Nothing
					| odd r 							= (hc !! (r-1)) !! i
					| otherwise 						= (hc !! (r-1)) !! (i+1)

-- gets hex from up left (from previous row)
getHexUL :: (Int, Int) -> Honeycomb -> Hex
getHexUL (r, i) hc 	| r <= 0 							= Nothing
					| (odd r) && (i==0)					= Nothing
					| odd r 							= (hc !! (r-1)) !! (i-1)
					| otherwise 						= (hc !! (r-1)) !! i

-- gets hex from down right (from next row)
getHexDR :: (Int, Int) -> Honeycomb -> Hex
getHexDR (r, i) hc 	| r > length (hc) - 1 				= Nothing
					| (odd r) && (i==length(hc !! r)-1)	= Nothing
					| odd r 							= (hc !! (r+1)) !! i
					| otherwise 						= (hc !! (r+1)) !! (i+1)

-- gets hex from down left (from next row)
getHexDL :: (Int, Int) -> Honeycomb -> Hex
getHexDL (r, i) hc 	| r > length (hc) - 1 				= Nothing
					| (odd r) && (i==0)					= Nothing
					| odd r 							= (hc !! (r+1)) !! (i-1)
					| otherwise 						= (hc !! (r+1)) !! i

-- gets list of all (6) Hexes thah adjoin one with given indexes
getAllAdjoining :: (Int, Int) -> Honeycomb -> [Hex]
getAllAdjoining x hc = [(getHexUR x hc),(getHexR x hc), (getHexDR x hc),
					(getHexDL x hc), (getHexL x hc), (getHexUL x hc)]

-- gets list of Hexes that can fill Honeycomb
getAllPossible :: [Hex]
getAllPossible = map (\i -> Just i) ['A' .. 'G'] 


-------------------------- HEX FINDERS -------------------------------------

-- finds indexes of given hex in Honeycomb
hexIndexInHc :: Hex -> Honeycomb -> Maybe (Int, Int)
hexIndexInHc _ [] = Nothing
hexIndexInHc hex hc = hexIndexInHc' hex hc 0

hexIndexInHc' :: Hex -> Honeycomb -> Int -> Maybe (Int, Int)
hexIndexInHc' _ [] _ = Nothing
hexIndexInHc' hex (r:rs) rindex	| hexIndexInRow hex r /= Nothing	
						= Just (rindex, (\(Just i)->i) (hexIndexInRow hex r))
								| otherwise	
						= hexIndexInHc' hex rs (rindex+1)

-- finds index of given hex in row
hexIndexInRow :: Hex -> Row -> Maybe Int
hexIndexInRow hex row = hexIndexInRow' hex row 0

hexIndexInRow' :: Hex -> Row -> Int -> Maybe Int
hexIndexInRow' _ [] _ = Nothing
hexIndexInRow' hex (x:xs) index | x==hex	= Just index
								| otherwise	= hexIndexInRow' hex xs (index+1)

-----------------------------------------------------------------------------
-- check if honeycomb is fully filled (unused)
isHcFilled :: Honeycomb -> Bool
isHcFilled hc = not (elem False (map isRowFilled hc)) 

-- check if row is fully filled (unused)
isRowFilled :: Row -> Bool
isRowFilled row = not (elem Nothing row)
