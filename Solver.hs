module Solver where

-------------------- IMPORTS ----------------------------------------
import Types
import Display

import Debug.Trace

-------------------- SOLVER -----------------------------------------

fill hc = fillHc getAllPossible hc

fillHc :: [Hex] -> Honeycomb -> Honeycomb
fillHc [] _ = error "Honeycomb cannot be solved"
fillHc pos hc = 
				trace ("Calling fillHc with pos =" ++ show(pos) ++ ", hc = \n" ++ hcToString hc True)(
				let newhc = fillHc' pos hc in 
				if newhc == Just hc then (\(Just i) -> i) newhc
				else 	if newhc == Nothing then fillHc (tail pos) hc
						else   fillHc pos ((\(Just i) -> i)(newhc)) 
				)

-- fills Honeycomb
fillHc' :: [Hex] -> Honeycomb -> Maybe Honeycomb
fillHc' [] _ = error "Honeycomb cannot be solved"
fillHc' pos hc = 	
					trace ("Calling fillHc' with pos =" ++ show(pos)  ++ ", hc = \n" ++ hcToString hc True)(					
					if hexIndexInHc Nothing hc == Nothing then Just hc 
					else	if (fillNextNothing' pos hc) == Nothing then Nothing
							else  fillNextNothing' pos hc
					)

-- fills first Hex with Nothing value
fillNextNothing' :: [Hex] -> Honeycomb -> Maybe Honeycomb
fillNextNothing' pos hc = let index = hexIndexInHc Nothing hc in
						if trace ("Calling fillNextNothing' with index = " ++ show(index))(index == Nothing) then Just hc
						else 	if hexToInsert  ((\(Just i) -> i)(index)) pos hc == Nothing then Nothing
								else  (\i -> (Just i))(replaceInHc ((\(Just i) -> i)(index)) (hexToInsert  ((\(Just i) -> i)(index)) pos hc)  hc)

-- returns hex to insert
hexToInsert :: (Int, Int) -> [Hex] -> Honeycomb -> Hex
hexToInsert _ [] _ = Nothing
hexToInsert (r,i) pos hc 	| hexToInsert' (r,i) pos hc == Nothing = Nothing
							| otherwise = (\(Just i) -> i)(hexToInsert' (r,i) pos hc)

hexToInsert' :: (Int, Int) -> [Hex] -> Honeycomb -> Maybe Hex
hexToInsert' _ [] _ = Nothing
hexToInsert' (r,i) pos hc = firstNotOn pos (getAllAdjoining (r,i) hc)

-- replaces hex in honeycomb
replaceInHc :: (Int,Int) -> Hex -> Honeycomb -> Honeycomb
replaceInHc (r, i) hex hc = replaceAtIndex r (replaceAtIndex i hex (hc !! r)) hc

-- returns first from list that is not on second one
firstNotOn :: Eq a => [a] -> [a] -> Maybe a
firstNotOn [] _ = Nothing
firstNotOn (x:xs) list 	| not(elem x list) 	= Just x
						| otherwise 		= firstNotOn xs list

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
getHexDR (r, i) hc 	| r >= length (hc) - 1 				= Nothing
					| (odd r) && (i==length(hc !! r)-1)	= Nothing
					| odd r 							= (hc !! (r+1)) !! i
					| otherwise 						= (hc !! (r+1)) !! (i+1)

-- gets hex from down left (from next row)
getHexDL :: (Int, Int) -> Honeycomb -> Hex
getHexDL (r, i) hc 	| r >= length (hc) - 1 				= Nothing
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
