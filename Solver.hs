module Solver where

-------------------- IMPORTS ----------------------------------------
import Types

import Display		-- used for debugging
import Debug.Trace	-- used for debugging

-------------------- SOLVER -----------------------------------------

fill hc = (\(Just i)-> i)(fillHc'' getAllPossible (Just hc))

fillHc'' :: [Hex] -> Maybe Honeycomb -> Maybe Honeycomb
fillHc'' [] _ = Nothing
fillHc'' _ Nothing = Nothing
fillHc'' pos hc = 	-- trace ("Calling fillHc'' with pos =" ++ show(pos)  ++ ", hc = \n" ++ hcToString ((\(Just i)-> i) hc) True) -- DEBUG
					--(
						let nextNothing = hexIndexInHc Nothing ((\(Just i)-> i) hc)	-- Sprawdza czy jest jeszcze jakis pusty hex
 	
						in	if  --trace("nextNothing: " ++ show(nextNothing))(--
								nextNothing == Nothing
								--) 
							then hc -- KONIEC (plaster pelny)

							else fillHc''' pos hc	-- Probuj wypelnic					
					--)

fillHc''' :: [Hex] -> Maybe Honeycomb -> Maybe Honeycomb
fillHc''' [] _ = Nothing	-- jezeli mozliwe litery sie skoncza, to zwraca Nothing do fillHc'' (nie da sie wypelnic nastepnego)
fillHc''' pos hc = 	
					--trace ("Calling fillHc''' with pos =" ++ show(pos)  ++ ", hc = \n" ++ hcToString ((\(Just i)-> i) hc) True) -- DEBUG
					--(
						let newhc = fillNextNothing' pos ((\(Just i)-> i) hc)	-- wypelnij pierwsze puste

						in	if fillHc'' getAllPossible newhc == Nothing -- Jezeli nie da sie wypelnic nastepepnego pola

							then fillHc''' (dropWhile (==(hexToInsert2  ( (\(Just i) -> i) (hexIndexInHc Nothing (\(Just i) -> i) hc) ) ) pos (\(Just i) -> i) hc) ) ) pos) hc	-- probuj inna litere
							else fillHc'' getAllPossible newhc	-- wpp wypelniaj nastepna
					--) 


-- fills first Hex with Nothing value
fillNextNothing' :: [Hex] -> Honeycomb -> Maybe Honeycomb
fillNextNothing' pos hc = let index = hexIndexInHc Nothing hc in
						if index == Nothing then Just hc
						else 	if hexToInsert2  ((\(Just i) -> i)(index)) pos hc == Nothing then Nothing
								else  (\i -> (Just i))(replaceInHc ((\(Just i) -> i)(index)) (hexToInsert2  ((\(Just i) -> i)(index)) pos hc)  hc)


--------------- FINDING POSSIBLE HEX FOR GIVEN INDEXES ------------------------

-- returns hex to insert
hexToInsert2 :: (Int, Int) -> [Hex] -> Honeycomb -> Hex
hexToInsert2 _ [] _ = Nothing
hexToInsert2 (r,i) pos hc 	| hexToInsert2' (r,i) pos hc == Nothing = Nothing
							| otherwise = (\(Just i) -> i)(hexToInsert2' (r,i) pos hc)

hexToInsert2' :: (Int, Int) -> [Hex] -> Honeycomb -> Maybe Hex
hexToInsert2' _ [] _ = Nothing
hexToInsert2' (r,i) pos hc = firstNotOn pos (getAllColliding (r,i) hc)


-- returns hex to insert
hexToInsert :: (Int, Int) -> [Hex] -> Honeycomb -> Hex
hexToInsert _ [] _ = Nothing
hexToInsert (r,i) pos hc 	| hexToInsert' (r,i) pos hc == Nothing = Nothing
							| otherwise = (\(Just i) -> i)(hexToInsert' (r,i) pos hc)

hexToInsert' :: (Int, Int) -> [Hex] -> Honeycomb -> Maybe Hex
hexToInsert' _ [] _ = Nothing
hexToInsert' (r,i) pos hc = firstNotOn pos (getAllAdjoining (r,i) hc)

-- returns first from list that is not on second one
firstNotOn :: Eq a => [a] -> [a] -> Maybe a
firstNotOn [] _ = Nothing
firstNotOn (x:xs) list 	| not(elem x list) 	= Just x
						| otherwise 		= firstNotOn xs list

------------------ REPLACING HEX IN HONEYCOMB ---------------------------------

-- replaces hex in honeycomb
replaceInHc :: (Int,Int) -> Hex -> Honeycomb -> Honeycomb
replaceInHc (r, i) hex hc = replaceAtIndex r (replaceAtIndex i hex (hc !! r)) hc



-- replaces element on list
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex index elem list = take index list ++ [elem] ++ drop (index + 1) list

-------------------- HEX GETTERS ----------------------------------------------
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

-- gets list of all (6) Hexes that adjoin one with given indexes
getAllAdjoining :: (Int, Int) -> Honeycomb -> [Hex]
getAllAdjoining x hc = [(getHexUR x hc),(getHexR x hc), (getHexDR x hc),
					(getHexDL x hc), (getHexL x hc), (getHexUL x hc)]

-- gets list of Hexes that can fill Honeycomb
getAllPossible :: [Hex]
getAllPossible = map (\i -> Just i) ['A' .. 'G'] 


------------------------- 7 HEX GETTERS ------------------------------------
-- gets 7 hexes around one from right (next in row)
get7HexR :: (Int, Int) -> Honeycomb -> [Hex]
get7HexR (r, i) hc = if i < (length (hc !! r) - 1) 
					then (getAllAdjoining (r,i+1) hc) ++ [(hc !! r) !! (i+1)]
					else [Nothing]

-- gets hex from left (previous in row)
get7HexL :: (Int, Int) -> Honeycomb -> [Hex]
get7HexL (r, i) hc = if i == 0 then [Nothing]
					else (getAllAdjoining (r,i-1) hc) ++ [(hc !! r) !! (i-1)]

-- gets hex from up right (from previous row)
get7HexUR :: (Int, Int) -> Honeycomb -> [Hex]
get7HexUR (r, i) hc | r <= 0 							= [Nothing]
					| (odd r) && (i==length(hc !! r)-1)	= [Nothing]
					| odd r 							= (getAllAdjoining (r-1,i) hc) ++ [(hc !! (r-1)) !! i]
					| otherwise 						= (getAllAdjoining (r-1,i+1) hc) ++ [(hc !! (r-1)) !! (i+1)]

-- gets hex from up left (from previous row)
get7HexUL :: (Int, Int) -> Honeycomb -> [Hex]
get7HexUL (r, i) hc | r <= 0 							= [Nothing]
					| (odd r) && (i==0)					= [Nothing]
					| odd r 							= (getAllAdjoining (r-1,i-1) hc) ++ [(hc !! (r-1)) !! (i-1)]
					| otherwise 						= (getAllAdjoining (r-1,i) hc) ++ [(hc !! (r-1)) !! i]

-- gets hex from down right (from next row)
get7HexDR :: (Int, Int) -> Honeycomb -> [Hex]
get7HexDR (r, i) hc | r >= length (hc) - 1 				= [Nothing]
					| (odd r) && (i==length(hc !! r)-1)	= [Nothing]
					| odd r 							= (getAllAdjoining (r+1,i) hc) ++ [(hc !! (r+1)) !! i]
					| otherwise 						= (getAllAdjoining (r+1,i+1) hc) ++ [(hc !! (r+1)) !! (i+1)]

-- gets hex from down left (from next row)
get7HexDL :: (Int, Int) -> Honeycomb -> [Hex]
get7HexDL (r, i) hc | r >= length (hc) - 1 				= [Nothing]
					| (odd r) && (i==0)					= [Nothing]
					| odd r 							= (getAllAdjoining (r+1,i-1) hc) ++ [(hc !! (r+1)) !! (i-1)]
					| otherwise 						= (getAllAdjoining (r+1,i) hc) ++ [(hc !! (r+1)) !! i]

getAllColliding :: (Int, Int) -> Honeycomb -> [Hex]
getAllColliding (r,i) hc = get7HexUR (r, i) hc ++ get7HexR (r, i) hc ++ get7HexDR (r, i) hc 
							++ get7HexDL (r, i) hc ++ get7HexL (r, i) hc ++ get7HexUL (r, i) hc 

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
