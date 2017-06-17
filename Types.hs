module Types where

----------------- DATA TYPES -----------------------------------

type Hex = Maybe Char	-- one honeycomb field (Char or Nothing)
type Row = [Hex]		-- row = list of hexes
type Honeycomb = [Row]	-- honeycomb = list of rowes


