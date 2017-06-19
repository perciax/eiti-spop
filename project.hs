--------------------- IMPORTS ------------------------------------
import Types
import Display
import Parser
import Solver

------------------------ MAIN ------------------------------------		
main :: IO()									
main = do
			-- load honeycomb line from file
			line <- loadHcLine
			-- print loaded line
			putStrLn "File content:"
			putStrLn line
			putStrLn "Parsed honeycomb:"
			let hc = parseHc1 line
			displayHc hc
			if isHcFilled hc then putStrLn "filled" else putStrLn "not filled"
			putStrLn ("First empty hex in first row: "
					 ++ show (hexIndexInRow Nothing (hc !! 0)))
			putStrLn ("First empty hex in Honeycomb: "
					 ++ show (hexIndexInHc Nothing hc))

			let x = (1,2)
			let nbh = getAllAdjoining x hc
			putStrLn (show(nbh))
			let all = getAllPossible
			putStrLn (show(all))
			
			let hc1 = fill hc
			displayHc hc1

----------------- LOADING FROM FILE -----------------------------
loadHcLine :: IO String
loadHcLine = do
				putStrLn "Give filename:"
				-- get file name
				fileName <- getLine
				-- read file content
				fileContent <- readFile fileName
				-- return file content    
				return fileContent 

---------------------------------------------------------------
