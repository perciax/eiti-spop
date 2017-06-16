




------------------------ MAIN ----------------------------------		
									
main = do
			-- load honeycomb line from file
			hcLine <- loadHcLine
			-- print loaded line
			putStrLn "Zawartość pliku: "
			putStrLn hcLine

			print 5

----------------- LOADING FROM FILE ---------------------------

loadHcLine = do
				putStrLn "Podaj nazwe pliku z plastrem"
				-- get file name
				fileName <- getLine
				-- read file content
				fileContent <- readFile fileName
				-- retutn file content    
				return fileContent 
