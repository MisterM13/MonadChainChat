module MonadChainChat where
import System.Directory


--main :: IO ()
main = do 
	putStrLn "This is a sample: "
	putStrLn "generating Chatevent..."
	makeChatevent
	putStrLn "appending to Chatevent..."
	appendEvent
	putStrLn "generating Metahead file..."
	makeMetahead
	putStrLn "prooving and generating Meta file..."
	makeMeta
	putStrLn "reading Chatevent..."
	readChatevent

-- code used from https://www.youtube.com/watch?v=X8XHXhSvfrY (12.7.2022)
makeChatevent :: IO()
makeChatevent = do writeFile "chatevent.txt" "example Chatevent file"

--makeMeta = do writeFile "meta.txt" "example Meta file"


makeMetahead :: IO()
makeMetahead = do
	input <- readChatevent
	chatEvent <- extractMeta input
	putStrLn "making metahead..."
	writeFile "metahead.txt" ("example Meta file\n" ++ chatEvent)

makeMeta :: IO()
makeMeta = do 
	headinput <- readHeadFile
	input <- readChatevent
	chatEvent <- extractMeta input
	check ("example Meta file\n" ++ chatEvent) headinput		
	
check file1 file2
	| file1 == file2 = writeFile "meta.txt" file2
	| otherwise = putStrLn "metahead file is not congruent, please create it again with makeMetahead"	-- we can't overwrite the own, already open headfile, so we have to run it manual...					

-- makeLock = do 
--	headinput <- readHeadFile
--	writeFile "lock.txt" getHash headinput

writeMeta = do
	copyFile "metahead.txt" "meta.txt"

-- extracts the metadata from the log
extractMeta :: Monad m => [Char] -> m [Char]
extractMeta input = do
	return ("content of chatEvent:\n" ++ input)

appendEvent :: IO()
appendEvent = do 
	appendFile "chatevent.txt" "\nnew appended stuff"
	makeMetahead
	
readChatevent :: IO String
readChatevent = do
	chatEvent <- readFile "chatevent.txt" 
	return chatEvent
		
readHeadFile :: IO String
readHeadFile = do
	headFile <- readFile "metahead.txt"
	return headFile
	
readMetaFile :: IO String
readMetaFile = do
	metaFile <- readFile "meta.txt"
	return metaFile
	
getInput = do
	input <- getLine
	return input

createIndex = do writeFile "index.txt" "0"

getIndex = do
	number <- readFile "index.txt"
	return number
	