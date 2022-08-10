module MonadChainChat where
import System.Directory
import System.IO
import Data.Char
import Control.Monad.Trans.Maybe
import Data.Strings

-- import Crypto.ECC

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

check :: [Char] -> [Char] -> IO ()
check file1 file2
	| file1 == file2 = writeFile "meta.txt" file2
	| otherwise = putStrLn "metahead file is not congruent, please create it again with makeMetahead"	-- we can't overwrite the own, already open headfile, so we have to run it manual...					

-- makeLock = do 
--	headinput <- readHeadFile
--	writeFile "lock.txt" getHash headinput

writeMeta :: IO ()
writeMeta = do
	copyFile "metahead.txt" "meta.txt"
	removeFile "metahead.txt"

-- extracts the metadata from the log
extractMeta :: Monad m => [Char] -> m [Char]
extractMeta input = do
	return ("content of chatEvent:\n" ++ input)

appendEvent :: IO()
appendEvent = do 
	input <- getInput
	index <- getIndex
	-- TODO: ECC encrypted Hash
	let text = ("\n" {- ++ hash -}  ++ index {- ++ otherindex -} ++ ";" ++ input)
	appendFile "chatevent.txt" text
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
	
getInput :: IO String	
getInput = do
	input <- getLine
	return input

createIndex :: IO ()
createIndex = do writeFile "index.txt" "0"

getIndex :: IO String
getIndex = do
	number <- readFile "index.txt"
	return number

-- code used from https://stackoverflow.com/questions/31342012/read-and-writing-to-file-in-haskell , 18.7.22
--				  https://stackoverflow.com/questions/20667478/haskell-string-int-type-conversion , 18.7.22
incIndex :: IO ()
incIndex = do 
	handle <- openFile "index.txt" ReadWriteMode
	num <- hGetContents handle
	let ind = read num :: Integer
	let inc = (ind+1)
	writeFile "indexhead.txt" (show inc)
	hClose handle
	copyFile "indexhead.txt" "index.txt"
	removeFile "indexhead.txt"
	return()

setIndex :: Show a => a -> IO ()
setIndex num = do
	writeFile "index.txt" (show num)

getNames :: IO ()
getNames = do
	exists <- doesFileExist "names.txt" 
	if exists
	then do 
		names <- readFile "names.txt"
		putStr ( "Here are the available chatnames:" ++ names ++ "\n" )
	else
		print "You have no Chats jet, please import some first."


importName = do
print "Please input the Name of the Chat, you have imported."
nameraw <- getLine
if (strEndsWith nameraw ".txt")
then do
	let file = nameraw
	let name = "\n" ++ strReplace ".txt" "" nameraw 	
	appendFile "names.txt" name
else do
	let file = nameraw ++ ".txt"
	let name = "\n" ++ nameraw
	appendFile "names.txt" name

--importChatFile filepath = do
--	if (fileExist filepath)
--	then do
--		let name = "\n" ++ strReplace ".txt" "" filepath
		

--- File Architecture: ---

-- Chatevent.txt: (this is the Append Only Log)
-- Checksum + Signature + Time/Index. + ReceiverNr + Message

-- Person.txt: (this are the AOL from the other messagers)

-- pivKey.txt: My private key
-- pubKey.txt: My public key
-- personKey.txt: public key of other messagers


-- index.txt: the current index of the messages in the Chatevent

-- meta.txt: Messages in ordered by Index

-- names.txt: Names of the other messangers