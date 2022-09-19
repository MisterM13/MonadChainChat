module MonadChainChat where
import System.Directory
import System.IO
import System.Posix.Files
import Data.Char
import Control.Monad.Trans.Maybe
import Data.Strings
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS
import Crypto.Hash.Algorithms
import Crypto.PubKey.ECC.ECDSA (sign, verify, PublicKey, PrivateKey)
import Crypto.PubKey.ECC.Generate (generate)
import Crypto.PubKey.ECC.Types (getCurveByName, CurveName(..))
import Data.Time
import Data.Time.Clock.POSIX
import Data.Int




main :: IO String
main = do 
	putStrLn "This is a sample: "
	createFolders
	print "Folders successfully Built."
	createKeys
	putStrLn "generating Chatevent..."
	makeChatevent
	putStrLn "appending to Chatevent..."
	appendEvent "Max"
	putStrLn "generating Metahead file..."
--	makeMetahead "Max"
--	putStrLn "prooving and generating Meta file..."
--	makeMeta "Max"
--	putStrLn "reading Chatevent..."
	readChatevent

-- code used from https://www.youtube.com/watch?v=X8XHXhSvfrY (12.7.2022)
makeChatevent :: IO()
makeChatevent = do 
	createFolders
	exists <- fileExist "Chatlogs/chatevent.txt"
	if exists
	then do
		print "The Chatevent Log already exists."
	else do
		(pubKey,privKey) <- loadKeys
		writeFile "Chatlogs/chatevent.txt" ("New Chat Event" ++ show pubKey)
		print "Chatevent Log successfully created."


packStr :: String -> ByteString
packStr = TE.encodeUtf8.T.pack

--makeMetahead :: IO()
-- makeMetahead chatname = do
-- 	createFolders
-- 	input <- readChatevent
-- 	chatEvent <- extractMeta input
-- 	print ("making metahead from Chat: " ++ chatname)
-- 	writeFile ("Chats/"++chatname++"-head.txt") ("example Meta file\n" ++ chatEvent)
-- 
-- --makeMeta :: IO()
-- makeMeta chatname = do 
-- 	headinput <- readHeadFile chatname
-- 	input <- readChatevent
-- 	chatEvent <- extractMeta input
-- 	check ("example Meta file\n" ++ chatEvent) headinput		
-- 

makeMetahead chatname = do
	createFolders	
	ownChatlog <- readChatevent
	otherChatlog <- readFile ("Chatlogs/"++chatname++".txt")
-- TODO: Implement function verificateChatlog
--	verificateChatlog ownChatlog  ("Keys/ownPubKey.txt")
--	verificateChatlog otherChatlog ("Keys/"++chatname++"PubKey.txt")
-- TODO: Implement function extractMeta
--	ownBlocks <- extractMeta chatname ownChatlog
--	otherBlocks <- extractMeta chatname otherChatlog
--	let blocks = ownBlocks + otherBlocks
--	sortedData <- sort blocks
	print(ownChatlog)

--verificateChatlog chatlog pubKey = do
--	blocks <- readFile chatlog
--	fmap verificateBlock pubKey blocks
	
-- verificateBlock pubKey blocks = do
	

copyMeta :: [Char] -> IO ()
copyMeta chatname = do
 	copyFile (chatname++"-head.txt") (chatname++".txt")
 	removeFile (chatname++"-head.txt")

-- TODO: implement extractMeta, which takes the meta data and filters it on chat
-- extracts the metadata from the log
--extractMeta :: Monad m => [Char] -> m [Char]
--extractMeta input = do
--	return ("content of chatEvent:\n" ++ input)

--extractMeta chatname chatlog = do
--	related <- fmap isRelated chatlog chatname
--	let relatedBlocks = filter related
--	return relatedBlocks
		

getBlockTime block = do
	[sig,name,time,msg] <- extractBlock block
	return time

--isRelated :: MonadFail m => ByteString -> ByteString -> m Bool
isRelated block chatname = do
	[sig,name,time,msg] <- extractBlock block
	return (name == chatname)

extractBlock :: Monad m => ByteString -> m [ByteString]
extractBlock block = do
	return (BS.split ';' block)
	
readHeadFile :: [Char] -> IO String
readHeadFile chatname = do
	headFile <- readFile ("Chats/"++ chatname ++ "-head.txt")
	return headFile
	
readChat :: [Char] -> IO String
readChat chatname = do
	metaFile <- readFile ("Chats/"++chatname++".txt")
	return metaFile

--check :: [Char] -> [Char] -> IO ()
--check file1 file2
--	| file1 == file2 = writeFile "meta.txt" file2
--	| otherwise = putStrLn "metahead file is not congruent, please create it again with makeMetahead"	-- we can't overwrite the own, already open headfile, so we have to run it manual...					

-- Asymetric key Crypto with help from https://www.youtube.com/watch?v=wjyiOXRuUdo (14.9.2022)
createKeys :: IO ()
createKeys = do
	createFolders
	exists <-  (fileExist "Keystore/PrivKey.txt")
	if exists
	then do
		print "You already created your keys"
	else do
		let secp256k1 = getCurveByName SEC_p256k1
		(pubKey,privKey) <- generate secp256k1
		writeFile "Keystore/PrivKey.txt" (show privKey)
		writeFile "Keystore/ownPubKey.txt" (show pubKey)
		print "Asymetric Keys successfully generated"

loadKeys :: IO (PublicKey, PrivateKey)
loadKeys = do
	exists <-  (fileExist "Keystore/PrivKey.txt")
	if exists
	then do
		privKeytext <- readFile "Keystore/PrivKey.txt"
		pubKeytext <- readFile  "Keystore/ownPubKey.txt"
		let privKey = (read privKeytext)
		let pubKey = (read pubKeytext)
		-- we need: 
		-- pubKey :: Crypto.PubKey.ECC.ECDSA.PublicKey
		-- privKey :: Crypto.PubKey.ECC.ECDSA.PrivateKey
		return (pubKey ::Crypto.PubKey.ECC.ECDSA.PublicKey, privKey :: Crypto.PubKey.ECC.ECDSA.PrivateKey )
	else do
		createKeys
		loadKeys
		
-- code used from https://hackage.haskell.org/package/time-1.13/docs/Data-Time-Clock-POSIX.html (15.9.22)
nanosSinceEpoch :: UTCTime -> Int64
nanosSinceEpoch =
	floor . (1e9 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds
	
getTime :: IO Int64	
getTime = do
	u <- getCurrentTime
	return $ nanosSinceEpoch u

appendEvent :: [Char] -> IO ()
appendEvent chatname = do 
	input <- getInput
	time <- getTime
	-- code used from https://stackoverflow.com/questions/41656678/haskell-read-last-line-with-a-lazy-mmap (15.9.22)
	preblock <- ((last . lines) <$> readFile "Chatlogs/chatevent.txt")
	(pubKey,privKey) <- loadKeys
	hash <- sign privKey SHA3_256 (packStr preblock)
	let text = ("\n"  ++ show hash ++ ";" ++ chatname ++ ";" ++ show time ++ ";" ++ input)
	appendFile "Chatlogs/chatevent.txt" text
--	makeMetahead chatname
	
readChatevent :: IO String
readChatevent = do
	chatEvent <- readFile "Chatlogs/chatevent.txt" 
	return chatEvent
	
getInput :: IO String	
getInput = do
	input <- getLine
	return input

-- Not used anymore:
-- createIndex :: IO ()
-- createIndex = do writeFile "index.txt" "0"
-- 
-- getIndex :: IO String
-- getIndex = do
-- 	number <- readFile "index.txt"
-- 	return number

-- code used from https://stackoverflow.com/questions/31342012/read-and-writing-to-file-in-haskell , 18.7.22
--				  https://stackoverflow.com/questions/20667478/haskell-string-int-type-conversion , 18.7.22
--incIndex :: IO ()
--incIndex = do 
--	handle <- openFile "index.txt" ReadWriteMode
--	num <- hGetContents handle
--	let ind = read num :: Integer
--	let inc = (ind+1)
--	writeFile "indexhead.txt" (show inc)
--	hClose handle
--	copyFile "indexhead.txt" "index.txt"
--	removeFile "indexhead.txt"
--	return()

--setIndex :: Show a => a -> IO ()
--setIndex num = do
--	writeFile "index.txt" (show num)

getNames :: IO ()
getNames = do
	exists <- fileExist "names.txt" 
	if exists
	then do 
		names <- readFile "names.txt"
		putStr ( "Here are the available chatnames:" ++ names ++ "\n" )
	else
		print "You have no Chats jet, please import some first."

importName :: IO ()
importName = do
	print "Please input the Name of the Chat, you have imported."
	nameraw <- getLine
	if (strEndsWith nameraw ".txt")
		then importChatFile nameraw
		else importChatFile (nameraw ++ ".txt")
	
importChatFile :: FilePath -> IO ()
importChatFile nameraw = do
	createFolders
	let filepath = ("Import/"++ nameraw)
	let name = "\n" ++ strReplace ".txt" "" filepath
	exists <-  (fileExist filepath) -- not Uppercase sensitive
	if exists
	then do
		appendFile "names.txt" name
		copyFile filepath ("Chatlogs/"++name++".txt")
		copyFile ("Import/"++name++"PubKey.txt") ("Keystore/"++name++"PubKey.txt")
		print ("Successfully imported Chat:" ++ name)
		removeFile filepath
		removeFile ("Import/"++name++"PubKey.txt")
	else do
		putStrLn "No importfile for this Name found."
		putStrLn "Make shure there is a File [Name].txt in the 'Import' folder."

exportChatfile :: IO ()
exportChatfile = do
	createFolders
	print "Please input your Name:\n"
	name <- getInput
	copyFile "Chatlogs/chatevent.txt" ("Export/"++name++".txt")
	copyFile "Keystore/ownPubKey.txt" ("Export/"++name++"PubKey.txt")
	print "Files successfully exported, please look in your 'Export' Folder."


createFolders :: IO ()
createFolders = do
	createDirectoryIfMissing False "Chats"
	createDirectoryIfMissing False "Chatlogs"
	createDirectoryIfMissing False "Keystore"
	createDirectoryIfMissing False "Import"
	createDirectoryIfMissing False "Export"
	

--- File Architecture: ---

-- names.txt: Names of the other messangers
-- MonadChainChat.hs

-- Chatlogs >>
-- Chatevent.txt: (this is the Append Only Log)
-- Checksum + Signature + Time/Index. + ReceiverNr + Message
-- Person.txt: (this are the AOL from the other messagers)

-- Keystore >>
-- pivKey.txt: My private key
-- pubKey.txt: My public key
-- personKey.txt: Public key of other messagers

-- Chats >>
-- PersonChat.txt: Messages ordered by time

-- Import >>
-- Person.txt:		Append Only Log from Person
-- personKey.txt	Public key from Person

-- Export >>
-- MyName.txt: 	Append Only Log from me
-- MyKey.txt: 	My public key	