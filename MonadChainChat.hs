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
import Crypto.PubKey.ECC.ECDSA (sign, verify, PublicKey, PrivateKey, Signature)
import Crypto.PubKey.ECC.Generate (generate)
import Crypto.PubKey.ECC.Types (getCurveByName, CurveName(..))
import Data.Time
import Data.Time.Clock.POSIX
import Data.Int





main = do 
	putStrLn "This is a sample: "
	createFolders
	print "Folders successfully Built."
	createKeys
	putStrLn "generating Chatevent..."
	makeChatevent
	putStrLn "appending to Chatevent..."
	putStrLn "writing to Max: "
	appendEvent "Max"
--	putStrLn "generating Metahead file..."
--	makeMetahead "Max" "Matthias"
--	putStrLn "prooving and generating Meta file..."
--	makeMeta "Max"

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

makeMetahead :: [Char] -> [Char] -> IO ()
makeMetahead chatname yourname = do
	createFolders	
	ownChatlog <- extractFile "Chatlogs/chatevent.txt"
	otherChatlog <- extractFile ("Chatlogs/"++chatname++".txt")
	print "verifying Logs:"
	v1 <- verifyLog ownChatlog  "own"
	v2 <- verifyLog otherChatlog chatname
	print ("Your Chatlog verification: ", v1)
	print (chatname ++ "Chatlog verification: " ,v2) 
	print "extracting Data..."
	let ownBlocks = getBlocklistRelated ownChatlog (packStr chatname)
	let otherBlocks = getBlocklistRelated otherChatlog (packStr yourname)
	let blocks = ownBlocks ++ otherBlocks
	let blockData = makeMetaList blocks
	--let sortedData = sort blocks
	--print(sortedData)
	writeMetahead chatname blockData
	copyMeta chatname

--sort :: [ByteString] -> [ByteString]
--sort blocks = do 
--	sorted <- isSortedlow blocks
--	if sorted 
--	then do
--		let [result] = blocks
--		return result
--	else do
--		let first <- (head blocks)
--		let second <- (head (tail blocks))
--		let third <- (tail (tail blocks))
--		timehead <-  getBlockTime first
--		let timeheadInt = read (show timehead) ::Int
--		timetail <- getBlockTime second 
--		let timetailInt = read (show timetail) ::Int
--		if timeheadInt <= timetailInt
--		then do
--			let sortedtail = sort (tail blocks)
--			return (first : sortedtail)
--		else do
--			firstthird <- sort (first : third)
--			return (second : firstthird)

-- sort blocks = do
-- 	sorted <- isSortedlow blocks
-- 	if not sorted 
-- 	then do
-- 		let (lower, upper) = switch (head blocks) (head (tail blocks))
-- 		return lower : upper : sort (tail blocks)
-- 	else
-- 		return blocks

switch :: MonadFail m => ByteString -> ByteString -> m (ByteString, ByteString)
switch b1 b2 = do
	val1 <- getBlockTime b1
	let tval1 =  read (show val1) ::Int
	val2 <- getBlockTime b2
	let tval2 =  read (show val2) ::Int
	if tval1 < tval2
	then do
		return (val1,val2)
	else do
		return (val2,val1)

	
-- code used from exercises of the Paradigm lecture
isSortedlow :: MonadFail m => [ByteString] -> m Bool
isSortedlow blocks = do 
	timehead <-  getBlockTime (head blocks)
	let timeheadInt = read (show timehead) ::Int
	timetail <- getBlockTime (head (tail blocks)) 
	let timetailInt = read (show timetail) ::Int
	if length blocks > 1
	then do
		tailSorted <- isSortedlow (tail blocks)
		return (timeheadInt <= timetailInt  && tailSorted)
	else do
		return True
	

writeMetahead:: Show a => [Char] -> [[(a, [Char], [Char])]] -> IO ()
writeMetahead chatname blockData = do
	datablock <- (blockDataToString (head blockData))
	writeFile ("Chats/"++chatname++"-head.txt") datablock
	appendMetahead chatname (tail blockData)

appendMetahead:: Show a => [Char] -> [[(a, [Char], [Char])]] -> IO ()
appendMetahead chatname blockData = do
	if length blockData > 1
	then do
		datablock <- (blockDataToString (head blockData))
		appendFile ("Chats/"++chatname++"-head.txt") ("\n"++ datablock)
		appendMetahead chatname (tail blockData)
	else do 
		datablock <- (blockDataToString (head blockData))
		appendFile ("Chats/"++chatname++"-head.txt") ("\n"++ datablock)
	
verifyLog :: [ByteString] -> [Char] -> IO Bool
verifyLog chatlog pubKey = do
	if length chatlog <= 1
	then do 
		return True
	else do
		let preblock = head chatlog
		let afterblock = head (tail chatlog)
		-- print afterblock
		let signatur = (getBlockSig afterblock)
		-- print signatur
		let cropSig = strReplace "\"" "" signatur
		let sig  = (read cropSig)::Crypto.PubKey.ECC.ECDSA.Signature
		-- print sig
		key <- (loadKey pubKey) 
		let verifyhead = (verify SHA3_256 key sig preblock)
		-- print verifyhead
		verifytail <- (verifyLog (tail chatlog) pubKey)
		return (verifyhead && verifytail)
	

copyMeta :: [Char] -> IO ()
copyMeta chatname = do
 	copyFile ("Chats/"++chatname++"-head.txt") ("Chats/"++chatname++".txt")
	correctCopy <- check ("Chats/"++chatname++"-head.txt") ("Chats/"++chatname++".txt")
	if correctCopy
	then do
		print ("Copying '"++chatname++"-head.txt' to '" ++chatname++".txt' was successfull.")
		removeFile ("Chats/"++chatname++"-head.txt")
	else do
		print "Error: Meta-head file could not be copied."

testBlocks :: IO ()
testBlocks = do
	blocks <- extractFile "Chatlogs/chatevent.txt"
	let chatname = packStr "Max"
	print chatname
	let relatedBlocks = getBlocklistRelated blocks chatname
	print relatedBlocks
	let chatname = packStr "Fritz"
	print chatname
	let relatedBlocks = getBlocklistRelated blocks chatname
	print relatedBlocks

makeMetaList :: [ByteString] -> [[(ByteString, [Char], [Char])]]
makeMetaList blocklist = do
	let blocklisthead = getBlockData (head blocklist)
	if length blocklist > 1
	then do
		let blocklisttail = makeMetaList (tail blocklist)
		return blocklisthead ++ blocklisttail
	else
		return blocklisthead
		
getBlocklistRelated :: [ByteString] -> ByteString -> [ByteString]
getBlocklistRelated chatevent chatname = [x | x <- chatevent, y <- isRelated x chatname, y]

getBlockData :: ByteString -> [(ByteString, [Char], [Char])]
getBlockData block = [(time, ("to: "++ show name),("msg: "++ show msg))| time <- getBlockTime block, name <- getBlockName block, msg <- getBlockMessage block]

blockDataToString :: (Monad m, Show a) => [(a, [Char], [Char])] -> m [Char]
blockDataToString block = do
	let [(time, name, msg)] = block
	let strTime = show time
	return (strTime++"\t\t" ++ name++"\t\t"++msg)

-- code used from lecture "Funktoren, Applikative Funktoren"
lift2 :: Monad m => (t1 -> t2 -> b) -> m t1 -> m t2 -> m b
lift2 f x y = do
	a <- x
	b <- y
	return (f a b)

getBlockSig :: ByteString -> [Char]
getBlockSig block = do
	[sig,name,time,msg] <- extractBlock block
--	return sig 
	signatur <- (show sig)
	return (signatur)

getBlockName :: MonadFail m => ByteString -> m ByteString
getBlockName block = do
	[sig,name,time,msg] <- extractBlock block
	return name
	
getBlockMessage :: MonadFail m => ByteString -> m ByteString	
getBlockMessage block = do	
	[sig,name,time,msg] <- extractBlock block
	return msg
		
getBlockTime :: MonadFail m => ByteString -> m ByteString
getBlockTime block = do
	[sig,name,time,msg] <- extractBlock block
	return time

isRelated :: MonadFail m => ByteString -> ByteString -> m Bool
isRelated block chatname = do
	[sig,name,time,msg] <- extractBlock block
	return (name == chatname)

extractBlock :: Monad m => ByteString -> m [ByteString]
extractBlock block = do
	return (BS.split ';' block)

extractFile :: FilePath -> IO [ByteString]
extractFile path = do
	exists <-  fileExist path
	if exists
	then do
		file <- readFile path
		return  (BS.split '\n' (packStr file))
	else do
		print ("this File doesn't exist: "++path)
		return [(packStr "")]
	
readHeadFile :: [Char] -> IO String
readHeadFile chatname = do
	headFile <- readFile ("Chats/"++ chatname ++ "-head.txt")
	return headFile
	
readChat :: [Char] -> IO String
readChat chatname = do
	metaFile <- readFile ("Chats/"++chatname++".txt")
	return metaFile

check :: FilePath -> FilePath -> IO Bool
check file1 file2 = do
	f1 <- readFile file1
	f2 <- readFile file2
	return (f1 == f2)
	
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
		
loadKey :: [Char] -> IO PublicKey
loadKey name = do
	pubKeytext <- readFile  ("Keystore/"++name++"PubKey.txt")
	let pubKey = (read pubKeytext)
	return (pubKey ::Crypto.PubKey.ECC.ECDSA.PublicKey)
	

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
		then importChatfile nameraw
		else importChatfile (nameraw ++ ".txt")
	
importChatfile :: String -> IO ()
importChatfile name = do
	createFolders
	let filepath = ("Import/"++ name ++".txt")
	--let name = "\n" ++ strReplace ".txt" "" filepath
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