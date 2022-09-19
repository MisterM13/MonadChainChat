 


> Written with [StackEdit](https://stackedit.io/) by [Matthias Müller](mailto:matthias01.mueller@stud.unibas.ch).

# Creating a Blockchain based Chat Application - Monad Chain Chat

## About the Project
This Project is part of the work for the monad seminar at the University of Basel. The goal is to create a simple chat application, which works on a blockchain, written in haskell with monadic structures. 

## How to protect the chat data from corruption in case of an application crash?
 
 We have two or three files: 
 - the _chatevent_ (= append only log),
 -  the _metadata_ 
 - and a _key_ file (if it is not also integrated into the metadata file).
 
 To guarantee that no data is lost due to a crash, we make a _newevent_ file, which contains the newly generated informations. Then the _newevent_ and the _chatevent_ file get read out, connected together to a _headchatevent_ file. The _headchatevent_ gets controlled by a special controll function, which generates, if everything was correct, a _headmeta_ file.
Then the _headmeta_ file gets proofed. After that, the _chatevent_ gets overwritten by _headchatevent_ and the _metadata_ gets overwritten by _headmeta_. 

## First tries in implementing creating & saving functions

As Haskell has a predefined `appendFile` function,  I decided to append every new message / event Block right on the _chatevent_ without making a special head file for this case. Within the `appendEvent` function, how i called the function to append a new event to the _chatevent_, i also call the `makeMetahead` function,  which makes the temporary meta file (with a function `extractMeta`, which should extract the needed information from the _chatevent_ and put it into the _headmeta_).
At the end, when we finished appending messages on the _chatevent_, we can call the `makeMeta` function, which reads in the   _headmeta_ file and then the _chatevent_ and extracts the data once again from _chatevent_, then it checks the two datasets with each other, to proof that the _metahead_ file is up to date. If it proofs correct, it copies _metahead.txt_ to _meta.txt_ with one command (doesen't work jet, because the file is locked from the readin, i still have to find a solution for that).

## Ordering the messages
As i found no good solution to get a timestamp from the system, I decided to use nummeration. To make it congruent, i still have to find a Way to resolve multiple nummerations so the order is still given over more than one log. I will maybe write the matching/joint numbers  in both logs.
For the write ahead log i have two ideas to keep it congruent. On one hand i could use a checksum, for all the messages that are saved at the head file. So if something's missing, the checksum doesn't mach. On the other hand i could make a lock file, which is existent during the saving process and deleted afterwards, so if the file is present and it's not during the saving function, something is corrupted. As i overthought the two methods i realised there's a much simpler way. I create a _metaHead_ file then append the new things to the _meta_ file. Once all is appended, the _metaHead_ file is removed. So if something crashes, the file is still present and we know some data is maybe corrupted.
Update: On my research for the Elliptic Curve implementation i found the `Data.Time.Clock.POSIX` module and how to use it. As using the current System time it should be much easier to use, instead of using multiple counters for every chat. So i implemented a time function, which returns the time as integer as nanoseconds since epoch, as this function was already implemented in the [example code](https://hackage.haskell.org/package/time-1.13/docs/Data-Time-Clock-POSIX.html). Maybe I write also an additional function to prevent time which is older than the current log time.

## Multiple chatlogs and timestamps

When I thought about having multiple chatlogs, I stumbled over several problems:
First of all i need to find a method to read out all the logs, but haskel `readFile` doesn't work with regular expressions, so i can't just read out all the Files with:
```haskell
readFile "*.txt"
```
The solution i thought for this problem is writing a seperate _names.txt_ file, which includes all the chat names. Then i can read out this file and give the user the option which chat he wants to open.
The second problem that comes up, is how to get a timestamp as I won't use a system time function. Hopefully the solution to this is relative nummerating: Every message has two numbers, the first is the number of the own numeration, and the second of the numeration of the other user, so we should get a relative time consistency. However i still have to plan how and which Blockevents get both time numbers and which not.

## Problems with imports
At a certain point I met the problem, that I need some special modules like `Data.Strings`. As I saw them at [hackage.haskell.org](https://hackage.haskell.org/package/strings-1.1/docs/Data-Strings.html) I first tried just to make `import Data.Strings` at my file, but the module was not known by the compiler, so I tried to install the module. I tried Cabal and Stack and both gave an error message that the file/folder is unknown. 
Because the Module was not within the normal Stack files, I couldn't just use `stack install`, so I imported the files manually by creating a directory _Data_ and putting the File _Strings.hs_ in it and it worked. I had also some Problems importing _Crypto.ECC_ (used for encrypting and hashing) , but then I realized that the package was named _Cryptonite-0.30_ instead of _Crypto_.

## Elliptic Curve Cryptography
For the ECC I found the _Crytonite_ library, as already mentioned above. But the library didn't have examples and as I'm not very proficient on Elliptic Curve and reading the module descriptions, I had some hard time to find out how to use this module. Luckily I found a Youtube [Video](https://www.youtube.com/watch?v=jcd__aqidhc) about this module (they are likely rare). There was also an other very interesting [tutorial Video](https://www.youtube.com/watch?v=DrqORFeJlOI) I can adwise, but it was only about hashing, TOTP and HMac.

## Restructuring the Backgrid of the MonadChainChat

As everything got a little bit messy with that lot of files, I tried to restructure the whole architecture. Making different Folders:

Chats
: is used to store the readable chat (also known as metadata)

Chatlogs
: is used to store the blockchain eventlogs with cryptological verification data

Keystore
: is used to store all the keys (own private and public key, and the public key of chat partners)

Import
: is used to easely import the files (eventlog and key)

Export
: is used to easely export the files (eventlog and key)

I also made a new concept of the `makeMeta` method. How it should work at the end: 
![schematic](schematic_makeMeta.png)
