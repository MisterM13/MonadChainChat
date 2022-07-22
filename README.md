


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

## Multiple chatlogs and timestamps

When I thought about having multiple chatlogs, I stumbled over several problems:
First of all i need to find a method to read out all the logs, but haskel `readFile` doesn't work with regular expressions, so i can't just read out all the Files with:
```haskell
readFile "*.txt"
```
The solution i thought for this problem is writing a seperate _names.txt_ file, which includes all the chat names. Then i can read out this file and give the user the option which chat he wants to open.
The second problem that comes up, is how to get a timestamp as I won't use a system time function. Hopefully the solution to this is relative nummerating: Every message has two numbers, the first is the number of the own numeration, and the second of the numeration of the other user, so we should get a relative time consistency.

