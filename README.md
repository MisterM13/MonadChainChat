


> Written with [StackEdit](https://stackedit.io/) by [Matthias Müller](mailto:matthias01.mueller@stud.unibas.ch).

# Creating a Blockchain based Chat Application - Monad Chain Chat

## About the Project
This Project is part of the work for the monad seminar of the University of Basel. The goal is to create a simple chat application, which works on a blockchain, written in haskell with monadic structures. 

## How to protect the Chat Data from Corruption in case of an Aplication Crash?
 
 We have two or three files: the _chatevent_ (= append only log), the _metadata_ and a _key_ file (if it is not also integrated into the metadata file).
 To guarantee that no data is lost due to a crash, we make a _newevent_ file, which contains the newly generated informations. Then the _newevent_ and the _chatevent_ file get read out, connected together to a _headchatevent_ file. The _headchatevent_ gets controlled by a special controll function, which generates, if everything was correct, a _headmeta_ file.
Then the _headmeta_ file gets proofed. After that, the _chatevent_ gets overwritten by _headchatevent_ and the _metadata_ gets overwritten by _headmeta_. 

## First tries in implementing creating & saving Functions

As Haskell has an predefined `appendFile` function,  I decided to to append every new message / event Block right on the _chatevent_ without making a special head file for this case. Within the `appendEvent` function, how i called the function to append a new event to the _chatevent_, i also call the `makeMetahead` function,  which makes the temporary meta file (with a function `extractMeta` which should extract the needed information from the _chatevent_ and put it into the _headmeta_).
At the end, when we finished appending messages on the _chatevent_ we can call the `makeMeta` function, which reads in the   _headmeta_ file and then the _chatevent_ and extracts the data once again from _chatevent_, then it checks the two datasets with each other, to proof that the _metahead_ file is up to date. If it proofs correct, it copies _metahead.txt_ to _meta.txt_ with one command (doesen't worke jet, because the file is locked from the readin, i still have to find a solution for that).
