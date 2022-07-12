


> Written with [StackEdit](https://stackedit.io/) by [Matthias Müller](mailto:matthias01.mueller@stud.unibas.ch).

# Creating a Blockchain based Chat Application - Monad Chain Chat

## About the Project
This Project is part of the work for the monad seminar of the University of Basel. The goal is to create a simple chat application, which works on a blockchain, written in haskell with monadic structures. 

## How to protect the Chat Data from Corruption in case of an Aplication Crash?
 
 We have two or three files: the _chatevent_ (= append only log), the _metadata_ and a _key_ file (if it is not also integrated into the metadata file).
 To guarantee that no data is lost due to a crash, we make a _newevent_ file, which contains the newly generated informations. Then the _newevent_ and the _chatevent_ file get read out, connected together to a _headchatevent_ file. The _headchatevent_ gets controlled by a special controll function, which generates, if everything was correct, a _headmeta_ file.
Then the _headmeta_ file gets proofed. After that, the _chatevent_ gets overwritten by _headchatevent_ and the _metadata_ gets overwritten by _headmeta_. 
