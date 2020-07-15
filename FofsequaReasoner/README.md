# Folsequa Translator

## Goal
Todo.

## Git
If you haven't already, git clone the coin-dedureas project to your own machine with `git clone https://github.com/oteonu/coin-dedureas.git`.

## Using the program
The program is currently under development and not ready for use yet.

### Prerequisites
1. Have a JVM installed
2. Have Scala and SBT installed

### Installing E
You can install E with the provided script on a Unix-like (Linux/MacOS) OS. It is currently only tested with Ubuntu, and should not work on Windows. The script depends on curl, tar, and CMake.

Run the script by `cd`ing into the directory where this README is located, and then either mark it executable (`chmod u+x ./eprover-init`) and run `./eprover-init`, or use the command `bash ./eprover-init`.

Make sure to add the newly created directory `eprover-executable/PROVER` to your `$PATH`.

### Publishing Fofsequa library
First build and publish (locally) the Fofsequa library. `cd` into the Fofsequa repository in the same repository as this project. Then execute `sbt publishLocal`.

### Running the program
`cd` into the directory where this README is located, and run `sbt run`