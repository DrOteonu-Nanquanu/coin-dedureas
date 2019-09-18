# Eprover Performance Test

## Goal
This Scala program generates a test database for testing the performance of [E](https://github.com/eprover/eprover), a formal reasoner.

## Git
If you haven't already, git clone the coin-dedureas project to your own machine with `git clone https://github.com/oteonu/coin-dedureas.git`.

## Using the program

### Prerequisites
1. Have a JVM installed
2. Have Scala and SBT installed

### Installing E
You can either install E manually or use the provided script on a Unix-like (Linux/MacOS) OS. It is currently only tested with Ubuntu, and should not work on Windows. The script depends on curl, tar, and CMake.

Run the script by `cd`'ing into this directory, and then either mark it executable (`chmod u+x ./eprover-init`) and run `./eprover-init`, or use the command `bash ./eprover-init`.

### Running the program
`cd` into this directory, and run `sbt run` to generate the database. Execute E on the database with the following command: `time <e_path> ./eprover-test.tptp --auto --answers -s`. Where `<e_path>` should be replaced by the path to the E executable, which is equal to ./eprover-executable/PROVER/eprover if you've used the script.