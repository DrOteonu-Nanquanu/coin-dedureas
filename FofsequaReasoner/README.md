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
Make sure you have the E Theorem Prover installed, which you can install with the provided script on a Unix-like OS (Linux/MacOS). It is currently only tested with Ubuntu, and should not work on Windows. The script depends on curl, tar, and CMake.

Run the script by `cd`ing into the directory where this README is located, and then either mark it executable (`chmod u+x ./eprover-init`) and run `./eprover-init`, or use the command `bash ./eprover-init`.

The executable is now located at `./eprover-executable/PROVER/eprover`. You will be asked to enter this location when first running the project.

### Publishing Fofsequa library
First build and publish (locally) the Fofsequa library. `cd` into the Fofsequa repository in the same repository as this project. Then execute `sbt publishLocal`.

### Running the program
`cd` into the directory where this README is located, and run `sbt run`. Or run `sbt test` to verify the project is installed correctly. If another project depends on this project as a library, also run `sbt publishLocal`. When running multiple `sbt` commands, you can also first execute `sbt` and then for example `test` and `publishLocal` in the CLI that will appear.
