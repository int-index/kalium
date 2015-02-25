# Kalium

This is a Pascal to Haskell translator. The ultimate goal is to generate
idiomatic functional code from imperative code. The project is under heavy
development, so nothing is documented.


## Building

You need the following programs to build *Kalium*:

* ghc (7.8.4 or higher)
* cabal-install (1.22 or higher)
* happy (1.19.5 or higher)

Download the source (using `git clone`) and issue the following commands in the
project root directory to initialize the building environment:

* `cabal sandbox init`
* `cabal --require-sandbox install --dependencies-only --enable-tests`

You can safely omit `--enable-tests` if you don't want to use the testing
system. If something goes wrong, you can always `cabal sandbox delete` and then
repeat the steps above. Now you can build the project itself with `cabal build`.


## Testing

Run the regression tests with `cabal test`. Python 3.4 or higher is needed.

Tests are stored in `testing/tests/`, one per directory. Each test consists of a
Pascal source file `program.pas` and a scenario file `scenarios`.  The latter is
simply a whitespace separated list of scenarios, against which the translated
program will be tested.

Scenarios are stored in `testing/scenarios/`, one per file. Each scenario is a
script that takes the path to the translated program through `STDIN` and outputs
a message to `STDOUT` if something is wrong.


## Structure

*Kalium* consists of a library that contains all the functionality and an
extremely small command-line tool. There are three parts of the library:

* Front-end: `Kalium.Pascal`
* Core: `Kalium.Nucleus`
* Back-end: `Kalium.Haskell`

The distinction between these parts is kept clear to allow front/back-ends for
other languages in the future. The command-line tool isn't ready for use yet,
and for now it's suitable for debug purposes only.


## Contributing

Contact me if you feel adventurous and want to contribute. My email address can
be found in commits.
