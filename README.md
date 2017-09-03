Assembler suite
===============

WARNING: This is a work in progress, expect bugs and the API may break several times.


Assembler
---------

The assembler is embedded in Haskell.


Demo
----

To compile and run the included demos:

    stack build demo
    stack exec demo -- --directory <dir>

Replace `<dir>` with the path to a directory where the exmaple files should be written.


CPUs
----

- 6502, 6502 with illegals, 65c02
- 6809 (in progress)
- open framefork to add more


Platforms
---------

- C64
- Atari (8-bit home computer, in progress)


Projects
--------

- https://github.com/alexkazik/tapecart-browser


TODO
----

- Documentation of the assembler language and it's compiler
- Documentation of the source of the compiler
- Nicer error messages
- More/better examples
- Atari and 6809 support
