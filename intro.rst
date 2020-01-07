Intro to Compilers
==================

`Slides <https://ucsc-cse-110a.github.io/winter20/slides/intro.key.pdf>`_

What is a Compiler?
-------------------
A function that maps an input string to an output string. Generally, these represent programs.

For examples, most language compilers compile human readable text into bytecode.

The output program should:
    - have the same meaning as the input
    - be runnable on hardware

What do they look like?
-----------------------

1. Source string is parsed into an AST (Abstract Syntax Tree)
2. AST is checked to make sure it makes sense
3. AST is simplified into IR (Intermediate Representation)
4. IR is optimized
5. IR -> CodeGen -> ASM (Assembly for architecture)
6. ASM is linked against a runtime (e.g. stdlibs)

Class Intro
===========
Class webpage: `Here <https://ucsc-cse-110a.github.io/winter20/>`_

