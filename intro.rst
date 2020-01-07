Intro to Compilers
==================

`Slides <https://ucsc-cse-110a.github.io/winter20/slides/intro.key.pdf>`_

`Class Webpage <https://ucsc-cse-110a.github.io/winter20/>`_

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

Intro to Haskell
================

`Slides <https://ucsc-cse-110a.github.io/winter20/slides/haskell.key.pdf>`_

Haskell is neat.

Quicksort:

.. code-block:: haskell

    sort [] = []
    sort x:xs = sort l ++ [x] ++ sort r
        where
            l = [e | e <- xs, e <= x]
            r = [e | e <- xs, e > x]

Programs in Haskell are expressions, not a sequence of statements; it evaluates to a value and has no side effects

Functions are first-class values

See `last quarter's notes <https://cse116-notes.readthedocs.io/en/latest/haskell.html>`_.



