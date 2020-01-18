Numbers, UnOps, Vars
====================

Let's write:
    - a compiler that translates a string into assembly
    - a runtime to execute it

Adder-1
-------
We have numbers!

The Runtime
^^^^^^^^^^^

.. code-block:: c

    #include <stdio.h>

    extern int our_code() asm("our_code_label");

    int main (int argc, char** argv) {
        int result = our_code();
        printf("%d\n", result);
        return 0;
    }

The main function calls ``our_code`` and prints its return value, where the return value is
the value stored in register ``EAX`` (per C calling convention) after running the assembly starting at label
``our_code_label``.

**Testing the Runtime**

A really simple example: Given the source program ``42``, we want to compile an assembly file ``fourty_two.s`` that
looks like:

.. code-block:: asm

    section .text
    global our_code_label
    our_code_label:
        mov eax, 42
        ret

**Assembling**

.. code-block:: bash

    $ nasm -f macho -o fourty_two.o fourty_two.s
    $ clang -g -m32 -o fourty_two.run fourty_two.o main.c
    $ fourty_two.run
    42

The Compiler
^^^^^^^^^^^^

Step 1: Types
"""""""""""""

Text -Parse> AST -CodeGen> ASM (.s)

The first step is to model the problem domain using types.

Let's call the input text ``Text``, the AST ``Expr``, and the ASM its own type.

A ``Text`` is a string.

An ``Expr`` is a tree structure:

.. code-block:: haskell

    data Expr = Number Int
    -- we'll add more later!

**ASM**

An ``Asm`` program is a list of instructions which can:
    - create a ``Label``
    - move an ``Arg`` into a ``Register``
    - ``Return`` to the runtime.

.. code-block:: haskell

    type Asm = [Instruction]

    data Instruction
        = ILabel Text
        | IMov   Arg Arg
        | IRet

    -- and:
    data Register
        = EAX

    data Arg
        = Const Int  -- literal number
        | Reg Register  -- register

Step 2: Transforms
""""""""""""""""""

.. code-block:: haskell

    parse :: Text -> Expr
    parse = parseWith expr  -- built in to parser.hs
        where
            expr = integer

    compile :: Expr -> Asm
    compile (Number n) =
        [ IMov (Reg EAX) (Const n)
        , IRet
        ]

    asm :: Asm -> Text
    asm is = L.intercalate
        "\n" [instr i | i <- is]

    -- where: (stringifiers)
    instr :: Instruction -> Text
    instr (IMov a1 a2) =
        printf "mov %s, %s"  -- String -> String
            (arg a1) (arg a2)

    arg :: Arg -> Text
    arg (Const n) = printf "%d" n
    arg (Reg r)   = reg r

    reg :: Register -> Text
    reg EAX = "eax"

.. note::

    We have 4 functions that crunch types to the text representation of x86:

    .. code-block:: haskell

        asm :: Asm -> Text
        instr :: Instruction -> Text
        arg :: Arg -> Text
        reg :: Register -> Text

    Let's write an overloaded function using typeclasses:

    .. code-block:: haskell

        class ToX86 a where
            asm :: a -> Text

        instance ToX86 Asm where
            asm is = L.intercalate "\n" [instr i | i <- is]

        instance ToX86 Instruction where
            asm (IMov a1 a2) = printf "mov %s, %s" (asm a1) (asm a2)
            asm IRet         = "ret"

        instance ToX86 Arg where
            asm (Const n) = printf "%d" n
            asm (Reg r)   = reg r

        instance ToX86 Register where
            asm EAX = "eax"

Adder-2
-------
Let's add incrementing!

``add1(7)``, ``add1(add1(42))``, etc.

Examples
^^^^^^^^^
``add1(7)``

In English:
    - move ``7`` into ``eax``
    - add ``1`` to ``eax``

in ASM:

.. code-block:: asm

    mov eax, 7
    add eax, 1

Now we have to handle the ``add`` instruction!

``add1(add1(42))``

.. code-block:: asm

    mov eax, 42
    add eax, 1
    add eax, 1

.. note::

    We have to write the compiler in a compositional manner:

    - generate ``Asm`` for each subexpression independently
    - generate ``Asm`` for each superexpression assuming the value of the subexpression is in ``eax``

Types
^^^^^

.. code-block:: haskell

    data Expr
        = Number Int
        | Add1   Expr

    data Instruction
        = ILabel Text
        | IMov   Arg Arg
        | IRet
        | IAdd   Arg Arg

Now our examples are:

.. code-block:: haskell

    src1 = "add1(7)"
    exp1 = Add1 (Number 7)
    asm1 = [ IMov (EAX) (Const 7)
           , IAdd (EAX) (Const 1)
           ]

    src2 = "add1(add1(42))"
    exp2 = Add1 (Add1 (Number 42))
    asm2 = [ IMov (EAX) (Const 42)
           , IAdd (EAX) (Const 1)
           , IAdd (EAX) (Const 1)
           ]

Parser
^^^^^^

.. code-block:: haskell

    parse :: Text -> Expr
    parse = parseWith expr

    expr :: Parser Expr
    expr = try primExpr
         <|> integer

    primExpr :: Parser Expr
    primExpr = Add1 <$> rWord "Add1" -- something, missing notes

Transformer
^^^^^^^^^^^

.. code-block:: haskell

    instance ToX86 Instruction where
        asm (IMov a1 a2) = printf "mov %s, %s" (asm a1) (asm a2)
        asm (IAdd a1 a2) = printf "add %s, %s" (asm a1) (asm a2)
        asm IRet         = "ret"

Compile
^^^^^^^

.. code-block:: haskell

    compile :: Expr -> Asm
    compile (Number n) =
        [IMov (Reg EAX) (Const n)]
    compile (Add1 e)
        = compile e
        ++ [IAdd (Reg EAX) (Const 1)]

Adder-3
-------
Add the ``twice()`` function that doubles the internal function!

... in homework!

Adder-4
-------
Numbers, Increment, Decrement, Local Vars

e.g. ``let x = add1(7), y = add1(x) in add1(y)``

Examples
^^^^^^^^

``let x = 10 in x``

Store 1 variable - x

``let x = 10, y = add1(x), z = add1(y) in add1(z)``

store 3 variables: x, y, z

.. code-block:: haskell

    let x = 10
        , c = let b = add1(a)
                in add1(b)
        in add1(c)

We need to handle N values in limited registers!

Let's look at memory!

Memory
""""""
As the stack gets higher, memory addresses get lower

We get a bunch of 4-byte slots on the stack at offsets from the stack pointer:

``EBP - 4 * 1, EBP - 4 * 2``, etc

The ``i`` th stack variable lives at ``EBP - 4 * i``.

So we need a mapping from source variables to stack positions.

