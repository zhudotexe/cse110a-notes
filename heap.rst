Heap Data
=========

Let's add support for data structures!

For this, we need heap allocation

We already have support for two:

.. code-block:: haskell

    data Ty
        = TNumber
        | TBoolean

and we could add things like floats or chars, etc

But these are all fixed-length structures!

Unbounded Structures
--------------------
Let's look at unbounded data structures like lists or trees - we need to put things on the heap

Pairs
-----
Let's add pairs.

Constructing pairs: ``pair := (e0, e1)``

Accessing pairs: ``p[0], p[1]``

So how do we:
    - represent pairs in memory
    - construct pairs in assembly
    - access pairs in assembly?

Representation
^^^^^^^^^^^^^^
For all our previous data types, they were always a single word on the stack or in a register.

This doesn't work for pairs though.

Pointers
""""""""
We represent a pair as a pointer to a block of 2 adjacent words of memory. For example, the structure ``(1, (2, 3))``
is stored on the heap as:

.. code-block:: text

       literal 1
       pointer a

    a: literal 2
       literal 3

How do we tell the difference between a number and a pointer?

We'll tag the last 3 bits.
    - if the LSB is 0, it's a number
    - if the 3 LSBs are 111, it's a bool
    - if the 3 LSBs are 001, it's a pointer

This only leaves 29 bits for the address, but if we always allocate in 8-byte chunks, the 3 LSBs don't matter!

Construction
^^^^^^^^^^^^
To construct a pair, we need to:
    - allocate a 2-word block and get the starting address from eax
    - copy the first value into ``[eax]``, and the second ``[eax+4]``
    - tag the last bit of eax with 1

The resulting eax is the value of the pair (a pointer).

Also, both elements of a pair need to be immediate. And pointers are immediates!

Allocation
^^^^^^^^^^
The global register ``esi`` keeps track of where the next free block on the heap is
so when we need a new block, we can:

- copy ``esi`` into ``eax``
- set the last bit to 1
- increment ``esi`` by 8 (note: some padding might be required!)

But this means we have no garbage collection!

Example:

.. code-block:: haskell

    let p = (3, (4, 5)),
        x = p[0],
        y = p[1][0],
        z = p[1][1]
    in
        x + y + z

    -- becomes, after ANFing

    let anf0 = (4, 5),
        p    = (3, anf0),
        x    = p[0]
        anf1 = p[1],
        y    = anf1[0],
        z    = anf1[1],
        anf2 = x + y
    in
        anf2 + z

    -- and the heap becomes
    0: 0x8  -- literal 4
    4: 0xA  -- literal 5
    8: 0x6  -- literal 3
    c: 0x1  -- pointer to 0x0

    -- and the stack (in the in clause)
    18: 0xE  -- anf2: literal 7
    14: 0xA  -- z:    literal 5
    10: 0x8  -- y:    literal 4
    0c: 0x1  -- anf1: pointer to 0x0
    08: 0x6  -- x:    literal 3
    04: 0x9  -- p:    pointer to 0x8
    00: 0x1  -- anf0: pointer to 0x0

Accessing
^^^^^^^^^
To access pair elements (i.e. compile expressions like ``e[0]``):

- check that ``e`` is an immediate that is a pointer
- load ``e`` into ``eax``
- remove the tag bit from ``eax``
- copy the value in ``[eax]`` (or ``[eax+4]`` for snd) into ``eax``

Implementation
^^^^^^^^^^^^^^

First, in the C runtime, we need to allocate heap space and pass a heap pointer to the assembly.

And also update ``print()`` to print pairs.

.. code-block:: c

    int main(int argc, char** argv) {
        int* HEAP = calloc(HEAP_SIZE, sizeof(int));
        int result = our_code_starts_here(HEAP);      // find this param and put it in esi!
        print(result);
        return 0;
    }

    // for printing, we need to recursively look until we find a primitive
    int isPair(int p) {
        return (p & 0x7) == 0x1;
    }

    void print(int val) {
        if (val & 0x00000001 ^ 0x00000001) { // val is a number
            printf("%d", val >> 1);
        }
        else if (val == 0xFFFFFFFF) { // val is true
            printf("true");
        }
        else if (val == 0x7FFFFFFF) { // val is false
            printf("false");
        }
        else if (isPair(val)) {
            int* valp = (int*) (val - 1); // extract address
            printf("(");
            print(*valp); // print first element
            printf(", ");
            print(*(valp + 1)); // print second element
            printf(")");
        }
        else {
            printf("Unknown value: %#010x", val);
        }
    }

Types
"""""

.. code-block:: haskell

    data Expr a
        = ...
        | Pair    (Expr a) (Expr a) a
        | GetItem (Expr a) Field    a

    data Field = First | Second

    -- dynamic types
    data Ty = TNumber | TBoolean | TPair

    -- register
    data Register
        = ...
        | ESI

Transforms
""""""""""
Now, our code has to:

- initialize ``esi``
- construct pairs
- access pairs

The latter two are handled by the AST.

**Initialize ESI**

.. code-block:: haskell

    prelude :: [Instruction]
    prelude =
        [ IMov (Reg ESI) (RegOffset 4 ESP),         -- copy param off stack
          IAdd (Reg ESI) (Const 8),                 -- adjust to ensure 8-byte aligned
          IAnd (Reg ESI) (HexConst 0xfffffff8) ]

**Construct**

.. code-block:: haskell

    type Asm = [Instruction]

    compileExpr env (Pair v1 v2)
        = pairAlloc                         -- 1. allocate pair
        ++ pairCopy First (immArg env v1)   -- 2. copy values
        ++ pairCopy Second (immArg env v2)
        ++ setTag EAX TPair                 -- 3. tag pointer

    pairAlloc :: Asm
    pairAlloc = [ IMov (Reg EAX) (Reg ESI),   -- copy free addr into eax
                  IAdd (Reg ESI) (Const 8) ]  -- increment esi by 8

    pairCopy :: Field -> Arg -> Asm
    pairCopy fld a
        = [ IMov (Reg EBX) a,
            IMov (pairAddr fld) (Reg EBX) ]

    -- the field's slot is either [eax] or [eax+4] depending on whether it's the fst or snd field
    pairAddr :: Field -> Arg
    pairAddr fld = Sized DWordPtr (RegOffset (4 * fieldOffset) EAX)

    fieldOffset :: Field -> Int
    fieldOffset First = 0
    fieldOffset Second = 1

    setTag :: Register -> Ty -> Asm
    setTag r ty = [ IAdd (Reg r) (typeTag ty) ]

    typeTag :: Ty -> Arg
    typeTag TNumber  = HexConst 0x0
    typeTag TBoolean = HexConst 0x7
    typeTag TPair    = HexConst 0x1

**Access**

.. code-block:: haskell

    compileExpr env (GetItem e fld)
        = assertType env e TPair                -- 1. check that e is a (pair) pointer
        ++ [ IMov (Reg EAX) (immArg env e) ]    -- 2. load pointer into eax
        ++ unsetTag EAX TPair                   -- 3. remove tag bit to get address
        ++ [ IMov (Reg EAX) (pairAddr fld) ]    -- 4. copy value from resp. slot to eax

    unsetTag :: Register -> Ty -> Asm
    unsetTag r ty = [ ISub (Reg r) (typeTag ty) ]

N-ary Tuples
------------
How do we generalize pairs to allow for tuples of arbitrary size?

Well, we can store them like Haskell lists ``(first, rest)``

Construction
^^^^^^^^^^^^

.. code-block:: haskell

    def tup3(x1, x2, x3):
        (x1, (x2, x3))

    -- and so on...

Accessing
^^^^^^^^^

.. code-block:: haskell

    def get(t, i):
        if i == 0:
            t[0]
        else:
            get(t[1], i-1)


