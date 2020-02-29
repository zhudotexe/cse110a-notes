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
