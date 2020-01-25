Branches and Binary Operators
=============================
Let's add branches and binops!

Branches
--------

.. code-block:: haskell

    data Expr =
        -- ...
        | EIf Expr Expr Expr

Examples
^^^^^^^^
Treat 0 as false, non-zero as true

.. code-block:: haskell

    if 10:
        22
    else:
        sub1(0)
    -- 22

    if sub1(1):
        22
    else:
        sub1(0)
    -- -1

**Quiz**: https://tiny.cc/cse110a-if3-ind -> 0

Assembly Control Flow
^^^^^^^^^^^^^^^^^^^^^
To compile branches, we will use assembly labels - landmarks from which execution can be started from or skipped to

Also, comparisons: ``cmp a1, a2``
    - performs a numeric comparison between the 2 args
    - stores the result in a special processor flag

And jumps: ``jmp LABEL``, ``je LABEL``, ``jne LABEL``
    - uses the result of the flag set by the most recent ``cmp``

**Quiz**: https://tiny.cc/cse110a-control-ind -> A

Example:

.. code-block:: haskell

    if 10:
        22
    else:
        33

    -- compiles to

        mov eax, 10
        cmp eax, 0
        je  if_false
    if_true:
        mov eax, 22
        jmp if_exit
    if_false:
        mov eax, 33
    if_exit:

Strategy
^^^^^^^^
to compile:

.. code-block:: haskell

    if eCond:
        eThen
    else:
        eElse

we will:

1. compile eCond
2. compare the result in eax to 0
3. jump if the result is zero to a special false label
4. otherwise continue into eTrue, then jump to exit label

**What about multiple if-statements?**

We need a way to generate unique labels for each branch, since assembly can't reuse labels

Types
^^^^^

.. code-block:: haskell

    data Expr a =
        -- ...
        | EIf (Expr a) (Expr a) (Expr a) a

We add tags of type ``a`` for each subexpression (e.g. source-position info for error messages)

Let's define a name for ``Tag``:

``type Tag = Int``

and we now use:

.. code-block:: haskell

    type BareE = Expr ()
    type TagE  = Expr Tag

Now, extend the assembly:

.. code-block:: haskell

    data Label
        = BranchFalse Tag
        | BranchExit  Tag

    data Instruction
        = ...
        | ICmp Arg Arg
        | ILabel Label -- Create a label
        | IJmp Label -- Jump always
        | IJe Label -- Jump if equal
        | IJne Label -- Jump if not-equal

Transforms
^^^^^^^^^^

Tags
""""
We don't want the programmer to put in tags, so let's add a tag step to our pipeline!

.. code-block:: haskell

    > let e = parseStr "if 1: 22 else: 33"

    > e
    If (Number 1 ()) (Number 22 ()) (Number 33 ()) ()

    > label e
    If (Number 1 ((),0)) (Number 22 ((),1)) (Number 33 ((),2)) ((),3)

The key work is done by ``doTag i e``:
    1. recursively walk over the ``BareE`` (e) starting tagging at counter i
    2. return a pair ``(i', e')`` of the updated counter and tagged expr

**Quiz**: https://tiny.cc/cse110a-tag-ind -> D

We can now tag the entire program by calling doTag with the initial counter, and throwing away the final counter.

CodeGen
"""""""

.. code-block:: haskell

    compile env (If eCond eTrue eFalse i)
        = compile env eCond ++       -- start with the input
        [ICmp (Reg EAX) (Const 0),   -- test if it's false
        IJe (BranchFalse i)] ++      -- if it is, jump to the false label
        compile env eTrue ++         -- otherwise keep doing true
        [IJmp (BranchExit i)] ++     -- then leave
        [ILabel (BranchFalse i)] ++  -- the false label
        compile env eFalse ++        -- do the false
        [ILabel (BranchExit i)]      -- the leave label