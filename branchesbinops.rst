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

Binary Operations
-----------------

**Quiz**: http://tiny.cc/cse110a-sub-ind -> B

Examples
^^^^^^^^

Numbers are easy:

.. code-block:: haskell

    -- 33 - 10
    mov eax, 33
    sub eax, 10

    -- 2 * 2
    mov eax, 2
    mul eax, 2

If either operand is a number, just use it as a literal

But what about:

.. code-block:: haskell

    let x = 10,
        y = 20,
        z = 30
    in
        x + (y * z)

    -- ==

    let x = 10,
        y = 20,
        z = 30,
        tmp = y * z
    in
        x + tmp

    -- ==

    mov eax, 10
    mov [ebp-4*1], eax  -- x = 10
    mov eax, 20
    mov [ebp-4*2], eax  -- y = 20
    mov eax, 30
    mov [ebp-4*3], eax  -- z = 30

    mov eax, [ebp-4*2]  -- y
    mul eax, [ebp-4*3]  -- y * z
    mov [ebp-4*4], eax  -- tmp = y * z

    mov eax, [ebp-4*1]  -- x
    add eax, [ebp-4*4]  -- x + tmp

So, if either operand is a variable, just grab it from the stack

**Quiz**: https://tiny.cc/cse110a-nest-ind -> C

But then what about ``(1 + 2) * (3 + 4)``?

Both ``1`` and ``x`` (numbers and variables) are immediate operations - the operand values don't require any computation

Administrative Normal Form (ANF)
""""""""""""""""""""""""""""""""""
An expression is in ANF if all primitive operations have immediate arguments

**Quiz**: https://tiny.cc/cse110a-anf-ind -> C

Not in ANF: ``(1 + 2) * (3 + 4)``

in ANF:

.. code-block:: haskell

    let t1 = 1 + 2,
        t2 = 3 + 4
    in
        t1 * t2

Strategy
^^^^^^^^
We can convert any expression to ANF by adding temporary variables for subexpressions, so..

we add a Normalize step to our code pipeline to make things ANF before we get to codegen

``Text =Parse> AST =Norm> ANF =Tag> ANF-Tag =CodeGen> ASM``

Types
^^^^^

.. code-block:: haskell

    data Prim2 =  -- called prim2 since it takes 2 args
        Plus | Minus | Times

    data Expr a =
        ...
        | Prim2 Prim2 (Expr a) (Expr a) a

    -- so, 2+3 becomes
    Prim2 Plus (Number 2 ()) (Number 3 ()) ()

    -- assembly:
    data Instruction =
        ...
        | IAdd Arg Arg
        | ISub Arg Arg
        | IMul Arg Arg

ANF
"""
We can define a separate type for ANF, but that's boring

so let's write a function that describes immediate code:

.. code-block:: haskell

    isImm :: Expr a -> Bool
    isImm (Number _ _) = True
    isImm (Var    _ _) = True
    isImm _            = False

    -- and isANF:
    isAnf :: Expr a -> Bool
    isAnf (Number      _ _) = True
    isAnf (Var         _ _) = True
    isAnf (Prim2 _ e1 e2 _) = isImm e1 && isImm e2
    isAnf (If   e1 e2 e3 _) = isAnf e2 && isAnf e3
    isAnf (Let   x e1 e2 _) = isAnf e2

Now, we define an **ANF expression** any ``Expr`` s.t. ``isAnf expr -> True``.

.. code-block:: haskell

    type BareE   = Expr ()
    type AnfE    = Expr ()
    type AnfTagE = Expr Tag
    type ImmTagE = Expr Tag

Compiling
^^^^^^^^^
Going from ``AnfTagE`` to ``Asm`` is easy, since both operands for every binop will be an immediate.

.. code-block:: haskell

    compile :: Env -> TagE -> Asm
    compile env (Prim2 o v1 v2)
        = [ IMov (Reg EAX) (immArg env v1),
            (prim2 o) (Reg EAX) (immArg env v2) ]

    prim2 :: Prim2 -> Arg -> Arg -> Instruction
    prim2 Plus = IAdd
    prim2 Minus = ISub
    prim2 Times = IMul

    immArg :: Env -> ImmTag -> Arg
    immArg _   (Number n _) = Const n
    immArg env (Var    x _) = RegOffset ESP i
        where
            i   = fromMaybe err (lookup x env)
            err = error "unbound variable"

**Quiz**: https://tiny.cc/cse110a-anf2-ind -> E