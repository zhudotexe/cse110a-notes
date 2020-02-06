Data Representation
===================
Let's add support for multiple datatypes and calling external functions!

For this, we need tagged representations and calling conventions.

Plan
----

1. add representation for boolean values and numbers
2. arithmetic ops
3. arithmetic comparison
4. type checking

.. note::
    But how do we separate numbers from bools? We need some way to distinguish them.

    **Option 1**: use a second word to store the type - but this takes up 2x the mem and requires 2 memory accesses

    **Option 2**: Use a tag bit - the Least Significant Bit becomes 0 for number and 1 for bool.
        - so numbers are left shifted one bit - i.e. 3 = ``0x6 = 0b110``.
        - and booleans are encoded with MSB as truth - i.e. TRUE = ``0b1000...0001``

Types
-----
Let's extend our source types:

.. code-block:: haskell

    data Expr a =
        ...
        | Boolean Bool a

    data Arg =
        ...
        | HexConst Int

    -- Boolean False == HexConst 0x00000001
    -- Boolean True  == HexConst 0x80000001
    -- Number 3      == HexConst 0x00000006
    -- etc...

Transforms
----------
It's mainly codegen that matters, so let's look at ``compile``:

.. code-block:: haskell

    -- let's make a typeclass to convert haskell types to x86 representations:
    class Repr a where
        repr :: a -> Arg

    instance Repr Int where
        repr n = Const (Data.Bits.shift n 1)

    instance Repr Bool where
        repr False = HexConst 0x00000001
        repr True  = HexConst 0x80000001

    -- and immediate values:
    immArg :: Env -> ImmTag -> Arg
    immArg (Var x _)     = ...
    immArg (Number n _)  = repr n
    immArg (Boolean b _) = repr b

    -- now:
    compileEnv _ e@(Number _ _)  = [IMov (Reg EAX) (immArg env e)]
    compileEnv _ e@(Boolean _ _) = [IMov (Reg EAX) (immArg env e)]  -- new

**Quiz**: https://tiny.cc/cse110a-number-ind -> D

And now we need to fix our runtime.

.. code-block:: c

    void print(int val) {
        if (val == CONST_TRUE)
            printf("true");
        else if (val == CONST_FALSE)
            printf("false");
        else
            printf("%d", d >> 1);
    }

**Quiz**: https://tiny.cc/cse110a-letnum-ind -> C

**Quiz**: https://tiny.cc/cse110a-ifnum-ind -> C

Arithmetic Operations
---------------------

**Quiz**: https://tiny.cc/cse110a-addnum-ind

Addition and subtraction works fine, but...

**Quiz**: https://tiny.cc/cse110a-mulnum-ind

Multiplication multiplies the result by 2.

Strategy
^^^^^^^^

- addition and subtraction are fine
- for multiplication, we have to right-shift once

Types
^^^^^

.. code-block:: haskell

    data Instruction =
        ...
        | IShr Arg Arg  -- x86 right shift

    -- and then add this instruction to the compiler for times.

Now, what about ``2 * (-1)``?

We end up getting close to ``SIGNED_INT_MAX``. Our right shift is not sign extending!

So, we need to use ``sar`` - shift arithmetic right instead.

.. code-block:: haskell

    data Instruction =
        ...
        | ISar Arg Arg  -- x86 right shift

    compilePrim2 env Times v1 v2 = [ ...
        ISar (Reg EAX) (Const 1) ]

Arithmetic Comparisons
----------------------

**Via Bit-Twiddling**

Since a negative number's MSB is 1, we can implement ``arg1 < arg2`` as ``arg1 - arg2``, then only look at the MSB/LSB
to encode as a bool.

.. code-block:: asm

    mov eax, arg1
    sub eax, arg2
    and eax, 0x80000000
    or  eax, 0x00000001

Types
^^^^^

.. code-block:: haskell

    -- required types/methods
    data Instruction =
        ...
        | IAnd Arg Arg
        | IOr Arg Arg

    instrAsm (IAnd a1 a2) = ...
    instrAsm (IOr a1 a2) = ...

We can also compute a lot with just these:
    - ``a1 > a2`` == ``a2 < a1``.
    - ``a1 != a2`` == ``a1 < a2 || a2 < a1``
    - ``a1 = a2`` == ``!(a1 != a2)``

Dynamic Checking
----------------
But now we can have problems with expressions like ``2 + false``!

For now, we should try and abort execution when the wrong types of operands are found during execution.

- ``+`` -> int, int
- ``-`` -> int, int
- ``&&`` -> bool, bool
- ``=`` -> int or bool, int or bool
- etc.

Strategy
^^^^^^^^

- if eax is an int, just check that LSB is 0.
- if it's not, jump to an ``error_non_int`` label

.. code-block:: asm

    mov eax, arg
    mov ebx, eax
    and ebx, 0x1
    cmp ebx, 0
    jne error_non_number

and at the error:

.. code-block:: asm

    error_non_number:
        push eax
        push 0
        call error

Which we handle in the runtime:

.. code-block:: c

    void error(int code, int v) {
        if (code == 0) {
            fprintf(stderr, "Error: expected a number but got %#010x\n", v);
        } else if (code == 1) {
            ...
        } else {
            ...
        }
        exit(1);
    }

But running this on ``1`` causes a segfault!

Call Stack
^^^^^^^^^^
We need to manage the C call stack (C calling convention).

1. The local variables for an executing function are saved in its stack frame
2. The start of the stack frame is saved in ``ebp``
3. The start of the *next* stack frame is sabed in ``esp``.

**In the callee**:

.. code-block:: asm

    ; at the start of the fcn:
    push ebp        ; save previous caller's ebp on stack
    mov ebp, esp    ; make current esp the ebp
    sub esp, 4*N    ; allocate space for N new locals

    ; at the end:
    mov esp, ebp    ; restore value of esp to that just before call
    pop ebp         ; restore caller's ebp from stack
    ret             ; return to caller - return address pushed onto stack by caller

**In the caller**:

.. code-block:: asm

    push arg_N      ; push args, last first
    ...
    push arg_1
    call target     ; make the call, putting return addr on stack
    add esp, 4*N    ; clear args by adding 4*N

.. note::
    when compiling on macOS, must respect 16 byte stack alignment invariant - esp must be 16-byte aligned

So now...

.. code-block:: asm

    section .text
    extern error
    extern print
    global our_code_starts_here
    our_code_starts_here:
        ; == prelude ==
        push ebp
        mov ebp, esp
        sub esp, 0          ; 0 local variables here
        ; == logic ==
        mov eax, 1          ; not a valid number
        mov ebx, eax        ; copy into ebx register
        and ebx, 0x00000001 ; extract lsb
        cmp ebx, 0          ; check if lsb equals 0
        jne error_non_number
        ; == postlude ==
        mov esp, ebp
        pop ebp
        ret
    error_non_number:
        push eax
        push 0
        call error

But now our compiler has to keep track of how many local variables are needed!

Types
^^^^^

.. code-block:: haskell

    data Ty = TNumber | TBoolean  -- data type for the runtime types

    data Label =
        ...
        | TypeError Ty  -- type error label
        | Builtin Text  -- functions implemented in C

Transforms
^^^^^^^^^^

Now, the compiler has to:
    - dynamically typecheck
    - exit by calling error if an error occurs
    - manage the calling convention

Type Assertions
"""""""""""""""

.. code-block:: haskell

    -- asserts that v is of type ty by checking the LSB
    assertType :: Env -> IExp -> Ty -> [Instruction]
    assertType env v ty
        = [ IMov (Reg EAX) (immArg env v)
          , IMov (Reg EBX) (Reg EAX)
          , IAnd (Reg EBX) (HexConst 0x00000001)
          , ICmp (Reg EBX) (typeTag ty)
          , IJne (TypeError ty)
          ]

    -- where typeTag is:
    typeTag :: Ty -> Arg
    typeTag TNumber  = HexConst 0x00000000
    typeTag TBoolean = HexConst 0x00000001

    -- and add the type assertions:
    compilePrim2 :: Env -> Prim2 -> ImmE -> ImmE -> [Instruction]
    compilePrim2 env Plus v1 v2 = assertType env v1 TNumber
                               ++ assertType env v2 TNumber
                               ++ [ IMov (Reg EAX) (immArg env v1)
                                  , IAdd (Reg EAX) (immArg env v2)
                                  ]

Errors
""""""
We also need to write the actual error handlers, which just call the C function ``error``:

.. code-block:: haskell

    errorHandler :: Ty -> Asm
    errorHandler t =
        -- the expected-number error
        [ ILabel (TypeError t)
        -- push the second "value" param first,
        , IPush (Reg EAX)
        -- then the first "code" param,
        , IPush (ecode t)
        -- call the run-time's "error" function.
        , ICall (Builtin "error")
        ]

    ecode :: Ty -> Arg
    ecode TNumber = Const 0
    ecode TBoolean = Const 1

Stack Management
""""""""""""""""
First, local variables live at an offset from ebp now instead of esp.

.. code-block:: haskell

    immArg :: Env -> ImmTag -> Arg
    immArg _ (Number n _) = Const n
    immArg env (Var x _) = RegOffset EBP i
        where
            i = fromMaybe err (lookup x env)
            err = error (printf "Error: Variable '%s' is unbound" x)

Next, we need to make sure our code respects the calling convention - which we can do by just wrapping the
calling convention code around our generated body

.. code-block:: haskell

    compileBody :: AnfTagE -> Asm
    compileBody e = entryCode e
                 ++ compileEnv emptyEnv e
                 ++ exitCode e

    entryCode :: AnfTagE -> Asm
    entryCode e = [ IPush (Reg EBP)
                  , IMov (Reg EBP) (Reg ESP)
                  , ISub (Reg ESP) (Const 4 * n)
                  ]
        where
            n = countVars e  -- what's this?

    exitCode :: AnfTagE -> Asm
    exitCode = [ IMove (Reg ESP) (Reg EBP)
               , IPop (Reg EBP)
               , IRet
               ]

    -- just hack countvars for now
    countVars _ = 100

CountVars
"""""""""
So, as it turns out, getting an exact number is impossible.

But getting a heuristic is easy enough.