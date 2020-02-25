Functions
=========

Let's add user-defined functions! To do this, we need:

- static checking
- calling conventions
- tail recursion

Defining Functions
------------------
Example:

.. code-block:: haskell

    def incr(x):
        x + 1

    incr(10)

There is a function definition followed by a single main expression that is evaluated to the result.

.. code-block:: haskell

    def fac(n):
        let t = print(n) in
        if n < 1:
            1
        else
            n * fac(n - 1)

    fac(5)

    5
    4
    3
    2
    1
    0
    120

Functions are top-level and named (and not first-class).

.. code-block:: haskell

    def fac(n):
        let t = print(n),
            r =
            if n < 1:
                1
            else
                n * fac(n - 1)
        in
            print(res)

    fac(5)

    5
    4
    3
    2
    1
    0
    1
    1
    2
    6
    24
    120
    120

And support mutual recursion!

.. code-block:: haskell

    def even(n):
        if (n == 0):
            true
        else:
            odd(n - 1)

    def odd(n):
        if (n == 0):
            false
        else:
            even(n - 1)

    let t0 = print(even(0)),
        t1 = print(even(1)),
        t2 = print(even(2)),
        t3 = print(even(3))
    in
        0

    true
    false
    true
    false
    0

Types
-----

Bindings
^^^^^^^^
We need a special type that represents places where variables are bound:

.. code-block:: haskell

    data Bind a = Bind Id a

A ``Bind`` is basically an ``Id`` (str) decorated with some metadata ``a``

We use these in 2 places:
    - let-bindings
    - function parameters

Programs/Declarations
^^^^^^^^^^^^^^^^^^^^^
Now, a program is a list of declarations followed by an expression.

.. code-block:: haskell

    data Program a = Prog
        { pDecls :: [Decl a],
          pBody  :: !(Expr a) }

    data Decl a = Decl
        { fName  :: (Bind a),  -- name
          fArgs  :: [Bind a],  -- params
          fBody  :: (Expr a),  -- body expression
          fLabel :: a }        -- metadata

    data Expr a =
        ...
        | Let (Bind a) (Expr a) (Expr a) a
        | App Id       [Expr a]          a

An app/call comprises:
    - an Id (name of func being called)
    - a list of expressions corresponding to the parameters
    - metadata

Static Checking
---------------
We should do this to point out code errors at compile time. It's called static checking because it's done without
compiling and running the code.

For example, we should check:
    - are variables we try and use defined?
    - are functions we try and use defined?
    - do function apps have the right amount of args?

And if they fail, we should fail with a useful message. Also, *all* the errors!

Types
^^^^^

.. code-block:: haskell

    data UserError = Error
        { eMsg  :: !Text,
          eSpan :: !SourceSpan }
        deriving (Show, Typeable)

    instance Exception [UserError]  -- we can throw an array of usererrors!

    -- making errors
    mkError :: Text -> SourceSpan -> Error
    mkError msg l = Error msg l

    abort :: UserError -> a
    abort e = throw [e]

    -- displaying errors
    renderErrors :: [UserError] -> IO Text
    -- should take an error annotated with sourcespan and output a pretty message!

    -- putting it all together
    main :: IO ()
    main = runCompiler `catch` esHandle

    esHandle :: [UserError] -> IO ()
    esHandle es = renderErrors es >>= hPutStrLn stderr >> exitFailure

Pipeline
^^^^^^^^
Text ``=Parse>`` BareP ``=Check>`` BareP ``=Norm>`` AnfP ``=Tag>`` AnfTagP ``=CodeGen>`` Asm

.. code-block:: haskell

    type BareP   = Program SourceSpan
    type AnfP    = Program SourceSpan
    type AnfTagP = Program (SourceSpan, Tag)

Catching Multiple Errors
^^^^^^^^^^^^^^^^^^^^^^^^
We should return as many errors as possible in one go.

So, we'll write a function that recursively walks over the entire program, and returns a list of all errors.
If the list is empty, it's all good, otherwise throw!

.. code-block:: haskell

    check :: BareProgram -> BareProgram
    check p = case wellFormed p of
        [] -> p
        es -> throw es

    wellFormed :: BareProgram -> [UserError]
    wellFormed (Prog ds e)
        = duplicateFunErrors ds
        ++ concatMap (wellFormedD fEnv) ds
        ++ wellFormedE fEnv emptyEnv e
        where
            fEnv = fromListEnv [(bindId f, length xs)
                                | Decl f xs _ _ <- ds]

``wellFormed`` does:
    - creates a map ``fEnv`` from function names to arity (num args)
    - computes the errors for each declaration given ``fEnv``
    - concatenates the resulting list of errors

Example: let's look for unbound variables and undefined functions.

.. code-block:: haskell

    wellFormedD :: FunEnv -> BareDecl -> [UserError]
    wellFormedD fEnv (Decl _ xs e _) = wellFormedE fEnv vEnv e
        where
            vEnv = addsEnv xs emptyEnv

and ``wellFormedE`` takes the function parameters and list of declared functions, and traverses the expression:
    - at each ``Let x e1 e2``, add ``x`` to ``vEnv`` and check ``e2``
    - at each var use ``Id x``, check if the ``x`` is in ``vEnv`` and if not, create an error
    - at each call ``App f es``, check if the function is in ``fEnv``

.. code-block:: haskell

    wellFormedE :: FunEnv -> Env -> Bare -> [UserError]
    wellFormedE fEnv vEnv0 e = go vEnv0 e
        where
            gos vEnv es               = concatMap (go vEnv) es
            go _ (Boolean {})         = []
            go _ (Number n l)         = []
            go vEnv (Id x l)          = unboundVarErrors vEnv x l
            go vEnv (Prim1 _ e _)     = go vEnv e
            go vEnv (Prim2 _ e1 e2 _) = gos vEnv [e1, e2]
            go vEnv (If e1 e2 e3 _)   = gos vEnv [e1, e2, e3]
            go vEnv (Let x e1 e2 _)   = go vEnv e1
                                     ++ go (addEnv x vEnv) e2
            go vEnv (App f es l)      = unboundFunErrors fEnv f l
                                     ++ gos vEnv es

**Quiz**: https://tiny.cc/cse110a-wellform-ind -> C

**Quiz**: https://tiny.cc/cse110a-wellform2-ind -> C (or E if you care about arg names being the same as func names)

**Quiz**: https://tiny.cc/cse110a-wellform3-ind -> B

**Quiz**: https://tiny.cc/cse110a-wellform4-ind -> A

Compiling
---------

Text ``=Parse>`` BareP ``=Check>`` BareP ``=Norm>`` AnfP ``=Tag>`` AnfTagP ``=CodeGen>`` Asm

.. code-block:: haskell

    type BareP   = Program SourceSpan
    type AnfP    = Program SourceSpan
    type AnfTagP = Program (SourceSpan, Tag)

Tagging
^^^^^^^
Now, the tag phase needs to tag function bodies too.

Norm
^^^^
Also, each body of each function is normalized to ANF. Additionally, the **arguments to function calls** need to be
in immediate form.

CodeGen
^^^^^^^
So now, how do we compile definitions and calls?

- Definitions: each definition is compiled into a labeled block of ``Asm`` that implements the body.
- Calls: each call of ``f(args)`` will execute the block labeled ``f``
    - what about parameters?

We need to use the stack to pass parameters (``EBP + 4 * (n + 1)``) and store local vars (``EBP - 4 * n``)

Note that params are at ``4 * (n+1)`` because the return address is at ``EBP+4``.

So, we need to:
    - ensure that esp and ebp are properly managed
    - compile the ``Body`` with initial ``Env`` mapping parameters to ``-2``, ``-3``, etc (i.e. ``-(n+1)``)

Calls
"""""

- Before the call, ``push`` the parameter values onto the stack in reverse order
- Call the appropriate function using its label
- After the call, clear the stack by incrementing esp (to get rid of the parameters)

Types
^^^^^

.. code-block:: haskell

    data Label =
        ...
        | DefFun Id         -- asm label for functions

    data Arg =
        ...
        | Sized Size Arg    -- sized arg

    data Sized = DWordPtr   -- usually everything is just a double word

Implementation
^^^^^^^^^^^^^^

.. code-block:: haskell

    -- compiles Programs, Decls, and Exprs respectively
    compileProg :: AnfTagP -> Asm
    compileDecl :: AnfTagD -> Asm
    compileExpr :: Env -> AnfTagE -> Asm

    compileBody :: Env -> AnfTagE -> Asm    -- helper to manage the C calling convention

.. code-block:: haskell

    compileProg :: AnfTagP -> Asm
    compileProg (Prog ds e)
        = compileBody emptyEnv e
        ++ concatMap compileDecl ds     -- the labels come after the main!

    -- decl
    compileDecl :: AnfTagD -> Asm
    compileDecl (Decl f xs e _)
        = ILabel (DefFun (bindId f))
        : compileBody (paramsEnv xs) e

    paramsEnv :: [Bind a] -> Env
    paramsEnv xs = fromListEnv (zip xids [-2, -3..])
        where
            xids = map bindId xs

    -- body
    compileBody :: Env -> AnfTagE -> Asm
    compileBody env e
        = entryCode e
        ++ compileExpr env e
        ++ exitCode
        ++ [IRet]

    entryCode :: AnfTagE -> Asm
    entryCode e = [ IPush (Reg EBP),
                    IMov (Reg EBP) (Reg ESP),
                    ISub (Reg ESP) (Const 4 * n) ]
        where
            n = countVars e

    exitCode :: Asm
    exitCode = [ IMov (Reg ESP) (Reg EBP),
                 IPop (Reg EBP) ]

    -- calls
    compileExpr :: Env -> AnfTagE -> Asm
    compileExpr env (App f vs _)
        = call (DefFun f) [param env v | v <- vs]

    param :: Env -> ImmE -> Arg
    param env v = Sized DWordPtr (immArg env v)

    call :: Label -> [Arg] -> [Instruction]
    call l as = [ IPush (Sized DWordPtr (argAsm a)) | a <- (reversed as) ]  -- push args in reverse order
              ++ [ ICall l,                                                 -- call
                   IAdd (Reg ESP) (Const (length as)) ]                     -- clear params from stack

Compiling Tail Calls
""""""""""""""""""""
What about really big recursion, like:

.. code-block:: haskell

    def sumTo(n):
        if n < 1:
            n
        else:
            n + sumTo(n - 1)

    sumTo(10000)

We can make this tail recursive (the recursive call is the final thing):

.. code-block:: haskell

    def loop(r, i):
        if (0 <= i):
            let rr = r + i,
                ii = i - 1
            in
                loop(rr, ii)
        else:
            r

    def sumTo(n):
        loop(0, n)

    sumTo(10000)

This lets us make the recursive call in place instead of having to make stack frames for each call.

Instead of using ``call`` to make the new call, we can just:

- move the call's arguments to the same stack position as the current args
- free current stack space by resetting esp/ebp
- jump back to the start of the function

.. code-block:: text

    mov eax, [ebp-8]    # overwrite i with ii
    mov [ebp+12], eax
    mov eax, [ebp-4]    # overwrite r with rr
    mov [ebp+8],  eax
    mov esp, ebp        # "free" stack frame
    pop ebp
    jmp def_loop        # jump to function start

But to do this, we need to:
    - identify tail calls in the source expr
    - compile the tail calls like this.

But let's not do that during codegen.

Text ``=Parse>`` BareP ``=Check>`` BareP ``=Norm>`` AnfP ``=Tag>`` AnfTagP ``=Tails>`` AnfTagTlP ``=CodeGen>`` Asm

.. code-block:: haskell

    type BareP   = Program SourceSpan
    type AnfP    = Program SourceSpan
    type AnfTagP = Program (SourceSpan, Tag)
    type AnfTagTlP = Program ((SourceSpan, Tag), Bool)  -- each call is "tail" or not
