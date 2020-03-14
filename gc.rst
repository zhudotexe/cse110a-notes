Garbage Collection
==================

Let's look at some examples:

Example 1
^^^^^^^^^
Garbage at end:

.. code-block:: haskell

    let x = (1, 2),
        y = let tmp = (10, 20)
            in tmp[0] + tmp[1],
        p0 = x[0] + y,
        p1 = x[1] + y
    in
        (p0, p1)

Here, the variable ``tmp`` is unused after the declaration of ``y``.

To determine if something is garbage, check if we have any references to it.

Example 2
^^^^^^^^^
Garbage in the middle:

.. code-block:: haskell

    let y = let tmp = (10, 20)
            in tmp[0] + tmp[1],
        x = (1, 2),
        p0 = x[0] + y,
        p1 = x[1] + y
    in
        (p0, p1)


Now, the tuple ``(10, 20)`` is garbage, but it's not at the top of the heap anymore!

.. code-block:: diff

      0x10:
    + 0x0c: 2
    + 0x08: 1
    + 0x04: 20
    + 0x00: 10

We can redefine garbage as anything on the heap that is **not reachable from the stack**.

.. code-block:: diff

      0x10:
    + 0x0c: 2
    + 0x08: 1
    ! 0x04: 20
    ! 0x00: 10

So we can copy the live cells into the garbage, but now our pointers on the stack are pointing to the wrong place.

.. code-block:: diff

      0x10:
      0x0c:
      0x08:
    + 0x04: 2
    + 0x00: 1

Example 3
^^^^^^^^^
Garbage in the middle with stack

.. code-block:: haskell

    def foo(p, q):
        let tmp = (p, q)
        in tmp[0] + tmp[1]

    let y = foo(10, 20),
        x = (y, y+1),
        z = foo(100, 200)
    in
        x[0] + z

When we compact memory on the heap, we:

1. compute the forward addrs
2. redirect the stack pointers
3. compact cells on the heap

Example 4
^^^^^^^^^
Recursive data

.. code-block:: haskell

    def range(i, j):
        if (j <= i): false else: (i, range(i+1, j))

    def sum(l):
        if l == false: 0 else: l[0] + sum(l[1])

    let t1 =
            let l1 = range(0, 3)
            in sum(l1)
      , l = range(t1, t1 + 3)
    in
        (1000, l)

Heap at end of ``range(0, 3)``:

.. code-block:: diff

      0x2c:
      0x28:
      0x24:
      0x20:
      0x1c:
      0x18:
    + 0x14: 0x09
    + 0x10: 0
    + 0x0c: 0x01
    + 0x08: 1
    + 0x04: false
    + 0x00: 2

Heap at end of ``range(t1, t1 + 3)``:

.. code-block:: diff

    + 0x2c: 0x21
    + 0x28: 3
    + 0x24: 0x19
    + 0x20: 4
    + 0x1c: false
    + 0x18: 5
      0x14: 0x09
      0x10: 0
      0x0c: 0x01
      0x08: 1
      0x04: false
      0x00: 2

But now, when we try and allocate the final tuple, we're OOM!

And also, our stack only has the pointer ``0x29`` but there's more mem used than that!

So now, we need to:

1. mark the live addrs
2. compute the forward addrs
3. redirect the stack **and heap** pointers
4. compact cells on the heap
