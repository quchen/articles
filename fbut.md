Frequently brought up topics in #haskell
========================================





Contents
--------

1. [`ByteString.Char8` is bad][toc-bsbad]
2. [``(a `op`)`` is not ``\x -> a `op` x``][toc-sections]
3. ["I don't understand Monads"][toc-monads]
4. [Tabs vs. spaces][toc-tabspaces]
5. [`(- 4)` is not `\x -> x - 4`][toc-special-minus]
6. [I'm looking for a good Regex library][toc-regex]
7. [`Show` is not for prettyprinting][toc-show]
7. [Imposing constraints on data types][toc-constraint-types]

[toc-bsbad]:            #bytestringchar8-is-bad
[toc-sections]:         #a-op-is-not-x---a-op-x
[toc-monads]:           #i-dont-understand-monads
[toc-tabspaces]:        #tabs-vs-spaces
[toc-special-minus]:    #--4-is-not-x---x---4
[toc-regex]:            #im-looking-for-a-good-regex-library
[toc-show]:             #show-is-not-for-prettyprinting
[toc-constraint-types]: #imposing-constraints-on-data-types






`ByteString.Char8` is bad
-------------------------

### Short version

"The short answer is 'any time you write `Data.ByteString.Char8` in your code,
your code is probably wrong'" - *merijn on #haskell*

### The problem

`ByteString` is a library for representing raw data, suitable for storing it or
sending it over the network. Raw data is seldomly what you actually want; it is
an intermediate form for the actual data you're trying to pack into single bytes
. The one crucial assumption about encoding and decoding data is that doing
them one after another gets you the original data back, i.e.

```haskell
decode . encode = id
```

This is where `ByteString.Char8` fails: when converting a `[Char]` to a
`ByteString.Char8`, all the `Char` are truncated silently to their first byte.
Truncation means loss of information, therefore

```haskell
unpack . pack /= id
```

Unfortunately, this is merely a remark and not a 36pt bold underlined warning
in the documentation.

### Why is this bad?

You might say "I'm only using ASCII data anyway, and the issue above only
exists for multi-byte Unicode". This is a very dangerous argument, because you
never know in what context your code might be used by an unsuspecting third
party (and you should consider yourself in 3 months one of those).

To give you an analogy, suppose you have a number library for manipulating
`Int`, but it turns out that `(*)` is not commutative for numbers larger than
`100`. Would it be wise to say "I'll just use it for small numbers so it'll be
fine"? Is it OK to not catch exceptions that "should never happen"? Do you not
have airbags in your car because when you drive you're always very careful?

### The right way

Use `ByteString`, not `ByteString.Char8`. If what you want is a conversion
`String -> ByteString`, then use a serialization library such as
[Binary][binary] that takes care of the conversion from `[Char]` to `[Word8]`
to `ByteString` and back. The same goes for `Text`, which can be serialized
using `ByteString.Encoding`.

`ByteString.Char8` has very few limited uses, for example if you're
communicating over a HTTP connections the commands are all ASCII, so using
`Char8` saves you from converting between `ByteString` and `String` explicitly
all the time. (Note that HTTP requests can still contain Unicode in their
bodies, so HTTP isn't a ticket to using `Char8` in general.)

[binary]: http://hackage.haskell.org/package/binary





``(a `op`)`` is not ``\x -> a `op` x``
--------------------------------------

These two forms are seemingly identical, but there is a subtle difference. The
first one is just sugar for `op a`, while the second one is a lambda (and not
direct application of `op`). This leads to different strictness properties in
the presence of ⊥:

```haskell
> let op = undefined

-- Section
> (() `op`) `seq` ()
>>> *** Exception: Prelude.undefined

-- Prefix
> (op ()) `seq` ()
>>> *** Exception: Prelude.undefined

-- Lambda
> (\x -> () `op` x) `seq` ()
>>> ()
```

The reason for this behaviour is that a lambda can be thought of as an
additional wrapper around the application of `op`, and this wrapper is already
WHNF. For this reason, `op` is never forced, and the `seq` terminates without
complaints.





"I don't understand Monads"
---------------------------

There are many articles about Monads and how to use them out there, and as many
articles pointing that out ("Monad tutorial fallacy"), and probably even more
meta-levels. Here is my practical advice for demystifying Monads.

A Haskell Monad is a typeclass, and typeclasses unify types that act alike in
some sense. For typeclasses like `Eq` that "alike-ness" is fairly obvious, but
for Monads it isn't. So what should you do? Use Monad instances, and don't
worry about the Monad part. How does `do` notation work for `Maybe`? What does
`>>=` do in this scenario? Now write some small example code that just *uses*
these features. Up next: how does `do` notation work for `Writer`? What does
`>>=` do in this scenario? Now write some small example code that just *uses*
these features. Up next: how does `do` notation work for `State`? What does
`>>=` do in this scenario? Now write some small example code that just *uses*
these features. You can see where this is going, but for the sake of it, here's
the essence of it: **Learn Monad instances separately without worrying about
how they are "monadic". After some time you will develop an intuition for what
`do`, `<-`, `return`, `>>=` etc. have in common.** And you guessed it, the
Monad typeclass unifies that common-ness. And that's how I think you should
approach Monads.

Here is the order in which I recommend looking at how to use standard Monad
instances:

1. `Maybe`; you can also have a look at `Either` which is pretty similar.
2. `State`, `Writer`.
3. `Reader`, `IO`. `IO` in particular is a good Monad to get an *intuitive*
   feeling for, as `IO` is primitive (i.e. hardcoded) and therefore the
   "I don't understand the implementation so I can't understand this one" is
   not an excuse.



Tabs vs. spaces
---------------

Short version: use spaces.

Long version: You can write valid Haskell with spaces, tabs, or anything in
between. The between part is a pain in the ass, but you can make
only-tab-indented code work. So why shouldn't you do that?

1. If you were referred to this document, you're most likely a beginner.
   Beginners make *lots* of mistakes with tabs and are surprised why their code
   breaks and annoy others with this on IRC. Don't be one of those guys.

2. Spaces look the same to everyone, tabs don't. Chances are your tabbed code
   looks like crap with different settings; if it doesn't you have lots of
   newlines at the appropriate locations so it looks like crap in the first
   place.

3. The Haskell community has decided to use spaces. All common libraries use
   spaces, and the only reason there are a few tabs left in GHC's source is
   because of potential merge conflicts when editing them all out in bulk.

If you have any further questions, feel free to contact me at
`tabs-vs-spaces@example.com`.


`(- 4)` is not `\x -> x - 4`
----------------------------

A single negative sign is a special case in Haskell syntax, and simplifies
entering negative numbers. Unfortunately this rule breaks sections with `(-)`.

```haskell
-- The number "-4".
a :: Num a => a
a = (-4)
-- or simply "a = -4"

-- The function "subtract 4 from the argument".
b :: Num a => a -> a
b = \x -> x - 4

-- A useful Prelude definition for the "subtract from" function.
subtract :: Num a => a -> a -> a
subtract a b = b - a

-- The "subtract 4" function written with the above.
c :: Num a => a -> a
c = subtract 4
```

So in summary, if you want a section with the `(-)` operator, use `subtract`
like in example `c`. A final word of caution, using `subtract` in infix looks
reasonable, but produces wrong (negative) results due to reversed arguments -
``3 `subtract` 1`` is `-2`.



I'm looking for a good Regex library
------------------------------------

No. Stop. What you want to do is parse something, right? Use a parser! Regex is
widely used in other languages a lot, but *very* unpopular in Haskell:

- Regex is very hard to read once written.
- Writing Regex is error-prone, and the lack of readability makes it hard to
  debug them.
- Even if correct, the lack of readability makes them unmaintainable.
- Regex can split and transform text, but you'll always get out text again.
  This makes Regex more like a lexer, not a parser. You'l still have to convert
  your Regex chunks into actual data afterwards.

Want an example? Here's a regex to check US phone numbers in Python
([source][regex-source]):

```python
r'^(1[-\s.])?(\()?\d{3}(?(2)\))[-\s.?\d{3}[-\s.]?\d{4}$'
```

Actually no, there's a something missing. First task, find it. Afterwards, you
may object that the code lacks documentation, so of course it's unreadable.
Split in multiple lines, use comments:

```python
r'^'
r'(1[-\s.])?' # optional '1-', '1.' or '1'
r'(\()?'      # optional opening parenthesis
r'\d{3}'      # the area code
r'(?(2)\))'   # if there was opening parenthesis, close it
r'[-\s.]?'    # followed by '-' or '.' or space
r'\d{3}'      # first 3 digits
r'[-\s.]?'    # followed by '-' or '.' or space
r'\d{4}$'     # last 4 digits
```

What's the back reference to getting the area code again? The answer is don't
use Regex. If you want to do dirty hacking, Regex is the right tool for the job.
If you want to parse use parsers, for example [Parsec][parsec] or
[Attoparsec][attoparsec].



[regex-source]: http://pypix.com/tools-and-tips/advanced-regular-expression-tips-techniques/
[parsec]: http://hackage.haskell.org/package/parsec
[attoparsec]: http://hackage.haskell.org/package/attoparsec



`Show` is not for prettyprinting
--------------------------------

Sometimes the output of a `Show` instance looks like it could be displayed in a
human-friendlier way; `Set` for example displays as `fromList [1,2,3]`, and
derived `Show` instances account for all encountered constructors even. This
may look not very pretty or contain a lot of redundancy, so that the question
of how to generate a prettier `Show` instance for something comes up.

As it turns out, this question is wrong: `Show` is not for prettyprinting, it's
for converting things to `String`, often in a way where the resulting `String`
is valid Haskell and could be re-inserted into code. Because of this,
**`Show` is first and foremost a debugging class** to have a quick glance at
some data without losing any information or introducing ambiguities. For
prettyprinting, there are other libraries, such as [`pretty`][pretty] or
[`pretty-show`][pretty-show] and `Text.Printf`.

[pretty]:      http://hackage.haskell.org/package/pretty
[pretty-show]: http://hackage.haskell.org/package/pretty-show

Similar arguments apply to `Read`, which is the counterpart to `Show`: it's
meant to convert things generated by `Show` back to Haskell. It is not a
general `String -> Haskell` converter, which is what parsers are for (such as
[Parsec][parsec] or [Attoparsec][attoparsec] as mentioned in a previous
section).



Imposing constraints on data types
----------------------------------

Sometimes, it might seem useful to require a data type to be constrained to a
type class, for example you might want to require an `Ord` constraint for a
list that's always sorted ascending like so:

```haskell
data Ord a => OrdList a = Nil | a :< OrdList a
-- (Non-legal Haskell, but possible using the deprecated `DatatypeContexts`
-- GHC extension.)
```

This is usually not a good idea, because the `Ord` constraint would be required
by *all* functions working with an `OrdList`, regardless of whether it actually
needs a comparison. For example, the `ordLength` function would look like this:

```haskell
ordLength :: Ord a => OrdList a -> Int
ordLength Nil = 0
ordLength (_ :< xs) = 1 + ordLength xs
```

Note that this does not make use of `Ord` anywhere, yet the type signature
demands it. Using this function inside another one would maybe propagate the
`Ord` constraint up and cause even more unnecessary constraints.

The beginner's way of having an ordered list is to **put the constraints in the
functions, not in the data declaration**, like so:

```haskell
data OrdList a = Nil | a :< OrdList a
infixr 5 :<

ordLength :: OrdList a -> Int
ordLength Nil = 0
ordLength (_ :< xs) = 1 + ordLength xs

-- Prepend an element, fail if it doesn't fit the ordering
cons :: Ord a => a -> Maybe (OrdList a)
cons x Nil = x :< Nil
cons x (y :< ys) | x <= y    = Just (x :< y :< ys)
                 | otherwise = Nothing

-- More API here
```

To make it impossible to construct ill-formed `OrdList`s, one could export only
operations that cannot violate `OrdList`s invariant (that the elements are in
ascending order).

A more advanced way of constructing the `OrdList` would be by using generalized
algebraic data types (GADTs), enabled by the
[`GADTs` GHC extension][ghc-gadt-manual] (which is not deprecated and widely
used), enabling the following:

```haskell
{-# LANGUAGE GADTs #-} -- GHC extension, non-standard Haskell!

data OrdList a where
      Nil  :: OrdList a
      (:<) :: Ord a => a -> OrdList a -> OrdList a

ordLength :: OrdList a -> Int -- No Ord constraint!
ordLength Nil = 0
ordLength (_ :< xs) = 1 + ordLength xs
```

However, GADTs are a chapter on their own (and non-standard Haskell at that);
I merely mention this for completeness sake here. For basic usage, follow the
advice before: constrain functions, not the data declarations.

[ghc-gadt-manual]: http://www.haskell.org/ghc/docs/latest/html/users_guide/data-type-extensions.html#gadt




`seq` does not specify an evaluation order
==========================================

[The `seq` function is defined by the following equations in the Haskell Report][haskell-report-seq]:

```
seq ⊥ x = ⊥
seq y x = x   (if y ≠ ⊥)
```

Any function that satisfies these properties is a valid implementation of `seq`. In particular, no evaluation order is specified; in other words, implementations can choose whether to evaluate `seq x y` by evaluating `x` first, `y` first, or even choosing randomly. In case such an order is desirable, there is the `pseq` function from Control.Concurrent, which guarantees evaluation of the first parameter first. These would all be valid implementations for `seq`:

```haskell
-- Evaluate x first
seq1 x y = x `pseq` y

-- Evaluate y first
seq2 x y = y `pseq` x `seq1` y

-- Random choice
seqR | randomBool = seq1
     | otherwise  = seq2
     where randomBool = unsafePerformIO (randomRIO (False, True))
```

It is worth noting that evaluating `seq (error "x") (error "y")` allows inspection of which argument is actually evaluated first. However, the errors are identical from within the program's perspective; it takes an intervention of the runtime to extract anything useful from it.

[haskell-report]: http://www.haskell.org/onlinereport/haskell2010/
[haskell-report-seq]: http://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1260006.2