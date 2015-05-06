Frequently brought up topics in #haskell
========================================

This is a collection of various questions, statements, and antipatterns I've
encountered in Freenode's #haskell over the years. It's a lot like an FAQ,
except that the *F* stands for *frequently* instead of *someone thought this
might be worth mentioning*.





Contents
--------

1.  [`ByteString.Char8` is bad]                  [toc-bsbad]
2.  [``(a `op`)`` is not ``\x -> a `op` x``]     [toc-sections]
3.  [I don't understand Monads]                  [toc-monads]
4.  [Tabs vs. spaces]                            [toc-tabspaces]
5.  [`(- 4)` is not `\x -> x - 4`]               [toc-special-minus]
6.  [I'm looking for a good Regex library]       [toc-regex]
7.  [`Show` is not for prettyprinting]           [toc-show]
8.  [Imposing constraints on data types]         [toc-constraint-types]
9.  [`seq` does not specify an evaluation order] [toc-seq]
10. [Where is `IO` defined?]                     [toc-io]
11. [Don't use ...]                              [toc-dont-use]
    - [`fail`]                                   [toc-dont-use-fail]
    - [`read`]                                   [toc-dont-use-read]
    - [`genericLength`]                          [toc-dont-use-genericlength]
    - [`unsafePerformIO`]                        [toc-dont-use-unsafeperformio]
    - [`head`, `isJust`, ...]                    [toc-dont-use-headtail]
    - [`nub`]                                    [toc-dont-use-nub]
12. [How to start learning Haskell]              [toc-haskell-start]
13. [`f x = ...` is not `f = \x -> ...`]         [toc-lambda-vs-normal]
14. [Reversed type class instances]              [toc-reversed-instances]
15. [Folding direction of `foldl` and `foldr`]   [toc-foldl-foldr]
15. [`($)` has special powers]                   [toc-special-dollar]



[toc-bsbad]:                    #bytestringchar8-is-bad
[toc-sections]:                 #a-op-is-not-x---a-op-x
[toc-monads]:                   #i-dont-understand-monads
[toc-tabspaces]:                #tabs-vs-spaces
[toc-special-minus]:            #--4-is-not-x---x---4
[toc-regex]:                    #im-looking-for-a-good-regex-library
[toc-show]:                     #show-is-not-for-prettyprinting
[toc-constraint-types]:         #imposing-constraints-on-data-types
[toc-seq]:                      #seq-does-not-specify-an-evaluation-order
[toc-io]:                       #where-is-io-defined
[toc-dont-use]:                 #dont-use-
[toc-dont-use-fail]:            #fail
[toc-dont-use-read]:            #read
[toc-dont-use-genericlength]:   #genericlength
[toc-dont-use-unsafeperformio]: #unsafeperformio
[toc-dont-use-headtail]:        #head-tail-isjust-isnothing-fromjust-
[toc-dont-use-nub]:             #nub
[toc-haskell-start]:            #how-to-start-learning-haskell
[toc-lambda-vs-normal]:         #f-x---is-not-f--x---
[toc-reversed-instances]:       #reversed-type-class-instances
[toc-foldl-foldr]:              #folding-direction-of-foldl-and-foldr
[toc-special-dollar]:           #-has-special-powers





`ByteString.Char8` is bad
-------------------------

### Short version

"The short answer is 'any time you write `Data.ByteString.Char8` in your code,
your code is probably wrong'" - [*merijn on #haskell*][merijnquote]

[merijnquote]: http://ircbrowse.net/browse/haskell?id=16225289&timestamp=1375966557#t1375966557

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
using `Text.Encoding`.

`ByteString.Char8` has very few limited uses, for example if you're
communicating over a HTTP connections the headers are all ASCII. Using
`Char8` saves you from converting your literal `String`s in the code to
`ByteString` explicitly all the time, but note that HTTP requests can still
contain Unicode in their bodies, so HTTP isn't a ticket to using `Char8` in
general.

[binary]: http://hackage.haskell.org/package/binary





``(a `op`)`` is not ``\x -> a `op` x``
--------------------------------------

These two forms are seemingly identical, but there is a subtle difference in
GHC. The first one is just sugar for `op a`, while the second one is a lambda
(and not direct application of `op`). This leads to different strictness
properties in the presence of ⊥:

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





I don't understand Monads
-------------------------

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
between. You can write valid programs with all of these styles, so why stick to
spaces?

1. If you were referred to this document, you're most likely a beginner.
   Beginners make *lots* of mistakes with tabs and are surprised why their code
   breaks, since Haskell is sensitive to whitespace.

2. Spaces look the same to everyone, tabs don't. Chances are your tabbed code
   looks bad with different settings for tab size; if it doesn't you have
   lots of newlines at the appropriate locations so it looks bad in the first
   place.

3. **The Haskell community has decided to use spaces.** All common libraries use
   spaces, and as of 7.10, GHC's Haskell source is fully de-tabbed, and the
   compiler issues a warning if there are literal tabs in source files.





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
-- Note the switched order of a and b.
subtract :: Num a => a -> a -> a
subtract a b = b - a

-- The "subtract 4 from the argument" function written with the above.
c :: Num a => a -> a
c = subtract 4
```

So in summary, if*f* you want a section with the `(-)` operator, use `subtract`
like in example `c`.

A final word of caution, using `subtract` in infix looks reasonable, but
produces wrong (negative) results due to reversed arguments - ``3 `subtract` 1``
is `-2`.





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
needs a comparison, and in particular it would eliminate *no* `Ord` constraints
on any function that uses `OrdList` compared to how an non-`Ord` list would
work. For example, the `ordLength` function would look like this:

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
------------------------------------------

[The `seq` function is defined by the following mathematical equations in the Haskell Report][haskell-report-seq]:

```
seq ⊥ x = ⊥
seq y x = x   (if y ≠ ⊥)
```

Any function that satisfies these properties is a valid implementation of `seq`.
In particular, as mathematical equations, no evaluation order is specified:
implementations can choose whether to evaluate `seq x y` by evaluating `x`
first, `y` first, or even choosing randomly. In case such an order is
desirable, there is the `pseq` function from Control.Concurrent, which
guarantees evaluation of the first parameter first. These would all be valid
implementations for `seq`:

```haskell
-- Evaluate x first
seq1 x y = x `pseq` y

-- Evaluate y first
seq2 x y = y `pseq` x `seq1` y

-- Random choice
seqR | randomBool = seq1
     | otherwise  = seq2
     where randomBool = unsafePerformIO randomIO
```

It is worth noting that evaluating `seq (error "x") (error "y")` seemingly
allows inspection of which argument is actually evaluated first, since only one
of the errors is printed. This is a red herring though: the compiler may (and
does!) choose which argument to evaluate first as an optimization, so there
really is no guarantee of evaluation order even if the above simple test always
displays the "x" error. Even worse, that behaviour can change as the compiler
pleases; it would not be wrong to crash with "x" in August, and with "y"
during all the other months. In other words, do not rely on `seq` having an
evaluation order even if you're absolutely sure you found it out using some
voodoo magic.

[haskell-report]: http://www.haskell.org/onlinereport/haskell2010/
[haskell-report-seq]: http://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1260006.2



One rather strange consequence of this is the fact that functions like `foldl'`
do not *guarantee* no space leaks even when applied correctly! This would be a
valid reduction strategy:

```haskell
  foldl' (+) 0 [1,2,3]

= let x = 0 + 1
  in  x `seq` foldl' (+) x [2,3]

= let x = 0 + 1
  in x `seq`
      let y = x + 2
      in y `seq` foldl' (+) y [3]

= let x = 0 + 1
  in x `seq`
      let y = x + 2
      in y `seq`
          let z = y + 3
          in z `seq` foldl' (+) z []

= let x = 0 + 1
  in x `seq`
      let y = x + 2
      in y `seq`
          let z = y + 3
          in z `seq` z

= let x = 0 + 1
  in x `seq`
      let y = x + 2
      in y `seq`
          let z = y + 3
          in z

-- And now it's business as usual
```

As you can see, the length of the expression grows as we walk down the list,
despite the fact that one would expect the `seq` to take care of that sort of
issue. In fact, `foldl'` not blowing up the stack is an optimization done by
our dear friend, the sufficiently smart compiler, which GHC is an example of.






Where is `IO` defined?
----------------------

This question is hard to answer for multiple reasons.

1. `IO` is all over the place.
2. Looking at the userland definitions of `IO` most likely doesn't answer
   basic questions about it.
3. A lot of `IO` is primitive and hardwired into the GHC source.

In any case, here are a couple of links to some interesting definitions around
`IO`. I'm providing Github search links so these are somewhat robust against
a changing code base.

Definition          | Location
--------------------|-----------------------------------------------------------
`newtype IO`        | [`libraries/ghc-prim/GHC/Types.hs`][newtype-io]
`primtype State#`   | [`compiler/prelude/primops.txt.pp`][primtype-state]
`RealWorld`         | [`compiler/prelude/primops.txt.pp`][primtype-realworld]
`instance Monad IO` | [`libraries/base/GHC/Base.lhs`][instance-monad-io]

[newtype-io]: https://github.com/ghc/packages-ghc-prim/search?q=newtype%20io
[primtype-state]: https://github.com/ghc/ghc/search?q=%22primtype+State%23%22&type=Code
[primtype-realworld]: https://github.com/ghc/ghc/search?q=%22primtype+RealWorld%22&type=Code
[instance-monad-io]: https://github.com/ghc/packages-base/search?q=%22instance++Monad+IO++where%22&type=Code





Don't use ...
-------------

These functions have either every few use cases, or none at all. The former
means that unless you have a very good reason to use them, they are *wrong*.
(Not good reasons include that the types match conveniently, it's convenient,
anything involving "probably".)

### `fail`
`fail` from the `Monad` typeclass is a mistake for multiple reasons. It
prefers `String` over better text representations, `Monad` has nothing to do
with text anyway, and worst of all, many monads *cannot* have an
implementation of `fail`. As a rule of thumb, all `Monad`s that are not also
`MonadPlus` have a crash baked in because `fail` does simply not exist for
them.

Now turn back to Haskell's type system: If a type signature says `Monad m`,
the function should work for any `Monad` `m`. If the function uses `fail` it
does not, hence the function is partial, and partial functions in Haskell are
bad.

There are some valid use cases of `fail` though: when you're in monadic code
that is specific to one instance that supports it. For example, using `fail` is
safe in `Data.Binary`'s `Get`, and the API does not expose a dedicated
failing function.

### `read`
`read` crashes at runtime on a parse error. Use `readMaybe` instead, which
has a `Maybe a` result.

### `genericLength`
`genericLength` is literally the naive `1 + length rest` implementation,
and nobody is quite sure why it is in the standard library. `genericLength`
uses O(n) stack space so it can overflow, which is just awful behaviour. If
you need an `Integer` version of `length`, use `fromIntegral . length`, which
runs in constant stack space and is well-optimized. (The *one* valid use case
for `genericLength` is for lazy nats, which nobody ever uses.)

One person mentioned that `genericLength` works for lists with even larger
numbers of elements than `maxBound :: Int`. Assuming a 4-byte `Int` on a system,
that single list would have to be a little over 16 GiB in memory (each entry
is an `Int` value, and an `Int` pointer to the next value). At this point,
lists are probably not the right representation for your data.

### `unsafePerformIO`
`unsafePerformIO` is very useful in advanced Haskell, and very wrong
otherwise. Chances are `>>=` is what you want.

### `head`, `tail`, `isJust`, `isNothing`, `fromJust`, ...
These should all be substituted by pattern matching. For one, they separate
structural code from code that does computations, and even more importantly,
the compiler knows when you forget to handle a case (when compiled with `-W`).

### `nub`
`nub` has terrible performance (quadratic), since its type is overly general,
requiring only an `Eq` constraint. If your data satisfies `Ord`, then you can
implement much better algorithms that run in O(n*log(n)) time.

```haskell
nub1, nub2, nub3 :: Ord a -> [a] -> [a]

-- Naive implementation; has to traverse entire list before
-- returning a result, does not maintain input order
nub1 = map head . group . sort
-- Bonus question: why is using 'head' safe here?
-- This is one of the rare occasions where using it
-- is not a mistake.

-- Using Data.Set and explicit recursion. This is very similar
-- to the standard 'nub', but uses a Set instead of a List
-- to cache previously encountered elements.
nub2 = go Set.empty where
      go [] _ = []
      go (x:xs) cache
            | x `Set.member` cache = go xs cache
            | otherwise            = x : go xs (Set.insert x cache)

-- nub2, implemented as a fold. Coming up with this yourself is
-- a good exercise, try doing it before reading the code!
nub3 xs = foldr go (const []) xs Set.empty where
      go x xs cache
            | x `Set.member` cache = xs cache
            | otherwise            = x : xs (Set.insert x cache)
```





How to start learning Haskell
-----------------------------

The usual Haskell beginner books are [Hutton][hutton], [LYAH][lyah] (free to
read online) and [RWH][rwh] (ditto). I recommend starting with Hutton or LYAH,
which cover the absolute basics better than RWH. The latter on the other hand
is something in between a practical reference guide for some libraries and an
introductory book. Some of the chapters use outdated libraries (the book is from
2008), but it's still conceptually right and definitely worth a read after
you've had a good look at the other books.

Hutton and RWH provide exercises to each chapter. Regardless of which book
you're actually reading, you should look these exercises up and solve them
whenever you feel you've learned enough to do so. Another good place to start
writing little functions is by implementing functions from [Prelude][prelude]
and [Data.List][data.list].


[hutton]: http://www.cs.nott.ac.uk/~gmh/book.html
[lyah]: http://learnyouahaskell.com/
[rwh]: http://book.realworldhaskell.org/
[prelude]: http://hackage.haskell.org/package/base-4.7.0.0/docs/Prelude.html
[data.list]: http://hackage.haskell.org/package/base-4.7.0.0/docs/Data-List.html





`f x = ...` is not `f = \x -> ...`
----------------------------------

Although theory tells us these two should be identical, there are some subtle
differences between the two, in particular in GHC.

- The Haskell Report demands a difference between the two; refer to the section
  about the Monomorphism Restriction for further information.

- GHC only inlines fully applied functions, i.e. when all parameters in the
  definition are filled with values.

  ```haskell
  f x = <expr(x)>

  aaa    = f                   -- No inlining

  bbb  x = f x                 -- f may be inlined to ...
  bbb' x = <expr(x)>           -- ... this

  -------------------------------------------------------

  f = \x -> <expr(x)>

  sss   = f                    -- f may be inlined to ...
  sss'  = \x -> <expr(x)>      -- ... this

  ttt  x = f x                 -- f may be inlined to ...
  ttt' x = (\y -> <expr(y)>) x -- ... this
  ```

- `where` clauses span over the current definition, so parameters have different
  scope.

  ```haskell
  f    x =  <expr> where <decls> -- x is in scope in <decls>
  f = \x -> <expr> where <decls> -- but here it's not
  ```

- Sharing of values in `where` clauses is different.

  ```haskell
  f    x =  <expr> where <decls> -- A function that is re-evaluated on every
                                 -- invocation, including the <decls>. This may
                                 -- be improved by compiler optimizations
                                 -- automatically, but better not rely on it.
  f = \x -> <expr> where <decls> -- A constant that has a function as its value.
                                 -- Since the "where" spans over the entire
                                 -- constant, it does not need to be
                                 -- recalculated on every invocation.
  ```

  This behaviour becomes a little more difficult in the presence of typeclasses;
  for brevity's sake, consider a typeclass as an implicit argument passed
  similar to the `x` in the first case above.





Reversed type class instances
-----------------------------

It is a common misconception that parent classes cannot be defined using their
child classes. For example, I've often heard the phrase "When `Applicative`
becomes a parent class of `Monad` I can't define `Applicative` using `Monad`'s
`ap` anymore, which is very convenient". This is incorrect! The following code
compiles and is well-behaved (with and without `Applicative => Monad`):

```haskell
import Control.Applicative
import Control.Monad

newtype Id a = Id a

-- Functor defined using Applicative
instance Functor Id where
      fmap = liftA

-- Applicative defined using Monad
instance Applicative Id where
      pure = Id
      (<*>) = ap

-- Monad defined using Applicative
instance Monad Id where
      return = pure
      Id x >>= f = f x
```

Another example, `Eq` in terms of `Ord`:

```haskell
newtype Nat = Z | S Nat

instance Eq Nat where
      a == b = compare a b == EQ

instance Ord Nat where
      compare (S x) (S y) = compare x y
      compare (S _)  Z    = GT
      compare  Z     Z    = EQ
      compare  Z    (S _) = LT
```





Folding direction of `foldl` and `foldr`
----------------------------------------

Both functions traverse a list from left to right. To stress it some more, any
angle under which this seems not to be the case is *wrong*.

In each step, both functions look at the head of the list (if present)
exclusively, and combine it with something else using the stepping function.

```haskell
foldl _ z [] = z
foldl f z (x:xs) = foldl f (f z x) xs -- x is the first list element,
                                      -- no others are considered.

foldr _ z [] = z
foldr f z (x:xs) = z `f` foldr f z xs -- dito
```





`($)` has special powers
------------------------

The definition you can find in the Prelude

```haskell
($) :: (a -> b) -> a -> b
f $ x = f x
```

isn't really all there is to the function. GHC's current typechecker requires
some aid to work with Rank-2-types occasionally, so GHC contains a hack that
gives `($)` special powers in that context.

```haskell
runST (return ())     -- OK
runST $ return ()     -- OK
($) runST (return ()) -- Type error. Wat
```

This special `($)` behaviour is very useful in the presence of the
`f . g . h $ do {...}` idiom, but leads to a confusing error message.

The reason for this is that in order to check the type of `($)`

```haskell
($) :: (a -> b) -> a -> b
```

GHC has to unify `forall s. ST s c` with `a`, but then the first and second
`a` in `($)`'s type will have two different `s`, as in

```haskell
($) :: ((forall s. ST s c) -> b) -> (forall s. ST s c) -> b
```

But note that both the `forall`s close over their `s` argument, so the above
is equivalent to

```haskell
($) :: ((forall s. ST s c) -> b) -> (forall t. ST t c) -> b
```

Because of this, GHC *cannot* unify the two terms as demanded by `($)`'s type
signature, and the typecheck fails.
