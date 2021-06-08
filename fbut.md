Frequently brought up topics in #haskell
========================================

This is a collection of various questions, statements, and antipatterns I've
encountered in Freenode's #haskell over the years. It's a lot like an FAQ,
except that the *F* stands for *frequently* instead of *someone thought this
might be worth mentioning*.



Contents
--------

1.  [`ByteString.Char8` is bad][TOC/bsbad]
2.  [I don't understand Monads][TOC/monads]
3.  [Tabs vs. spaces][TOC/tabspaces]
4.  [`(- 4)` is not `\x -> x - 4`][TOC/special-minus]
5.  [I'm looking for a good Regex library][TOC/regex]
6.  [`Show` is not for prettyprinting][TOC/show]
7.  [Imposing constraints on data types][TOC/constraint-types]
8.  [`seq` does not specify an evaluation order][TOC/seq]
9. [Where is `IO` defined?][TOC/io]
10. [Don't use ...][TOC/dont-use]
    - [`fail`][TOC/dont-use-fail]
    - [`read`][TOC/dont-use-read]
    - [`genericLength`][TOC/dont-use-genericlength]
    - [`unsafePerformIO`][TOC/dont-use-unsafeperformio]
    - [`head`, `isJust`, ...][TOC/dont-use-headtail]
    - [`nub`][TOC/dont-use-nub]
    - [`String`][TOC/dont-use-string]
11. [How to start learning Haskell][TOC/haskell-start]
12. [`f x = ...` is not `f = \x -> ...`][TOC/lambda-vs-normal]
13. [Reversed type class instances][TOC/reversed-instances]
14. [Folding direction of `foldl` and `foldr`][TOC/foldl-foldr]
15. [`($)` has special powers][TOC/special-dollar]
16. [Comment syntax][TOC/comment-syntax]
17. [`data`, `newtype`, `type`][TOC/data-newtype-type]
18. [Indentation and sensitive whitespace][TOC/sensitive-whitespace]
19. [What program should I write?][TOC/what-should-i-write]
20. [`undefined` is not Haskell’s `null`][TOC/undefined-null]



`ByteString.Char8` is bad
-------------------------

### Short version

"The short answer is 'any time you write `Data.ByteString.Char8` in your code,
your code is probably wrong'" - [*merijn on #haskell*][merijnquote]

### The problem

`ByteString` is a library for representing raw data, suitable for storing it or
sending it over the network. Raw data is seldomly what you actually want; it is
an intermediate form for the actual data you're trying to pack into single bytes.
The one crucial assumption about encoding and decoding data is that doing
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
party (and you should consider yourself in three months one of those).

To give you an analogy, suppose you have a number library for manipulating
`Int`, but it turns out that `(*)` is not commutative for numbers larger than
`100`. Would it be wise to say "I'll just use it for small numbers so it'll be
fine"? Is it OK to not catch exceptions that "should never happen"? Do you not
have airbags in your car because when you drive you're always very careful?

### The right way

Use `ByteString`, not `ByteString.Char8`. If what you want is a conversion
`String -> ByteString`, then use a serialization library such as
[Binary][Hackage/binary] that takes care of the conversion from `[Char]` to `[Word8]`
to `ByteString` and back. The same goes for `Text`, which can be serialized
using `Text.Encoding`.

`ByteString.Char8` has very few limited uses, for example if you're
communicating over a HTTP connection the headers are all ASCII. Using
`Char8` saves you from converting your literal `String`s in the code to
`ByteString` explicitly all the time, but note that HTTP requests can still
contain Unicode in their bodies, so HTTP isn't a ticket to using `Char8` in
general.



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
  This makes Regex more like a lexer, not a parser. You'll still have to convert
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
If you want to parse use parsers, for example [Parsec][Hackage/parsec] or
[Attoparsec][Hackage/attoparsec].



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
prettyprinting, there are other libraries, such as [`pretty`][Hackage/pretty] or
[`pretty-show`][Hackage/pretty-show] and `Text.Printf`.

Similar arguments apply to `Read`, which is the counterpart to `Show`: it's
meant to convert things generated by `Show` back to Haskell. It is not a
general `String -> Haskell` converter, which is what parsers are for (such as
[Parsec][Hackage/parsec] or [Attoparsec][Hackage/attoparsec]).



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



`seq` does not specify an evaluation order
------------------------------------------

The [`seq` function is defined by the following mathematical equations in the Haskell Report][Report/seq]:

```haskell
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
issue. In fact, `foldl'` not blowing up the heap (where `let` allocates) is an
optimization done by our dear friend, the sufficiently smart compiler, which
GHC is an example of.



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



Don't use ...
-------------

These functions have either every few use cases, or none at all. The former
means that unless you have a very good reason to use them, they are *wrong*.
(Not good reasons include that the types match conveniently, it's convenient,
anything involving "probably".)

### [`fail`][Hackage/fail]

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

### [`read`][Hackage/read]

`read` crashes at runtime on a parse error. Use `readMaybe` instead, which
has a `Maybe a` result.

### [`genericLength`][Hackage/genericLength]

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

### [`unsafePerformIO`][Hackage/unsafePerformIO]

`unsafePerformIO` is very useful in advanced Haskell, and very wrong
otherwise. Chances are you're looking for a very basic introduction to `IO`
based on `do` notation or chaining `>>=` ("bind").

In order to use `unsafePerformIO` safely, it's not enough to know how `IO`
roughly works. In addition to being pretty damn sure that you actually need a
function of type `IO a -> a`, you should also understand how your compiler
handles inlining, in which cases it evaluates expressions and how many times,
and how errors should be detectable in case you make one using this dangerous
function. Really, don't use it lightheartedly.

### [`head`][Hackage/head], [`tail`][Hackage/tail], [`isJust`][Hackage/isJust], [`isNothing`][Hackage/isNothing], [`fromJust`][Hackage/fromJust], ...

These should all be substituted by pattern matching. For one, they separate
structural code from code that does computations, and even more importantly,
the compiler knows when you forget to handle a case (when compiled with `-W`).

For example, instead of

```haskell
map f xs | null xs   = []
         | otherwise = f (head xs) : tail xs
```

you should write

```haskell
map f (x:xs) = f x : map f xs
map _ []     = []
```

### [`nub`][Hackage/nub]

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

### [`String`][Hackage/String]

`String` is literally a singly linked list of individual characters. To state
the obvious, this is a *terrible* way to represent text in general. `String`
suffers from the following problems:

- Its memory footprint is pretty bad.

    - One `(:)` cell per character, one machine word (typically 8 byte) each.
    - One pointer from the `(:)`'s first argument to the `Char` value of the
      unicode code point in memory. One word per character.
    - One pointer from the `Char` value to the memory address holding the raw
      value. One word per character.
    - Another pointer per `(:)` cell to point to the rest of the list to
      follow. One word per character.

    That makes a total overhead of 32 byte per character just for storage. (You
    need to store the actual raw `Char#` value as well of course.)

- No cache locality at all; pretty much every new character needs to be read
  from RAM, and not from one of the faster CPU caches.

- Many very common string operations are pretty expensive for lists,
  concatenation being the most obvious example (which runs in linear time of
  its first argument's length).

[`Text`][Hackage/text] is a mature library that has none of the problems `String`
has. It is preferrable in almost all scenarios where nontrivial string handling
is required.

And since `ByteString` is also often added to the string confusion:

- `String` is the simplest string type you could have in Haskell. It is
  convenient to use, since there are lots of functions working for lists in the
  standard library. You can use list comprehensions, you can pattern match.
- [`Text`][Hackage/text] is for when you want to do serious things with strings.
  Many people argue that it should be the default people use, instead of
  `String`.
- [`ByteString`][Hackage/bytestring] does not have much to do with strings; it's a
  sequence of bytes. It is what you would serialize your data to, what you read
  from a network socket or a raw file.



How to start learning Haskell
-----------------------------

The usual Haskell beginner books are [Hutton][Book/Hutton], [LYAH][Book/LYAH] (free to
read online) and [RWH][Book/RWH] (ditto). I recommend starting with Hutton or LYAH,
which cover the absolute basics better than RWH. The latter on the other hand
is something in between a practical reference guide for some libraries and an
introductory book. Some of the chapters use outdated libraries (the book is from
2008), but it's still conceptually right and definitely worth a read after
you've had a good look at the other books.

Hutton and RWH provide exercises to each chapter. Regardless of which book
you're actually reading, you should look these exercises up and solve them
whenever you feel you've learned enough to do so. Another good place to start
writing little functions is by implementing functions from
[Prelude][Hackage/prelude] and [Data.List][Hackage/Data.List].



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
foldl _ z []     = z
foldl f z (x:xs) = foldl f (f z x) xs -- x is the first list element,
                                      -- no others are considered.

foldr _ z []     = z
foldr f z (x:xs) = x `f` foldr f z xs -- dito
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



Comment syntax
--------------

### Line comments

Haskell has line comment syntax that is stranger than you might think. For some
reason, the developers wanted to allow operators starting with `--`, which
leads to a couple of odd special cases.

```haskell
-- This is a comment.
--- So is this.
------ And this.
----------------------------------------------------------------------------
-- Some people like to separate their code blocks with lines like these.

--. This is an operator called "--." and not a comment.
|-- This is an operator as well.
--| In particular, this is not a Haddock comment but an operator called "--|"
--  followed by some oddly human language looking code.
--^ Likewise.
```

The practical advice here is that you should surround your `--` in whitespace
to be sure it's a comment.

(Note how GitHub's parser displays this wrong in the markdown rendering
because of precisely these corner cases.)

### Block comments

Luckily, block comments are easy. They all look like `{- -}`, and they can be
nested. The absence of special cases here is that curly braces aren't part of
valid operators, so there is no room for ambiguity.



`data`, `newtype`, `type`
-------------------------

Defining new data types frequently confuses beginners.

- `data`: Define a new type that can have multiple fields and is
  distinguishable from other types by the typechecker.

    ```haskell
    data Bool    = False | True
    data Maybe a = Nothing | Just a
    data List  a = Nil | Cons a (List a)
    ```

  **If I could choose a better name, it would be `type` instead of `data`.**

- `type` defines a type synonym, and does not create a new type. It is used to
  make lengthy composite types more readable, or to provide convenient names
  for special use cases.

    ```haskell
    type String = [Char]
    type NumberOfSheep = Integer
    ```

  **My suggested better keyword for `type` would be `synonym`.**

- `newtype` sits between `type` and `data` and combines some of their desirable
  features.

  To the typechecker, `newtype`s look like `data` types.

    ```haskell
    -- A and B are distinct types that cannot be used interchangably.
    newtype A = A Int
    newtype B = B Int
    -- ==> A ≠ B

    -- Dito.
    data A = A Int
    data B = B Int
    -- ==> A ≠ B

    -- Type synonyms *can* be used interchangably.
    type A = Int
    type B = Int
    -- ==> A = B = Int
    ```

  Once typechecking is done however, the compiler can simply throw the
  `newtype` wrappers away, and work as if you had used simple `type` synonyms.
  This makes `newtype` a zero-cost abstraction in many scenarios.

  Typical uses for `newtype`s include defining type-safe aliases for existing
  types, or wrapping existing types so that they can be given different type
  class instances than the base type (e.g. `Sum`).

  **If I could rename the keyword, I'd call it `wrapper`.**



Indentation and sensitive whitespace
------------------------------------

Haskell’s grammar is usually written in a way that is sensitive to whitespace.
This is based on the concept of *layout blocks*. A layout block is a Haskell
source code unit, similar to `{ ... }` is a C or Java source code unit.

The first symbol (lexeme, if you prefer) after `where`, `let`, `do` `of` marks
the beginning of a layout block. If we mark the block using an ASCII box, it
would look like this:

```haskell
          +---------------+
case x of | Just y -> [y] | -- Block opened by the Just
          | Nothing -> [] |
          +---------------+
```

The lines following a new block can start in three positions:

  1. Same column as the layout block opener. In this case, it is made a new item
     of the block. Example:

     ```haskell
     case x of Just y -> [y]
               Nothing -> []
     ```

     The `Just` opens a layout block, and since the `Nothing` starts in the same
     column, it is another part of the list of alternatives.

  2. In a column that is further to the right than the block opener. In this
     case, it is as if we joined lines with the previous line. Example:

     ```haskell
     case x of Just y -> [y]
                 Nothing -> []
     ```

     This is a syntax error, since it is equivalent to

     ```haskell
     case x of Just y -> [y] Nothing -> []
     ```

  3. In a column that is further to the left than the block opener. This closes
     the previous block. Example:

     ```haskell
     case x of Just y -> [y]
             Nothing -> []
     ```

     The `Nothing` is *not* part of the `case` anymore, so it is a syntax error.
     (It can’t be part of a not-shown `case` further outside, because the `case`
     shown is indented less than it.)



What program should I write?
----------------------------

When learning a new programming language – Haskell or any other – we usually
really want to use our shiny new tool, but we don’t know what to build with it!
To solve this dilemma, here is the project I always recommend:

**Build a Brainfuck interpreter.**

Why?

  - Multiple simple components
  - Lots of room to extend them
  - Many important topics covered

If you’re not familiar with Brainfuck, it’s a very simple programming language
that you can probably learn in less than 5 minutes, but is pretty hard to write
programs in. It is however very instructional to implement it! [Everything you
need to know you can find in the Wikipedia
article.](https://en.wikipedia.org/wiki/Brainfuck) To give you a taste, here is
what Hello World looks like:

```brainfuck
++++++++++[>+++++++>++++++++++>+++>+<<<<-]  Yes, it’s hard to read, but
>++.>+.+++++++..+++.>++.<<+++++++++++++++.  you should implement it, not
>.+++.------.--------.>+.>.                 write programs in it! ;-)
                                            (Wikipedia has a more readable version.)
```

So, why that project? What’s in it for me, or rather – you? Well, the goal is
simple: write a server that the user can send Brainfuck sourcecode such as the
above to, and that runs the programs and sends back the reply.

To achieve this, you’ll need the following basic components:

  - **Parser:** Read a simple text format into logical blocks.
  - **Syntax tree:** Think about how to represent Brainfuck code in your
    interpreter.
  - **User interaction:** How to read user input, how to send back the result?

Depending on prior knowledge, this might take you anywhere from a couple of
minutes to a weekend. Don’t be afraid of the weekend end of the spectrum, you’ll
learn a *lot* in the process!

With that out of the way, we can go in many different directions:

  - **Web server:** Add a HTTP frontend, so that you can host your own
    interpreter over HTTP.
  - **Command-line tool:** Add a command line parser (many libraries available
    to do this) so the interpreter can be used from the terminal.
  - **Optimizer:** `+++---+-` does nothing! Why not remove it beforehand to
    speed up execution? What optimizations can you come up with?
  - **Rich intermediate representation:** `[-]` sets a cell to 0, by laboriously
    subtracting 1 repeatedly. Maybe the interpreter should have a special
    operation to just set things to a constant value?
  - **Code generators**
      - **Brainfuck:** You’ve optimized the input – why not give the
        user the optimized program back as output?
      - **C:** If you’re familiar with C, you can try generating a C program
        that represents the Brainfuck input. You can then run a C compiler over
        your generated code to get an executable.
      - **Native/LLVM:** This one is definitely not something for beginners,
        but if you’re already familiar with how to build a compiler to  machine
        code, why not add a code generator?

Up to now, we’ve already touched a very large programming spectrum, from super
low level to backend web programming. Here are some further ideas:

  - Make the web service scale! All of a sudden, 100 users connect concurrently.
    Parallelize your service to exploit that multicore CPU of yours!
  - Urk, some users give the program infinite loops! Maybe it should warn or
    abort if a number of steps were exceeded?
  - Add a GUI that allows watching/stepping through the program.
  - Remember past programs in some database, and let users re-run them by
    specifying e.g. their name or ID.



Undefined is not Haskell’s null
-------------------------------

(Technical note for those who know about such things: `undefined` will be used
synonymous to bottom/⊥ in the following.)

<!-- **Short rant.**

»Undefined is Haskell’s null« is something I’ve heard multiple times from people
that learned everything about Haskell from blog posts, as opposed to actually
using it. It’s one of the statements that make me angry, since people issuing it
are either vocal about their ignorance, or intellectually dishonest.

**End of rant.** -->

### What are they?

`null` is a special value that can have any type, or be assigned to any
variable. For example in Java, we can have a variable of type `Integer` that
does not contain an `Integer` as the type claims, but `null` instead.

`undefined` is the complete absence of a meaningful value, which can have
multiple reasons. An infinite loop has the value `undefined`, and so does a
function throwing an exception. In Haskell, we can have a variable of type
`Integer` that does not contain an `Integer` as the type claims, but is
`undefined` instead. Similarly, we can have a method in Java that claims to
return an `Integer` given no arguments, but fails to yield any answer, so it
also has value `undefined`.

These two sound similar, but there is really nothing they have in common at all
once you look at them in detail.



### What’s the problem?

In short, we can work with `null`, and we *should* avoid it. We cannot work with
undefined, and we *have to* avoid it. The core reason for this is that `null` is
checkable, while `undefined` is not.

Checkable means we can write a function to decide whether a value is `null` or
`undefined`. We can write such a function for null, e.g.

```javascript
isNull = x => x === null;
```

There is no such function in Haskell, since it does not have `null`. But `Maybe`
is a type that covers some of `null`’s (ab)use cases, and we can check for
whether something is not there,

```haskell
isNothing x = x == Nothing
```

We cannot, neither in Javascript nor in Haskell (nor in any other Turing
complete language) write a function that checks whether its argument is
`undefined`. The reason for this is that one way to be `undefined` is containing
an infinite loop and hence never returning a meaningful value, so checking
whether something is `undefined` would require us to solve the Halting problem.
We can check for some few instances of `undefined`, for example functions
throwing exceptions by wrapping them in try/catch or equivalent.

So, we *can* write a function that takes an arbitrary value and tells us whether
it’s `null` or not, but we *cannot* do the same for `undefined`.



### Who has it?

`null` is used by all large mainstream languages, strongly or weakly typed. It
is almost universally agreed on that `null` is a terrible idea language design
wise, yet it keeps popping up in new languages, presumably because programmers
tend to be very reluctant to learning new concepts. The biggest examples of at
least somewhat well-known languages that do not have `null` are Haskell and
Rust.

`undefined` is a placeholder value we assign to values that don’t produce a
meaningful result. Unbounded loops are `undefined`, exception throwers are
`undefined`. Any language that supports any of these constructs has `undefined`,
and all popular and even semi-popular languages do. One example of a language
that does not is (total) Idris, but it would be a stretch to even call it a
semi-semi popular language at this point.



### When is it valid?

Since `null` is unfortunately workable (remember, we can check for it), it
depends entirely on the programmer’s taste what to do with it. There are many
conventions about when or whether to use `null`, and they vary widely among
language ecosystems, companies, and even individual projects. This makes it
completely impossible to understand whether some code works without knowing the
culture of its authors.

`undefined` is very hard to work with, to the point where most programmers will
wonder what good it can be at all – since when would we ever want an infinite
loop? There are some few cases in which `undefined` is valid or even good code.
One is during prototyping, where it can serve as a »I’ll fill this out later«
value, much like a TODO comment that crashes the program, should the programmer
fail to fulfill the promise. Another is in dead code to please the typechecker,
when a branch of the program will not ever be taken, but the compiler fails to
recognize this and warns that, well, this branch might be taken. The popular
`assert` family of debugging functions also fall into this category.



### How to avoid it?

Avoiding `null` can be difficult, depending on the language. There are two
levels of avoidance. One is by self-discipline, by agreeing that `null` is bad
and should not be present in the codebase at all cost; static code analyzers can
(and probably should) be part of that self-discipline. The more important one is
by language design, since we don’t only use our own code, but use libraries
touched by many hands. Good language design means that it is easy to spot values
that might be `null`, so that we don’t have to play the »could it be `null`«
game. Java is famously bad at this, because it introduces an `Optional` type
that adds explicit nullability without removing `null` at all; Kotlin marks
variables of types that contain `null` explicitly.

Avoiding `undefined` in practice is fairly simple: we rarely write programs that
just hang in infinite loops producing nothing but CPU heat, and if we do, we’re
likely to catch those mistakes during testing. We try not to use exceptions too
much (especially in Haskell where they’re frowned upon).

Avoiding `undefined` in general is prohibitively hard, since we would have to
prove that none of the paths through a program might hang, or throw, or fail to
handle a certain case. One notable language that tries to push the boundaries
here is Idris, but it has a long way to go before becoming production ready.



[Book/Hutton]:             http://www.cs.nott.ac.uk/~gmh/book.html
[Book/LYAH]:               http://learnyouahaskell.com/
[Book/RWH]:                http://book.realworldhaskell.org/
[ghc-gadt-manual]:         http://www.haskell.org/ghc/docs/latest/html/users_guide/data-type-extensions.html#gadt
[Hackage/attoparsec]:      http://hackage.haskell.org/package/attoparsec
[Hackage/binary]:          http://hackage.haskell.org/package/binary
[Hackage/bytestring]:      http://hackage.haskell.org/package/bytestring/
[Hackage/Data.List]:       http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-List.html
[Hackage/fail]:            http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Monad.html#v:fail
[Hackage/fromJust]:        http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Maybe.html#v:fromJust
[Hackage/genericLength]:   http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-List.html#v:genericLength
[Hackage/head]:            http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-List.html#v:head
[Hackage/isJust]:          http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Maybe.html#v:isJust
[Hackage/isNothing]:       http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Maybe.html#v:isNothing
[Hackage/nub]:             http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-List.html#v:nub
[Hackage/parsec]:          http://hackage.haskell.org/package/parsec
[Hackage/prelude]:         http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html
[Hackage/pretty-show]:     http://hackage.haskell.org/package/pretty-show
[Hackage/pretty]:          http://hackage.haskell.org/package/pretty
[Hackage/read]:            http://hackage.haskell.org/packages/archive/base/latest/doc/html/Text-Read.html#v:read
[Hackage/String]:          http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-String.html#t:String
[Hackage/tail]:            http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-List.html#v:tail
[Hackage/text]:            http://hackage.haskell.org/package/text/
[Hackage/unsafePerformIO]: http://hackage.haskell.org/packages/archive/base/latest/doc/html/System-IO-Unsafe.html#v:unsafePerformIO
[instance-monad-io]:       https://github.com/ghc/packages-base/search?q=%22instance++Monad+IO++where%22&type=Code
[merijnquote]:             http://ircbrowse.net/browse/haskell?id=16225289&timestamp=1375966557#t1375966557
[newtype-io]:              https://github.com/ghc/packages-ghc-prim/search?q=newtype%20io
[primtype-realworld]:      https://github.com/ghc/ghc/search?q=%22primtype+RealWorld%22&type=Code
[primtype-state]:          https://github.com/ghc/ghc/search?q=%22primtype+State%23%22&type=Code
[regex-source]:            http://pypix.com/tools-and-tips/advanced-regular-expression-tips-techniques/
[Report/sections]:         https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-300003.5
[Report/seq]:              http://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1260006.2
[Report]:                  http://www.haskell.org/onlinereport/haskell2010/

[TOC/bsbad]:                    #bytestringchar8-is-bad
[TOC/comment-syntax]:           #comment-syntax
[TOC/constraint-types]:         #imposing-constraints-on-data-types
[TOC/data-newtype-type]:        #data-newtype-type
[TOC/dont-use-fail]:            #fail
[TOC/dont-use-genericlength]:   #genericlength
[TOC/dont-use-headtail]:        #head-tail-isjust-isnothing-fromjust-
[TOC/dont-use-nub]:             #nub
[TOC/dont-use-read]:            #read
[TOC/dont-use-string]:          #string
[TOC/dont-use-unsafeperformio]: #unsafeperformio
[TOC/dont-use]:                 #dont-use-
[TOC/foldl-foldr]:              #folding-direction-of-foldl-and-foldr
[TOC/haskell-start]:            #how-to-start-learning-haskell
[TOC/io]:                       #where-is-io-defined
[TOC/lambda-vs-normal]:         #f-x---is-not-f--x---
[TOC/monads]:                   #i-dont-understand-monads
[TOC/regex]:                    #im-looking-for-a-good-regex-library
[TOC/reversed-instances]:       #reversed-type-class-instances
[TOC/sensitive-whitespace]:     #sensitive-whitespace
[TOC/seq]:                      #seq-does-not-specify-an-evaluation-order
[TOC/show]:                     #show-is-not-for-prettyprinting
[TOC/special-dollar]:           #-has-special-powers
[TOC/special-minus]:            #--4-is-not-x---x---4
[TOC/tabspaces]:                #tabs-vs-spaces
[TOC/undefined-null]:           #undefined-is-not-haskells-null
[TOC/what-should-i-write]:      #what-program-should-i-write
