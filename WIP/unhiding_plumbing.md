Adventures in unhiding the hidden plumbing
==========================================

This post is about a brutal, last-resort debugging technique that occasionally
saved me some trouble. It is as unsafe as it sounds, and a good debugger should
do a much better job of doing it. Unfortunately, Haskell doesn't have one, so
here we go.

We're going to use a simple trick to gain access to unexposed data in libraries.
All we need to have is access to the source code.

The raw version of this file is literate Haskell. Tag along for some interesting
~~segfaults~~ lessons!

Many of the types defined in `containers` are abstract, i.e. their constructors
are not exported. You cannot pattern-match against them, and you cannot
construct values directly. Instead, you have to rely on their API. But sometimes
you may actually want to have a look at the internals even though you're not
supposed to. How imbalanced is the internal tree structure of a `Map`? Did it
rebalance?

> import qualified Data.Map      as M
> import           Data.Int
> import           Unsafe.Coerce -- Yup



`unsafeCoerce`
--------------

The trick itself is quite simple, but let's briefly discuss what `unsafeCoerce`
is. It's a function to fool the typechecker:

```haskell
unsafeCoerce :: a -> b
```

What it does is to simply change the type of an expression, changing nothing
else. So if you wanted to implement the [fast inverse square root algorithm][fsqrt],
which brutally casts a float to an integer and back, that's the sort of thing
that `unsafeCoerce` allows you to do.

> hack :: (Int64 -> Int64) -> Double -> Double
> hack f = unsafeCoerce . f . unsafeCoerce

This will apply a function working on `Int64` to a `Double`, by simply looking
at the bit representation as if it as an `Int64`. As you may already guess, this
is as unsafe as C memory dereferencing, which is about as unsafe as a language
can get.



Peeking inside a `Map`
----------------------

The source code of `Data.Map.Map` is

```haskell
data Map k a  = Bin {-# UNPACK #-} !Size !k a !(Map k a) !(Map k a)
              | Tip
              deriving (Show)
type Size = Int
```

but the constructors are hidden. We can make them accessible by creating a
structure with identical memory layout, and then coercing a `Data.Map.Map` to
our own data type. So first, create an identical type in a module you have
access to:

> data Map k a  = Bin {-# UNPACK #-} !Size !k a !(Map k a) !(Map k a)
>               | Tip
>               deriving (Show)
> type Size = Int

The coercion is simple,

> Unhide the plumbing
> unplumb :: M.Map k a -> Map k a
> unplumb = unsafeCoerce
>
> -- Back to the proper Map
> plumb :: Map k a -> M.Map k a
> plumb = unsafeCoerce

and that's it! We can try this out with some test data:

> testMap :: M.Map Int Char
> testMap = M.fromList (zip [1..] "hello")

Let's have a look at the root node.

> rootSize = let Bin size _key _value _l _r = coerceMap testMap in size

This is 4, which says that the 'h' node has a total of four children in the
rest of the tree attached to it. What does the left subtree look like?

> leftTree' = let Bin _s _k _v left _right = coerceMap testMap in left

When you run this you get a segfault. Oh my, what happened? We coerced to a
`Map`, whose left subtree is a `Map` as well.






[fsqrt]: https://en.wikipedia.org/wiki/Fast_inverse_square_root]





import qualified Data.Map.Lazy as M
import           Unsafe.Coerce



-- Copied from Data.Map.Lazy
data Map k a  = Bin {-# UNPACK #-} !Size !k a !(Map k a) !(Map k a)
              | Tip
              deriving (Show)
type Size = Int



testMap :: M.Map Int Char
testMap = M.fromList (zip [1..] "hello")



-- Segfaults
leftSubtree :: Map Int Char
leftSubtree = let Bin _size _key _value left _right = unsafeCoerce testMap
              in left
