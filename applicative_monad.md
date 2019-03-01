Haskell 2014: `Applicative => Monad` proposal (AMP)
===================================================

Haskell calls a couple of historical accidents its own. While some of them,
such as the "number classes" hierarchy, can be justified by pragmatism or lack
of a strictly better suggestion, there is one thing that stands out as, well,
not that: Applicative not being a superclass of Monad.

I will use the abbreviation *AMP* for the "`Applicative => Monad` Proposal".
Why do I think this is a particularly good time for the AMP? Haskell 2014 just
got a committee, that's why. We have a chance of actually going through with
this all the way, and the response to someone attempting to tackle this was
quite encouraging as well.

If you wish to contact me, I'm *dluposchainsky at gmail dot com*, or *quchen*
on Freenode.






Outline
-------

The rationale behind this proposal's contents is as follows:

1. **Break as little code as possible.** For example, do not move `return` to
   `Applicative` and remove `pure`. Instead, leave `return` in `Monad`, and
   give it `pure` as default implementation.

2. **Change only things that are closely related to the proposal.** For
   example, using `join` in a monad definition requires it to be a functor, so
   it goes hand in hand with the AMP. On the other hand, removing `fail` has
   nothing to do with what we're trying to accomplish.

Here's the list of final changes I have in mind:

- Make `Applicative` a superclass of `Monad`.

- Add Applicative to the Report, define it in the Prelude, and re-export it
  from `Control.Applicative` for compatibility.

- Add `join` to the `Monad` typeclass. In addition to the fact that `join` may
  be closer to mathematical monads, it's also sometimes more convenient to
  implement and understand in practice (example: Reader's `join f x = f x x`
  vs `m >>= f = \r -> m (f r) r`).

- Make `Alternative` (along with `Monad`) a superclass of `MonadPlus`.

The following will first discuss the consequences of these changes; afterwards
there's a strategy about how to make this chainge as painless as possible.







Discussion of the consequences
------------------------------



### It's the right thing to do™

Math. You've all heard this one, it's good and compelling so I don't need to
spell it out.



### Redundant functions

- `pure` and `return` do the same thing.
- `>>` and `*>` are identical.
- `liftM` and `liftA` are `fmap`. The `liftM*` are `liftA*`, `<*>` is `ap`.
- Prelude's `sequence` requires `Monad` right now, while `Applicative` is
  sufficient to implement it. The more general version of this issue is
  captured by `Data.Traversable`, whose main typeclass implements the *same*
  functionality twice, namely `traverse` and `mapM`, and `sequenceA` and
  `sequence`.
- The `WrappedMonad` type from `Control.Applicative` provides a semi-automatic
  way to using Functor/Applicative/Alternative functions for Monad/MonadPlus
  instances as a makeshift patch.

That very much violates the "don't repeat yourself" principle, and even more so
it *forces* the programmer to repeat himself to achieve maximal generality. It
may be too late to take all redundancies out, but at least we can prevent new
ones from being created.

(Note that it is not proposed to remove any functions for compatibility
reasons. Maybe some of them can be phased out in the long run, but that's
beyond scope here.)



### Using Functor/Applicative functions in monadic code

Whenever there's Monad code, you can use Functor/Applicative functions, without
introducing an additional constraint. Keep in mind that "Functor/Applicative
functions" does not only include what their typeclasses define but many more,
for example `void`, `(<$>)`, `(<**>)`.

Even if you think you have monadic code, strictly using the least restrictive
functions may result in something that requires only Applicative. This is
similar to writing a function that needs `Int`, but it turns out any `Integral`
will do - more polymorphism for free.



### Compatibility issues

These are the kinds of issues to be expected:

1. Monads lacking Functor or Applicative instances. This is easily fixable by
   either setting `fmap = liftM`, `pure = return` and `(<*>) = ap`, although
   more efficient implementations may exist, or by moving an already existing
   definition from `Control.Applicative` to the appropriate module.

2. This one is specific to building GHC: importing `Control.Monad/Applicative`
   introduces a circular module dependency. In this case, one can rely on
   handwritten implementations of the desired function, e.g.
   `ap f x = f >>= ...`.

3. Libraries using their own `(<*>)`. This one is much tougher, as renaming the
   operator may require a lot of effort. For building GHC though, this only
   concerns Hoopl, and a handful of renames.



### Beginner friendliness

How often did you say ...

- "A Monad is always an Applicative but due to historical reasons it's not but
  you can easily verify it by setting `pure = return` and `(<*>) = ap`"
- "`liftM` is `fmap` but not really." - "So when should I use `fmap` and when
  `liftM`?" - *sigh*

With the new hierarchy, the answer would *always* be "use the least restrictive
one".




How to apply this change
------------------------


### 1. Prepare GHC

Using a GHC fork with the full patch applied, find and fix all compilation
errors introduced by the change by adding Functor/Applicative instances for all
Monads.

According to SPJ, adding an ad-hoc warning of sorts "Monad without Applicative
detected" is not a problem, which will be crucial for the next phase. More
specifically, issue a warning if:

- Monad without Applicative
- MonadPlus without Alternative
- One of `<*>`, `pure`, `join` is defined in a different context to avoid
naming conflicts, as these functions will go into the Prelude


### 2. Prepare Hackage

The warning just mentioned will hint to all authors that they should fix (or
help others fix) the non-complying packages. This will ideally lead to
libraries eventually adding Applicative instances, and changing their APIs if
they redefine operators like `<*>`.

After enough time has passed by so libraries adapted to the circumstances, move
on to the next phase.


### 3. Apply the change

Once Hackage is prepared, applying the changes to the Base package is painless.
However, this is not primarily a GHC, but a Haskell change. The previous steps
were basically preparing the landscape, and when we've (hopefully) found out
that it is a good idea to go through with it, it can be proposed to go into
the Report. If we make it this far, the AMP should pass quite easily.









Outline of the new code
-----------------------

This is how the new code in Base would look like:


```haskell
-- (Unchanged)
class  Functor f  where

    -- Minimal complete definition: fmap

    fmap :: (a -> b) -> f a -> f b

    (<$) :: a -> f b -> f a
    (<$) =  fmap . const



-- (Unchanged)
class Functor f => Applicative f where

    -- Minimal complete definition: pure and (<*>)

    pure :: a -> f a

    (<*>) :: f (a -> b) -> f a -> f b

    (*>) :: f a -> f b -> f b
    (*>) x y = const id <$> x <*> y

    (<*) :: f a -> f b -> f a
    (<*) x y = const <$> x <*> y



class Applicative m => Monad m where

    -- Minimal complete definition: (>>=) or join

    (>>=) :: m a -> (a -> m b) -> m b
    m >>= f = join (fmap f m)

    (>>) :: m a -> m b -> m b
    (>>) = (*>)

    join :: m (m a) -> m a
    join m = m >>= id

    return :: a -> m a
    return = pure

    fail :: String -> m a
    fail s = error s



-- (Unchanged)
class Applicative f => Alternative f where

    -- Minimal complete definition: empty and (<|>)

    empty :: f a

    (<|>) :: f a -> f a -> f a

    some :: f a -> f [a]
    some v = some_v
      where
        many_v = some_v <|> pure []
        some_v = (:) <$> v <*> many_v

    many :: f a -> f [a]
    many v = many_v
      where
        many_v = some_v <|> pure []
        some_v = (:) <$> v <*> many_v



class (Alternative m, Monad m) => MonadPlus m where

    -- Minimal complete definition: nothing :-)

    mzero :: m a
    mzero = empty

    mplus :: m a -> m a -> m a
    mplus = (<|>)
```




Status report
-------------

- 2013-05-??: Added Applicatives to GHC for testing. Result: easy but boring.
- 2013-05-16: Asked the mailing list about adding instances to GHC
- 2013-05-22: SPJ confirmed that adding ad-hoc warnings would be possible
- 2013-05-23: AMP posted to libraries mailing list
- 2013-06-17: AMP accepted unanimously, last call sent, wiki updated
- 2013-09-09: [AMP warnings patch merged][amp-patch], to be released with
              GHC 7.8
- 2014-04-09: GHC 7.8.1 released. AMP is now in the second stage: warnings are
              issued, Hackage should adapt to cleaning them up.
- 2014-09-09: AMP merged into GHC master branch: `Applicative` is now a
              superclass of `Monad` in the compiler.
- 2015-03-27: GHC 7.10.1 released. The AMP is officially implemented. Thanks
              to all participants in its creation!

[amp-patch]: https://github.com/ghc/ghc/commit/75a9664af1c4e6f87794b49a215adb235b20696d
