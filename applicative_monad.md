Haskell 2014: `Applicative => Monad` proposal
=============================================

Haskell calls a couple of historical accidents its own. While some of them, such as the "number classes" hierarchy, can be justified by pragmatism, there is one thing that stands out as, well, not that: Applicative not being a superclass of Monad.

I will use the abbreviation *AMP* for the "`Applicative => Monad` Proposal".






Outline
-------

The rationale behind this proposal's contents is as follows:

1. **Break as little code as possible.** For example, do not move `return` to `Applicative` and remove `pure`. Instead, leave `return` in `Monad`, and give it `pure` as default implementation.

2. **Change only things that are closely related to the proposal.** For example, using `join` in a monad definition requires it to be a functor, so it goes hand in hand with the AMP. On the other hand, removing `fail` has nothing to do with what we're trying to accomplish.

Here's the list of final changes I have in mind:

- Make `Applicative` a superclass of `Monad`.

- Add Applicative to the Report, define it in the Prelude, and re-export it from `Control.Applicative` for compatibility.

- Add `join` to the `Monad` typeclass. In addition to the fact that `join` may be closer to mathematical monads, it's also sometimes more convenient to implement and understand in practice (example: Reader's `join f x = f x x` vs `m >>= f = \r -> m (f r) r`).

- Make `Alternative` a superclass of `MonadPlus`. (The *only* reason MonadPlus exists is because of the Applicativie/Monad issue.)

The following will first discuss the consequences of these changes; afterwards there's a strategy about how to make this chainge as painless as possible.







Discussion of the consequences
------------------------------



### It's the right thing to do™

Math etc. You've all heard this one, it's good and compelling so I don't need to spell it out. Moving on,



### Performance

Using Applicative can be beneficial to performance, as the code can potentially be optimized better.

An example: a State computation with Applicative either always or never uses `put`; whether it does can only depend on external parameters, and not on the intermediate results. On the other hand, a monadic computation can depend on previous results, so a State Monad can, but does not always have to, use `put`. The state may or may not be modified, but can't be optimized away that easily.

With the AMP, monadic computations (especially `do` blocks used for their readability) could be rewritten to use Applicative functions when possible.



### Redundant functions

- `pure` and `return` do the same thing.
- `>>` and `*>` are identical.
- `liftM` and `liftA` are `fmap`. The `liftM*` are `liftA*`, `<*>` is `ap`.
- Prelude's `sequence` requres `Monad` right now, while `Applicative` is sufficient to implement it. The more general version of this issue is captured by `Data.Traversable`, whose main typeclass implements the *same* functionality twice, namely `traverse` and `mapM`, and `sequenceA` and `sequence`.

That very much violates the "don't repeat yourself" principle, and even more so it forces the programmer to repeat himself to achieve maximal generality. It may be too late to take all redundancies out, but at least we can prevent new ones from being created.



### Using Functor/Applicative functions in monadic code

After code duplication, this one is the next most practically relevant point: whenever there's Monad code, you can use Functor/Applicative functions, without introducing an additional constraint. Keep in mind that "Functor/Applicative functions" does not only include what their typeclasses define but many more, for example `void`, `(<$>)`, `(<**>)`.



### Compatibility issues

These are the kinds of issues to be expected:

1. Monads lacking Functor or Applicative instances. This is easily fixable by either setting `fmap = liftM`, `pure = return` and `(<*>) = ap`, although more efficient implementations may exist, or by moving an already existing definition from `Control.Applicative` to the appropriate module.

2. This one is specific to building GHC: importing `Control.Monad/Applicative` introduces a circular module dependency. In this case, one can rely on handwritten implementations of the desired function, e.g. `ap f x = f >>= ...`.

3. Libraries using their own `(<*>)`. This one is much tougher, as renaming the operator may require a lot of effort. For building GHC though, this only concerns Hoopl, and a handful of renames.




### Beginner friendliness

How often did you say ...

- "A Monad is always an Applicative but due to historical reasons it's not but you can easily verify it by setting `pure = return` and `(<*>) = ap`"
- "`liftM` is `fmap` but not really." - "So when should I use `fmap` and when `liftM`?" - *sigh*

With the new hierarchy, the answer would *always* be "use the least restrictive one".




How to apply this change
------------------------


### 1. Prepare GHC

Using a GHC fork with the full patch applied, find and fix all compilation errors introduced by the change by adding Functor/Applicative instances for all Monads.

According to SPJ, adding an ad-hoc warning of sorts "Monad without Applicative detected" is not a problem, which will be crucial for the next phase. More specifically, issue a warning if:

- Monad without Applicative
- MonadPlus without Alternative
- One of `<*>`, `pure`, `join` is defined in a different context to avoid naming conflicts, as these functions will go into the Prelude


### 2. Prepare Hackage

The warning just mentioned will hint to all authors that they should fix (or help others fix) the non-complying packages. This will ideally lead to libraries eventually adding Applicative instances, and changing their APIs if they redefine operators like `<*>`.

After enough time has passed by so libraries adapted to the circumstances, move on to the next phase.


### 3. Apply the change

Once Hackage is prepared, applying the changes to the Base package is painless. However, this is not primarily a GHC, but a Haskell change. The previous steps were basically preparing the landscape, and when we've (hopefully) found out that it is a good idea to go through with it, it can be proposed to go into the Report. If we make it this far, the AMP should go through quite easily.




Status report
-------------

- 2013-05-??: Added Applicatives to GHC for testing. Result: easy but boring.
- 2013-05-16: Told the mailing list about adding instances to GHC
- 2013-05-22: SPJ confirmed that adding ad-hoc warnings is possible