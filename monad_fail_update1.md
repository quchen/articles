MonadFail proposal update 1
===========================


(Original MFP: https://github.com/quchen/articles/blob/master/monad_fail.md)


Short summary
-------------

A week has passed since I posted the MFP, and the initial discussion is mostly
over. Here are my observations:

- Everyone agrees that `fail` should not be in `Monad`.
- Almost everyone agrees that it should be thrown out of it.
- Some would prefer to see the special desugaring be gone entirely.
- The name `MonadFail` is controversial, because of a potential `Applicative`
  constraint.
- We're still unsure about whether `IO` should get a `MonadFail` instance, but
  the bias seems to be towards "yes".



New ideas worth thinking about
------------------------------

### Special desugaring or not

Johann suggested an optional warning whenever something desugars to use `fail`.
I think that's an idea we should think about. It is easily implemented in the
compiler, and would probably behave similar to -fwarn-unused-do-binds in
practice: notation that is not wrong, but might not be what the programmer
intended.


### Errors vs. Exceptions

Henning is concerned about the confusion between exceptions and programming
errors. In his words,

> We should clearly decide what "fail" is intended for - for programming
> errors or for exceptions.

What I see clashing with his point is backwards compatibility. Removing the
`String` argument breaks all explicit invocations of `fail`. Unfortunately,
we're not in a position to break very much. If someone has a better idea I'd
love to hear about it though.


### ApplicativeDo

ApplicativeDo is somewhere out there on the horizon, and we're not sure yet how
much `fail` makes sense in the context of `Applicative`. An Applicative
computation is statically determined in its shape, so it either always or never
fails. Depending on previous results would introduce the `Monad` constraint
anyway.



Probing status
--------------

Henning has started to look at the impact of the proposal when explicit
invocations of `fail` are considered as well, something I have not done in my
original survey. Luckily, things don't look too bad, Lens and its forest of
dependencies can be fixed in around ten trivial changes, for example.


Greetings,
David/quchen
