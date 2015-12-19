Tag, don't `type`
=================



The problem
-----------

I'm not a fan of Haskell's ability to introduce type synonyms: too often are
they used instead of actual new data types in order to introduce abstraction.
A type is a label we assign to values in order to know (and let the compiler
know) which operations can act on it, and type synonyms violate that very
principle.

It took me a long time until I realized that any type in GHC with "type" in its
name is likely [literally a synonym for `Type`][typesyns]. This phenomenon
allows many layers of indirection, which hides the place one might find
documentation about what the type represents behind one of the many layers,
making you play the "find the Haddock comment" game.

But type synonyms are at their worst when it comes to their use to encode
different invariants of a data type. GHC has types with invariants similar to
"after phase X, there are no `Nothing`s left", and encodes this fact via giving
the temporally different values different `type` labels. This is terrible,
since the typechecker offers no help at all in this scenario: it will happily
"convert" between values that mean entirely different things. This is, in
essence, a dynamic type system, where the user has to know what values are
legitimate.

To summarize:

1. `type` hides the contained data from the programmer's eyes
2. `type` can introduce many layers of indirection, scattering documentation
   and actual data constructors
3. Invariants encoded in `type` are completely invisible to the compiler, and
   by extension to the inexperienced (as in new to a codebase) user as well



A possible solution
-------------------

We can solve some of the above problems with a well-known type:

```haskell
newtype Tagged t a = Tagged { unTagged :: a }
```

[On Hackage, this is available in the `tagged` package.][hack-tagged]

This type carries a payload of type `a`, and a tag `t`, whose values are
completely ignored by the program (also known as a phantom type).

How is this beneficial to solving our problems?

1. `Tagged t a` makes it clear that the contained data is `a`, noting is hidden.
2. `Tagged t a` cannot introduce layers of indirection implicitly. Sure, you
   can have a `Tagged t (Tagged s a)`, but then it's still clear that your data
   is `a`, and it's hidden in plain sight behind two tags.
3. A `Tagged t a` is an entirely different type to the compiler than a
   `Tagged s a`, and the compiler will not implicitly convert between the two.
   However, the programmer can write a function `Tagged t a -> Tagged s a`
   easily (how?) to make the conversion explicit.

Now we can translate our GHC type synonyms:

```haskell
type PredType    = Type
type TcPredType  = PredType
type TcRhoType   = TcType
type TcSigmaType = TcType
type TcTauType   = TcType
type TcType      = Type
```

```haskell
-- Empty data types to be used as tags
data PredType
data TcPredType
data TcRhoType
data TcSigmaType
data TcTauType
data TcType

-- Now you can use ...    --   Instead of ...
Tagged PredType    Type   --   type PredType
Tagged TcPredType  Type   --   type TcPredType
Tagged TcRhoType   Type   --   type TcRhoType
Tagged TcSigmaType Type   --   type TcSigmaType
Tagged TcTauType   Type   --   type TcTauType
Tagged TcType      Type   --   type TcType
```

It is perfectly clear that all of them mean some form of `Type`, although with
maybe a different connotation. The documentation of the invariants would go to
the empty data types, which is a single place, and no worries of chasing
definitions.



Nothing's for free
------------------

There are downsides of this approach of course, so it's not a no-brainer
solution.

- You will have to wrap and unwrap thigns in `Tagged`, or use predefined
  convenience functions to do it for you. Whatever the case, there will be a
  certain amount of syntactic noise.
- `Tagged` does not share `a`'s instances. You can easily write all the
  instances so that `Tagged t a` behaves the same way as `a`, but you will have
  to do this for each type class again.
- There are very good use cases for `type`, such as being able to write lenses
  without depending on `lens`; this is something `Tagged` cannot help you with.



Take-away messages
------------------

- Use `type` sparingly, and if you really need that shortcut, try to keep it as
  local as possible.
- Using `Tagged` makes many things explicit that using `type` in its stead
  makes implicit.
- Keep asking yourself whether a handful of `Tagged` and `unTagged` make your
  codebase worse than a new reader making innocent mistakes because of a
  misunderstanding of which values are allowed in certain places.




[typesyns]: https://downloads.haskell.org/~ghc/latest/docs/html/libraries/ghc-7.10.2/TcType.html
[hack-tagged]: http://hackage.haskell.org/package/tagged
[rnm]: https://downloads.haskell.org/~ghc/latest/docs/html/libraries/ghc-7.10.2/TcRnTypes.html#t:RnM
