Algebraic blindness
===================



Abstract
--------

Algebraic data types make data more flexible, but also prone to a form of
generalized Boolean Blindness, making code more difficult to maintain.
Luckily, refactoring the issues is completely type-safe.



Boolean blindness
-----------------

In programming, there is a common problem known as *Boolean Blindness*, what it
»means« to be `True` depends heavily on the context, and cannot be inferred by the
value alone. Consider

```haskell
main = withFile True "file.txt" (\handle -> do stuff)
```

The Boolean parameter could distinguish between read-only/write-only, or
read-only/read+write, whether the file should be truncated before opening, and
countless other possibilities.

And often there is an even worse issue: we have two booleans, and we do not know
whether they describe something in the same problem domain. A boolean used for
read-only-vs-write-only looks just the same as one distinguishing red from blue.
And then, one day, we open a file in »blue mode«, and probably nothing good
follows.

The point is: in order to find out what the `True` *means*, you probably have to
read documentation or code elsewhere.



Haskell to the rescue
---------------------

Haskell makes it very natural and cheap to define our own data types. The
example code above would be expressed as

```haskell
main = withFile ReadMode "file.txt" (\handle -> do stuff)
```

in more idiomatic Haskell. Here, the meaning of the field is obvious, and since
a data type `data IOMode = ReadMode | WriteMode` has nothing to do with a data
type `data Colours = Red | Blue`, we cannot accidentally pass a `Red` value to
our function, despite the fact that they would both have corresponded to `True`
in the Boolean-typed example.



The petting zoo of blindness
----------------------------

Boolean blindness is of course just a name for the most characteristic version
of the issue that most standard types share. An `Int` parameter to a server
might be a port or a timeout, a `String` could be a host, a route, a log prefix,
and so on.

The Haskell solution is to simply wrap things in newtypes, tagging values with
phantom types, or introducing type synonyms. (I’m not a fan of the latter, which
you can read more about [in a previous article][tag-dont-type].)



Algebraic blindness
-------------------

Haskell has a lot more »simple, always available, nicely supported« types than
most other languages, for example `Maybe`, `Either`, tuples or lists. These
types naturally extend Boolean Blindness.

- `Maybe` adds another distinct value to a type. `Nothing` is sometimes used to
  denote an error case (this is what many assume by default, implicitly given by
  its `Monad` instance), sometimes the »nothing weird happened« case, and
  sometimes something completely different.

  `Maybe a` is as blind as `a`, plus one value.

- `Either` is similar: sometimes `Left` is an exceptional case, sometimes it’s
  just »the other« case.

  `Either a b` is as blind as `a` plus as blind as `b`, plus one for the fact
  that `Left` and `Right` do not have intrinsic meaning.

- Tuples have two fields, but how do they relate to each other? Does one maybe
  tell us about errors in the other? We cannot know.

  `(a,b)` is as blind as `a` times the blindness of `b`.

- Unit is not very blind, since even synonyms of it mostly mean the same thing:
  we don’t really care about the result.

In GHCi’s source code, there is a value of type `Maybe Bool` passed around, which has three possible values:

1. `Nothing` means there is no more input to process when GHCi is invoked via
   `ghc -e`.
2. `Just True` reports success of the last command.
3. `Just False` is an error in the last command, and `ghc -e` should abort with
   an exit code of `1`, while a GHCi session should continue.

It is very hard to justify this over something like

```haskell
data CommandResult
    = NoMoreInput -- ^ Haddock might go here!
    | Success
    | Failure
```

which is just as easy to implement, has four lines of overhead (instead of 0
lines of overheadache), is easily searchable in full-text search even, and gives
us type errors when we use it in the wrong context.



Haskell to the rescue, for real this time
-----------------------------------------

We’re lucky, because Haskell has a built-in solution here as well. We can
prototype our programs not worrying about the precise meaning of symbols, use
`Either () ()` instead of `Bool` because we might need the flexibility, and do
all sorts of atrocities.

The type system allows us to repair our code: just understand what the different
values of our blind values mean, and encode this in a new data type. Then pick a
random use site, and just put it in there. The compiler will yell at you for a
couple of minutes, but it *will* report every single conflicting site, and since
you’re introducing something entirely new, there is no way you are producing
undetected clashes. I found this type of refactoring to be **one of the most
powerful tools Haskell has to offer**, but we rarely speak about it because it
seems so normal to us.



Drawbacks
---------

Introducing new domain types has a drawback: we lose the API of the standard
types. The result is that we sometimes have to write boilerplate to get specific
parts of the API back, which is unfortunate, and sometimes this makes it not
worth bothering with the anti-blindness refactoring.



Conclusion
----------

When you have lots of faceless data types in your code, consider painting them
with their domain meanings. Make them distinct, make them memorable, make them
maintainable. And sometimes, when you see a type that looks like

```haskell
data IOMode
    = ReadMode
    | WriteMode
    | AppendMode
    | ReadWriteMode
```

take a moment to appreciate that the author hasn’t used

```haskell
Either Bool Bool
--      ^    ^
--      |    |
--      |    Complex modes: append, read+write
--      |
--      Simple modes: read/write only
```

instead.

[tag-dont-type]: tag-dont-type.md
