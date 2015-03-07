Functional design patterns
==========================

A *design pattern in the negative sense* "is a concept that is intuitively clear, but not easily expressed in a language" [(source)][hodapp-quote]. Most of us are familiar with design patterns in imperative languages, many of which simply don't exist in functional settings, as they are captured naturally by the paradigm. Other things aren't as natural, and so some tasks that would be easy in an imperative setting become a design pattern.

In contrast to that, there are also *design patterns in the positive sense*. They don't overcome problems, but make the most out of a language's features in order to capture complex behaviour in a simple, elegant way. Some would say that this simply means writing idiomatic code, but sometimes new ways of doing so have first to be discovered, and until they become mainstream, it's fair to regard them as patterns as well.

This is a collection of design patterns that I've come across using functional languages. For the sake of clarity, for me a "functional language" means a focus on using functions as control structures, avoiding mutable state, and strong type systems.

[hodapp-quote]: http://ircbrowse.net/browse/haskell?id=19809422&timestamp=1420581031#t1420581031



Basic programming patterns
--------------------------

Knowing about these patterns is very useful even at small scales, and suitable for users of all skill levels.

### Tying the knot

TODO





### Avoiding side effects

TODO





### Stacking effects

TODO





### TyDD: Type-driven development

Let me start this section with a parable. Why is it that trains have way fewer accidents than cars, both relatively and absolutely? It certainly isn't that they're bigger and heavier, and they're driven by people just the same. The reason is of course that there are much fewer directions a train can move: it is limited to going forward and backward and taking a left or right at a turnout every now and then. Imagine the ways a car accident can happen that cannot possibly happen to a train. The conductor briefly falls asleep (once)? No problem, you can't drift off to the side. There are no crossroads where all of a sudden another driver can appear from the side, because the tracks are constructed so that this would be predictable way before something happens. Trains don't suddenly stop for no reason very often, and if they do, they're planned to run with enough distance between them to react to such events.

All these things are restrictions imposed on the train that allow it to make certain practical and very strong assumptions about its environment, but the price it pays for that is that it cannot usually freely choose where to go except in some rare scenarios.

So what are the tracks in programming, and how can we make the best use of them?

-------------------------------------------------------------------------------

The goal of type systems is to reduce the number of programs expressible in a language. This comes with the hope that this predominantly eliminates undesirable programs, and leaves just a single valid program left over. With type systems that aren't ad-hoc like C's or Java's, basically dating back to ALGOL plus some practical decorations, this is hardly achievable, and they tend to get in the way too much (being annoying) or too little (not recognizing enough). However, there is a separate branch of computer science deeply connected to logic that investigates type systems, and it is that branch that laid the foundation for systems we can use in practice today that come with strong mathematical guarantees about the programs they describe.

There is a truly deep connection between programs and logic, which in a nutshell tells us that a program that typechecks is *logically sound*, that is, does not contain any objectively false elements. A type system defines a set of rules which allow other rules to be expressed, and if a type system has the right strength, there are usually only very few reasonable programs that satisfy this logical soundness. For this reason, one can not only get affirmation by the type system that one's reasoning is correct, but also let the implementation guide the programmer to (one of the) correct programs, because very often, there are only a handful of possible things to write in a certain place.

Leveraging this observation is what I like to call "TyDD", which is a compromise between a name clash with TDD, usually understood as test-driven development, and inventing a completely different word.

-------------------------------------------------------------------------------

I'll start with a simple example that may look like it's not the shortest, but can teach us a tremendous amount about this approach. Afterwards, we'll discuss the bigger picture.

Imagine you wanted to write the `map` function that applies a function to each element in a list, so that `map(twice, [1,2,3])` evaluates to `[2,4,6]`, and you had no idea how to implement it. The list is a singly linked list, and since we're functional, the `map` function is of course recurive and not iterative. Just thinking about what types `map` should take and what it should produce, we can write down its signature: it takes a list of things of any type `a`, a function to create a `b` out of any such `a`, and combines them to yield a list of things of type `b`. In `map(toString, [1,2,3])` for example, the function `toString` would have the type `int -> string`, and the list would be a list of `int`. Like some functional programming languages, I'll use parentheses for grouping, and denote function application with simply a space, so that `f x` stands for what you may know as `f(x)`. Alas,

```haskell
map : (a -> b, List a) -> List b
map(f, list) = ???
```

At this point, a sufficiently smart compiler could already tell us that instead of the `???` it expected something of type `List b`. No surprise there, but it's reassuring nevertheless. Now that since *all* that we have available is the function `f` and the list `list`, what cases can there be? In the simplest scenario, the list might be empty, so mapping `f` over it should yield an empty list again. If `list` is not empty, then mapping over it means applying `f` to the first element, and then continuing with mapping over the rest of the list.

```haskell
map : (a -> b, List a) -> List b
map(f, list) =
  if isEmpty list
    then: emptyList
    else: ?combine (f (head list), ?rest)

-- head gives us the first element of a list
```

Our compiler might now say that it expects `?combine` to have type
`(b,anything) -> List b`, while `?rest` has type `anything`. Do we have anything that satisfies these? The first one looks oddly like the "prepend an element to a list" function, often known as "cons", which has type `(b, List b) -> List b`, which gives us (or better, the compiler) the fact that `anything` must be `List b`.

```haskell
map : (a -> b, List a) -> List b
map(f, list) =
  if (isEmpty list)
    then: emptyList
    else: cons (f (head list), ?rest)

-- head gives us everything but the first element of a list.
-- In other words: cons (head list) (tail list) == list.
```

The compiler accepts the program, but complains that there's a placeholder `?rest` left, which as you remember had type `anything`, but our previous choice narrowed it down to `List b`. If only we had a `List b` lying around ... but wait, we're mapping over something, and the result of the operation should be a `List b`! Since we've accounted for the first element already, we'll just add "map over the rest" in the placeholder, yielding

```haskell
map : (a -> b, List a) -> List b
map(f, list) =
  if (isEmpty list)
    then: emptyList
    else: cons (f (head list), map (f, tail list))
```

Now we're done: this is a correct implementation of the `map` function. Now these steps may have seemed complicated, but evaluate them again carefully, taking note which steps required *us* to think, as opposed to having the compiler think *for us*. Here is everything put together:

- `map` should take a function and a list, and transform the list element-wise. This gave us the overall type.
- Applying a function to "all the elements" of an empty list does nothing.
- Mapping over a nonempty list requires applying the function to one element, and all the other elements.
- We can get one element, namely the first one, and also the list of the remaining ones.
- The list mapped over is the combination of the transformed element with the yet to be transformed rest of the list.

Each of these steps is very simple, and the compiler helped us pick the right functions.

The really amazing thing about this is that it scales incredibly well to complex functions. To give you an example about this, there is a very practical function with the type

```haskell
((a -> b) -> d, (a, c) -> b, c) -> d
```

that in this presentation I find to be a completely incomprehensible type. Nevertheless, it is possible to find an implementation for it, namely

```haskell
f (x, y, z) = x (λ c -> y c z)
```

and it is used to chain continuations together (which is to say, it's a glorified version of function composition). Using type-driven development, we can find such functions systematically.

-------------------------------------------------------------------------------

Take it up one notch and you're working with a huge library with lots of functions in scope. You don't know how the one you want is called, all you know is how its shape, i.e. its type, is supposed to look like. You build a skeleton of what you think you want, and the compiler serves you a handful of fitting alternatives on a silver tablet. I can't stress this enough: the approach scales well beyond anything short enough to present it in a document like this, and in fact I haven't seen or heard of the ceiling of it to date.

It's not all rainbows and unicorns though, because a well-typed program doesn't mean it actually does what we intended it to do. In the `map` example, we could easily have combined things the other way round, resulting in a reversed result list, which thet typechecker would have happily eaten.





### Deliberately unnecessary modules

Sometimes, the easiest way of debugging is using dirty "printf style", where you just insert various reporting and bookkeeping functions in places you'd like to scope in. But it's easy to forget that these functions are used, and sometimes one that's in an obscure place might not be noticed for some time, until one day it turns out it made its way into a release.

My solution to this problem is simple: all functions I use for debugging are in one module that contains only that. Before releasing, that module can simply be removed, and the compiler will complain about all usages that were forgotten. Simple, but it saved me from bad mistakes numerous times.





Advanced programming patterns
-----------------------------

These patterns range from being useful to most advanced Haskell users, to being very situational.

### Free structures

TODO





### Mutable updates

TODO





### Continuation-passing style

TODO





Architectural patterns
----------------------

An architectural pattern is not so much concerned about how actual code is written, but in what way it is aggregated and presented to the maintainer or user. Because of this, these patterns are likely applicable even outside of the functional domain.





### Extending modules

TODO





### Internal modules

The point of an API is only exposing the necessary functionality to accomplish an abstract task, while keeping the implementation details hidden. Sometimes, one would like to have access to these implementation details anyway, however. Valid reasons for this include testing unexposed functionality, and quick and dirty hacks (for some value of "valid", at least).

The concept of an internal module, as opposed to a hidden one, is a social convention: modules with "internal" in their names are exposed and usable just like the API, but don't necessarily provide safety guarantees or feature stability. Internal modules should only be used if one is *certain* that this is what one wants.

To give an example, the API of many compilers consists of converting source code to executables. It would be silly to test that and only that, as internals have their own invariants, and it is very insightful to detect the errors on the lowest possible level. Hence it is useful to expose internals purely for the sake of scoping in, and since the testsuite is shipped with the code of the same version API instability is not a concern.





### Barrier modules

Most nontrivial libraries or programs are composed of many *layers* of abstractions. Functionality starts at a rather low level (e.g. integer arithmetic), and is then aggregated by higher level modules (e.g. rational numbers), and at the top there is the high-level API exposed to the user (e.g. complex numerical algorithms).

One thing one should try to avoid is having too many interdependencies between these modules. If everything imports whatever it wants, one quickly ends up with a ball of wool that is hard to bisect again, in order to find out which functionality comes from where.

Strictly encapsulating all functionality, only exporting what is actually needed by the library, is one way to attempt to solve the issue. But this is an often too rigid counterpart of exporting everything publically: some functions just need a very small part of an otherwise large program section, that may even be inaccessible because the "usual" functionality doesn't require its public exposure. Hidden data constructors are an example I've encountered numerous times, for example.

The barrier module architecture aims at balancing these two approaches:

> A module should not be imported alongside any of its submodules.

This can be visualized as each import building up a barrier against modules it is the parent of: importing `Containers.List` means there is a barrier after `List` which may not be broken by e.g. importing `Containers.List.Linked`.

This has a number of consequences:

- A higher-level module stands for all the functionality provided by its children put together. There is no necessity to worry about which chunk of an entire hierarchy has to be imported.
- If lower-level functionality is required somewhere, then the imports are available. However, with doing so, one loses the ability to rely on the higher abstraction level.
- Exceptions from the rule are possible, should the need arise. In this sense, the barrier rule is a guideline to module structure, and can be ignored on a per-case basis if necessary. This point is particularly important to mention, because it allows selectively opting out of usually present order, as opposed to selectively introducing it in presentn disorder.
