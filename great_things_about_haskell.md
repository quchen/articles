What makes Haskell great
========================



(In order of greatness)



The type system
---------------

### Strong vs. weak typing

The good thing about strong typing, like encountered in C, C++ or Java, is that it catches a whole class of bugs during compilation: trying to compare apples with oranges, the compiler will raise a type mismatch error. If you *do* want to compare apples to oranges, you first have to at least convert one into the other. The downside of this is that you have to specify a type signature for every function ("this function takes an integer and returns a boolean"), and if you have to convert many apples to oranges, the code will be full of "toOranges(apple)" calls.

On the other side, there's weak typing, as seen in e.g. Python and JavaScript. There you don't have to give type annotations, saving you a lot of writing, making the code clearer in many cases. The cost of this is of course the introduction of bugs of the type mentioned above.

Haskell has the best of both worlds: at its core, it is a strongly typed language: you can't compare apples to oranges. However, Haskell doesn't require type annotations: based on the operations you use in a function, Haskell infers the type of all the operations and their combination on its own - it writes the type signatures automatically during compilation! You gain the safety of strong typing at the convenience of weak typing. Speaking of strong typing: Haskell's typing is even stronger than the ones of the languages mentioned above. Comparing the integer 0 with the string "0" will result in an error; Haskell *never* converts anything automatically. However, the need to do explicit conversions is surprisingly rare, so there is no real code clutter. In fact, if you feel like you have too many typecasts in a small block of code, there's probably a better way of writing it.

Also worth noting is that you can turn that principle around: by giving a function an explicit type signature, you tell the compiler what kind of function you want. You then write the implementation of that function to the best of your knowledge, and hit compile. Two possible outcomes: either your compiler is fine with what you wrote, or it complains. However, it tells you exactly where it complains and why. Often times, the compiler will guide you through writing your function, not just the types! "Hey, there's something wrong here, expected that". You look at the few options you have, think about them, put one there, and retry until it compiles. If it compiles, chances are very high that the function does what you think it should do, and by *very* high I mean so high it's hard to explain to someone who hasn't experienced it. Yanking this up even more, you get typed holes - you basically leave a placeholder in your code. The compiler tells you what type the hole has, and suggests functions that are in scope to fill that hole. It's almost interactive coding: make hole, read error, fill hole with a sensible solution; that solution can of course contain holes itself. You can keep doing this until your function is finished. Even better, functions with complex types may have a unique realization, so you get your code for free by just describing what the function should map to what.



### Custom types

Imagine how you would implement a function to search an array in your preferred language. If you find the desired item, you of course return it, but what happens when there is no such item? Chances are that you have to resort to a nonsensical value like -1. But what happens if your array includes negative numbers? How can you tell the "not found" -1 from the "found, result is" -1? You'll probably need to either return a tuple of the value found and a boolean that indicates whether that value was actually present in the array, or search for the corresponding array index instead, and afterwards returning the element at that index or handle the error if the "found" index is -1.

In Haskell, there is the `Maybe` type for cases like this. Variables of type `Maybe` fall into two kinds of values: they're either `Nothing`, or `Just [some value]`. This solves the above problem: `Nothing` stands for not found, whereas `Just x` is returned when the value `x` has been found. And it doesn't stop there: functions returning `Maybe` compose. Connect to database server? Might fail (`Nothing`), might be a connection (`Just [handler]`). Login? Might fail. Select database? Might fail. What would most languages do? Exceptions? Nested ifs? Early returns? Haskell just chains these operations to each other. If one fails, their combination fails, and whatever comes after the failure is skipped.

Big deal about this `Maybe`, you might say. But that was just the canonical example of a Haskell type. In fact, although `Maybe` is a standard library type, you can define it yourself very easily, with the same performance and everything. Many things you commonly use in Haskell are just defined in the source code of libraries, they are not built-in. Another example of this is `Either`, which is like a pimped `Maybe`, where the `Nothing` can also hold an additional value. An `Either` value can be `Left x` or `Right x`, the latter one is usually taken as a pun for the "right" result, whereas `Left` stands for an error. A common example of the `Either` type is the result of a parser: it's either the parsed object, or an error describing what went wrong.



### Reusability

Many things in programming have similar interfaces. You can add two numbers, adding 0 to anything doesn't change it, and it doesn't matter where you put the parentheses in `1+2+3`. You can multiply numbers, 1 doesn't change the result, and parentheses in `1*2*3` don't matter. So much for elementary operations. But there's more, in fact a lot more. When you concatenate a list and the empty list, you get back the list. When you concatenate three lists, it doesn't matter how you group the concatenation, you'll always get the same result. There are many other objects that obey these two simple rules. So why not have a common interface? Well, it's called `Monoid` in Haskell (and math). There are functions that take any monoid and put them together, using the appropriate monoid operation.

Monoids are of course just an example, there are many other so-called *type classes*. `Eq` is for things you can compare for equality; as you may expect, most things are part of this. `Ord` are ordered types, for example real numbers. Complex numbers aren't in `Ord` for example, because you can't order them in a meaningful manner. `Enum` (for enumeratable) are things that have a next and previous element.

Functions are rarely restricted more than they have to be. If you write a function, chances are that it doesn't only work for the type you have intended, but for the whole class of types that behaves similarly, and in addition, there are laws for the basic operations that ensure that these generic functions do what you think they should do, no matter what kind of an abstruse type you feed them.



Glue
----

What is commonly referred to as *glue* is the ability of a language to combine different parts of a program. In Haskell, functions are generally very small - a handful of lines - and do one very specific thing. When programming, you then combine these simple functions. Want the first five positive list elements? Chain `take 5` together with `filter (> 0)`. Haskell has many individual, very reusable functions, and glueing them together makes it possible to have a vast amount of different functions using only a few basic ones.

A consequence of this concept is that most functions are composed of smaller functions. When you know precisely how and that the smaller functions work, then you most likely know that about your combined function. It is very common to build a library of small bits, and then just export the actually useful combinations; however, only the small bits have to be tested. (Most likely, the typechecker complains when you combine them the wrong way anyway.)

If that still doesn't convice you, think of a Linux terminal. How do you use it? There's a small function for every detail, and you then chain those together. One just prints a file (cat), you then feed that to another function that filters out some elements (grep etc.), and finally you count how many entries are still left (wc). How many lines are in a file containing an 'a'? `cat file | grep "a" | wc`. That's precisely the kind of plumbing you do in Haskell all the time.



Read-Evaluate-Print-Loops (REPL)
--------------------------------

Haskell has two implementation of a REPL (GHCi and Hugs), a principle you may know from iPython. It's a command line tool similar to a terminal, where the commands are the functions you import. Typical Haskell workflow is working on a small function, loading it into the REPL, and seeing whether it compiles (fix errors, repeat), and whether it does what it should do. It's very helpful to play around with combinations of your functions before you plug them into your actual code. And this isn't just tinkering: this is what writing Haskell looks like. You have your editor, and your REPL, and you bounce back and forth between them. You immediately see if something is fishy after writing only ten lines; nobody writes more than a page of code in Haskell without giving it a test shot.



LYAH
----

Learn You a Haskell for Great Good, short LYAH, is the best introductory book for any programming language I've ever come across. It's fun to read, and in the meantime it teaches you Haskell by accident. [Oh, and it's available for free online.](http://learnyouahaskell.com/)



What makes Haskell so-so
========================



(Things that could be interpreted either way)



### Relearning how to program

Learning Haskell without a doubt feels like learning to program all over again. At least from my perspective, I couldn't imagine having it learned as not a hobby. Tinkering around until you understand all the new concepts may be something that's not for everyone. And you have to do that over and over again.



### Going back

After becoming proficient in Haskell, you will miss things in any other language you write. You will miss the basic things like `Maybe` as much as you'll miss the advanced features like the type safety, the ability to use functions as data, parameters and return values easily. And that's just the tip of the iceberg.



### The mathiness of the community

If you join #haskell on irc.freenode, you'll most likely encounter a discussion about category theory, logic or some other academic topic quite quickly. This has led to the prejudice that these advanced concepts are necessary to learn Haskell, *which they are absolutely not*. In fact, many of these concepts are applicable to many other programming languages, only that nobody recognizes them or bothers to talk about it. Haskell sure creates an environment where people gather that are interested in such matters, but it's not a requirement in order to learn how to use it.



### The library phase

Once you're past the first steps (e.g. after finishing LYAH), there's nothing to guide you really. It's what I call the library phase: many problems already have libraries to solve them, but you have to find out about them first, and then learn their API. Some concepts of functional problems require approaches that you wouldn't expect as an imperative programmer, making it hard to find the according library; #haskell will be very helpful here though.



### Code levels

If you see well-written code by someone who has a much higher skillset than you in say C, C++ or Java, you'll understand what it's doing after reading it a couple of times. In Haskell, code using concepts you're not familiar with may be completely unreadable, no matter how well-written and well-documented it is. Above I said you don't need to learn or use advanced math to write Haskell, but if you want to, you've surely come to the right place. On the other hand, the basic modules are all written in pretty basic style, so you won't encounter this all that much until you download your first super flexible, extremely useful and well-documented library, learn to use it, and then dare to take a look at its inside.



### Everyone thinks you're weird

First, get a mirror. Yes, you. Now.

"I like this language that has no loops, all variables are constant, and evaluates program parts in whatever order it wants to."

Now look at the mirror. That's what people will look like when you tell them that. Right now you probably think this is a joke. It's not.





What makes Haskell bad
======================

