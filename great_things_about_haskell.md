Foreword
========



The following is an attempt to write up why I think Haskell is a great language. While it is certainly somewhat biased, I'll try my best to be as objective as possible.

One thing worth noting is that most of the things I'm mentioning *could* be done in many other languages as well, but it's not common practice, mostly because the language isn't designed to house this kind of concept for practical purposes. So each time you think "this argument is bad, I could do that in X as well", think about how likely it would be that you actually do it - if you still think I'm wrong, I'll happily take your suggestion into consideration.




Great things about Haskell
==========================



The type system
---------------

### Strong vs. weak typing

The good thing about strong typing, like encountered in C, C++ or Java, is that it catches a whole class of bugs during compilation: trying to compare apples with oranges, the compiler will raise a type mismatch error. If you *do* want to compare apples to oranges, you first have to at least convert one into the other. The downside of this is that you have to specify a type signature for every function ("this function takes an integer and returns a boolean"), and if you have to convert many apples to oranges, the code will be full of "toOranges(apple)" calls. (Some - even rather statically typed - languages can do such conversions automatically for simple types. For more complicated ones, you'll have to write some type conversion constructor though, effectively moving the explicit conversion to a different location.)

On the other side, there's weak typing, as seen in e.g. Python and JavaScript. There you don't have to give type annotations, saving you a lot of writing, making the code clearer in many cases. The cost of this is of course the introduction of bugs of the type mentioned above.

Haskell has the best of both worlds: at its core, it is a strongly typed language: you can't compare apples to oranges. However, Haskell doesn't require type annotations: based on the operations you use in a function, Haskell infers the type of all the operations and their combination on its own - it writes the type signatures automatically during compilation! You gain the **safety of strong typing at the convenience of weak typing**. Speaking of strong typing: Haskell's typing is even stronger than the ones of the languages mentioned above. Comparing the integer 0 with the string "0" will result in an error; Haskell *never* converts anything automatically. However, the need to do explicit conversions is surprisingly rare, so there is no real code clutter. In fact, if you feel like you have too many typecasts in a small block of code, there's probably a better way of writing it.

Also worth noting is that you can turn that principle around: by giving a function an explicit type signature, you tell the compiler what kind of function you want. You can then write the implementation of that function to the best of your knowledge (i.e. the code makes sense in your eyes), and hit compile. Two possible outcomes: either your compiler is fine with what you wrote, or it complains. However, it tells you exactly where it complains and why. Often times, the compilation will guide you through writing your function, not just the types! "Hey, there's something wrong here, expected that". You look at the few options you have, think about them, put one there, and retry until it compiles. **If it compiles, chances are *very* high that the function does what you think it should do**. Yanking this up even more, you get typed holes - you basically leave a placeholder in your code. The compiler tells you what type the hole has, and suggests functions that are in scope to fill that hole. It's almost interactive coding: make hole, read error, fill hole with a sensible solution; that solution can of course contain holes itself. You can keep doing this until your function is finished. Even better, functions with complex types may have only one sensible realization, so you get your code for free by just describing what the function should map to what. A friend of mine called this technique "trial and error programming", and I have to say he's probably right about it - except that in most languages it is casino style gambling, while in Haskell it much more resembles the gambling in evolution. (Actually, I know a couple of complicated functions I know very specifically what they should do and what laws they have to obey, but couldn't write them down or remember them for my life - yet using the methods describe above, I could reproduce them in a minute.)



### Custom types

Imagine how you would implement a function to search an array in your preferred language. If you find the desired item, you of course return it, but what happens when there is no such item? Chances are that you have to resort to a nonsensical value like `-1`. But what happens if your array includes negative numbers? How can you tell the "not found" `-1` from the "found, result is" `-1`? You'll probably need to either return a tuple of the value found and a boolean that indicates whether that value was actually present in the array, or search for the corresponding array index/value pointer instead, and afterwards return the element at that location or handle the error.

In Haskell, there is the `Maybe` type for cases like this. Variables of type `Maybe` fall into two kinds of values: they're either `Nothing`, or `Just <some value>`. This solves the above problem: `Nothing` stands for not found, whereas `Just x` is returned when the value `x` has been found. Look a key up in a binary map? It'll either be `Just <corresponding value>` or `Nothing`. And it doesn't stop there: functions returning `Maybe` compose. Connect to database server? Might fail (`Nothing`), might be a connection (`Just <handler>`). Login? Might fail. Select database? Might fail. What would most languages do? Exceptions? Nested ifs? Early returns? Haskell just chains these operations to each other. If one fails (i.e. evaluates to `Nothing`), their combination fails, and whatever comes after the failure is skipped.

Big deal about this `Maybe`, you might say. But that was just the canonical example of a Haskell type. In fact, although `Maybe` is a standard type, you can define it yourself very easily, with the same performance and everything. Many things you commonly use in Haskell are just defined in the source code of libraries, they are not built-in. You could have written it yourself, and in fact, that's what a large part of Haskell is about: figure out your data structure and fill in the gaps. Custom data types are a powerful **abstraction** that does not only not get in your way, but can actively help you developing algorithms.

The other great thing about commonly using custom types is the **added safety**. Suppose you have a database that contains a list of names, and the corresponding bank accounts, each entry of which comes with some meta information like an ID, and then the actual data. The problem here is that all IDs are integers. Imagine what would happen if you add 1 to an ID because you mistake it for an array index, or you use a person's ID to fetch a bank account. These kinds of bugs can be extremely devious and hard to find in most languages, because an int is an int. Not so in Haskell: create a new type wrapping int (which produces no runtime overhead by the way) for each of people and bank accounts, and the compiler will complain (type mismatch, critical error) if you use a person's ID in a function that works only with bank accounts. (You could of course write a struct holding only an int in C as well every time you do something like this, but the point is in Haskell you *will*, in C you probably won't.)



Laziness
--------

In most (all?) common programming languages, `x = 1+1` gives `x` the value `2` - values are calculated as they are assigned. In Haskell this is different: **values are only evaluated when they are needed** (and even then only to the degree they are needed), say you want to print them; merely stating `x = 1+1` will give `x` the value `1+1` and that's it. This has some interesting consequences:

- Infinite data structures, such as the list of all primes or the entire [Collatz tree][collatz]. You can often define the set of all solutions to a problem, and then always take "one more" solution from that set as you need them. A sudoku solver can generate the entire (finite, but huge) set/tree of possible boards, and then incrementally filter out the wrong ones.

- Performance-wise, you pay only for what you actually need. A program that does some complicated calculation but never actually needs its value will simply not attempt doing said calculation. Compare this to other languages, where everything has to be calculated in advance, whether or not it is needed.

- Calculations can use values before they are known. You can write functions requiring some value, and you can write them in a way so they do actual calculations with that value, passing on a *hypothetical result*, sort of like using `x` as a placeholder for something you don't know yet in math. In the end, if you *do* need its value, you can insert the actual calculation in the placeholder; if there's no placeholder, there's no need to calculate it.

- A few appetizers: You can have nonsensical computations in your data, and the code will not necessarily fail - a (finite) list full of divisions by zero still has a well-defined length, even though none of its actual elements make any sense. You can split a (singly linked, possibly infinite!) list in two parts of same size or drop the last `n` elements without ever calculating its length.

There are some pitfalls to be aware when it comes to laziness, however. Summing up a billion `1`s in C incrementally adds them to an accumulating integer; doing the same in Haskell (the naive way) builds up a giant expression `1+(1+(1+...`, which blows up your stack. Input/output is also lazy, so if you assign a file's contents to a variable and close the handle, reading that variable may fail, as the file's not known to the program anymore. Laziness is neat, but takes some getting used to - which mostly means knowing when and how to disable it if it's not the right behavior.

[collatz]: http://en.wikipedia.org/wiki/Collatz_conjecture



### Reusability

Many things in programming have similar interfaces. Think of things that have a neutral element and an associative operation: addition and 0, multiplication and 1, list concatenation and the empty list. There are many other objects that follow this concept. So why not have a common interface? Well, it's called `Monoid` in Haskell (and math). There are functions that take any monoid and put them together, using the appropriate monoid operation.

Monoids are of course just an example, there are many other so-called *type classes*. `Eq` is for things you can compare for equality; as you may expect, most things are part of this. `Ord` are ordered things, for example real numbers. Contrary to that, complex numbers aren't in `Ord`, because you can't order them in a meaningful manner (without breaking other reasonable assumptions, that is). `Enum` (for enumerable) are things that have a next and previous element.

Functions are rarely restricted more than they have to be. If you write a function, chances are that it doesn't only work for the type you have intended, but for the whole class of types that behaves similarly, and in addition, there are laws for the basic operations that ensure that these generic functions do what you think they should do, no matter what kind of an abstruse type you feed them. **Typeclasses often mean the functions you write are much more general than what you initially intended them for**, while certain laws assure that each of the unanticipated uses will do the right thing (and only that).



Glue
----

What is commonly referred to as *glue* is the ability of a language to combine different parts of a program. In Haskell, functions are generally very *small*, in the sense that they only span a handful of lines, do one specific thing, and compose very well. This composability makes it possible to have a vast amount of different functions using only a few basic ones.

A consequence of this concept is that most "large" functions are composed of smaller functions. When you know precisely how and that the smaller functions work, then you most likely know that about your combined function. It is very common to build a library of small bits, and then just export the actually useful combinations; however, only the small bits have to be tested. (Most likely, the typechecker complains when you combine them the wrong way anyway.)

If that still doesn't convice you, think of a Linux terminal. How do you use it? There's a small function for every detail, and you then chain those together. One just prints a file (`cat`), you then feed that to another function that filters out some elements (`grep` etc.), and finally you count how many entries are still left (`wc`). How many lines are in a file containing an 'a'? `cat file | grep "a" | wc`. That's precisely the kind of plumbing you do in Haskell all the time. However, contrary to long terminal expressions, they can still be very readable even if they grow past the middle of the line.



Purity
------

Purity in a nutshell means that a function cannot have side effects; a side effect is something that modifies the environment. The standard hypothetical Haskell function to do this is called `launchMissiles`. A pure function cannot launch missiles as that would modify the environment; maybe a more friendly version of that function would be reading a file or printing something in the console.

While this sounds like a huge restriction when you haven't used it much, it also has obvious benefits in this case, namely type signatures tell you a lot about what a function actually does. This argument is closely linked to the one given above where the compiler can help you write your code by suggesting functions that fit in a gap. Suppose you have a function of which you know that it maps "a list that can contain anything" to an integer. With side effects, you can't tell anything. It could look up the list's contents in a database and return the time taken. In a pure language, a function cannot talk to the outside world, which eliminates all these cases. What's left is either a constant function (that maps every list to the same number), or a function that counts elements. When you know this, you don't have to understand the code, you only have to understand it enough to exclude the other options; in this case, the function is most likely calculating the length of the list. Strong typing combined with purity means that *the function type often gives away what a function does on its own*, even if you don't know, don't want to bother with, or don't care about the implementation.

Purity also means that a function always has the same result given the same input, leaving a lot of space for compiler optimization: if it's called twice, you only have to calculate it once and distribute the results. In impure languages, deciding whether a (possibly deeply nested) function modifies the environment is hard work for the compiler, in Haskell it's a piece of cake in arbitrarily complicated cases.




Immutability
------------

In a language like C, `x = 3; x = 4;` sets `x` to `4`. The same line in Haskell would imply that `3 = 4` and the compiler would raise a critical error. All variables in Haskell are immutable, you can define them once, but then they're set in stone.

Immutability, next to purity, is probably the other concept that seems extremely odd and impractical. But again, it guarantees certain things about the code. In fact, mutability is the number one enemy of parallel/concurrent programming, leading to the concept of locking mechanisms, but those are hard to build and even harder to debug. On the other hand, you never have to write a lock for an immutable variable, and you can *always* evaluate it in parallel.

A valid argument against immutability is of course that updating a variable is often more efficient than allocating a new one, so it should have a negative impact on performance. However, this argument goes both ways: if you have an algorithm referring to a value that has been calculated before, the compiler can be sure that it hasn't changed, and can therefore combine multiple calculations of the same thing into a single pointer to the generated value. In fact, immutability opens the door to a huge amount of compiler features that would be unfeasible to do in a mutable environment.



Concurrency, parallelization
----------------------------

Beyond the benefits of immutability, one thing stands out in GHC, Haskell's main implementation: **Haskell threads are dirt cheap**, you can fork [millions][millionthreads] (!). Clients connect to your server? Give each one an own thread. Calculate the sum of a large list? Split it in ten (or ten thousand?) parts. The cost of scheduling something in parallel being so low, it means that for even relatively simple calculations the performance hit due to new threads is so small that it's barely noticeable during execution on a single core; what *is* noticeable however is the performance increase as soon as you let the program run on multiple CPUs. [(As of beginning 2013, the scheduler scales linearly 32 cores.)][32cores]

[millionthreads]: http://www.scribd.com/doc/19465418/Multicore-Programming-in-Haskell-Now
[32cores]: http://www.haskell.org/pipermail/ghc-devs/2013-February/000414.html




It's evolving
-------------

In the beginning days of Haskell, there was a serious problem with input/output (I/O): how can this concept possibly be present in a *pure* language? I/O is about modifying the outside world after all. Well, I don't know how, but some people recognized the mathematical concept of a *monad* is a suitable abstraction for this issue. Using monads completely revolutionized Haskell.

Nowadays, programming in Haskell without using monads is almost unthinkable. Luckily, using them doesn't require you to understand the math behind them. It's like you don't have to know advanced math to calculate 3+5. After reading the sections about how flexible the type system is, it probably won't surprise you that monads aren't a language feature, they're built using things that are already present in it (which takes, in its current version, 5 lines total).

Every couple of years, someone comes along and finds something really interesting that can be applied to Haskell. Some things turn out to be impractical and become history, others add a new concept to the standard libraries and are used by the entire community from that point on.

At the time of writing this (early 2013), the concept of a so-called *comonad* is in the air. A comonad is something like a backwards monad in a sense, and it is probably very useful in practice. Unfortunately, only a few people truly understand their meaning and are able to put them to good use; there are not a lot of tutorials yet, no book even mentions their name. But here's the thing: it's exciting to see the language evolve. From all I know, once comonads break through, they may be the next big thing in Haskell, and in five years someone will write about how cute it was when nobody understood these things that are now on page 200 in every Haskell book.

In Haskell, you may be there when people discover the printing press, electricity and transistors, and you will see yourself grow on the new way of writing code.



Read-Evaluate-Print-Loops (REPL)
--------------------------------

Haskell has two implementation of a REPL ([GHCi][ghci] and [Hugs][hugs]), a principle you may know from [iPython][ipython]. It's a command line tool similar to a terminal, where the commands are the functions you import. Typical Haskell workflow is working on a small function, loading it into the REPL, and seeing whether it compiles (fix errors, repeat), and whether it does what it should do. It's very helpful to play around with combinations of your functions before you plug them into your actual code. And this isn't just tinkering: this is what writing Haskell looks like. You have your editor, and your REPL, and you bounce back and forth between them. You immediately see if something is fishy after writing only ten lines; nobody writes more than a page of code in Haskell without giving it a test shot.

[ghci]:    http://www.haskell.org/ghc/docs/latest/html/users_guide/ghci.html#ghci-introduction
[hugs]:    http://www.haskell.org/hugs/
[ipython]: http://ipython.org/


LYAH
----

Learn You a Haskell for Great Good, short **LYAH, is the best introductory book for any programming language I've ever come across**. It's fun to read, and in the meantime it teaches you Haskell by accident. [Oh, and it's available for free online.][lyah]

[lyah]: http://learnyouahaskell.com/




The community
-------------

**Haskell's community is known for their friendliness**, and that they give helpful answers to even the most basic beginner questions. If you present them with your problem (e.g. on StackOverflow, the Haskell mailing lists or #haskell on irc.freenode.net) and show what you've attempted, you will most likely get an answer that not only explains how to solve your problem, but gives you a deeper insight into what you were trying to accomplish and how it relates to other things you might encounter.

If you're not a beginner, don't fear! I have yet to find an advanced topic nobody in #haskell is able to write an essay about. This of course includes Haskell, but also parallels to logic and mathematics (specifically category theory).



Notable standard libraries
--------------------------

Below are a number of noteworthy libraries, in the sense that functionality provided by them is in some way special to Haskell.

### Parsing

There are a couple of parser libraries for Haskell, and Parsec is probably the most general purpose one among them. I'm not sure you can state this objectively, but **parsing using Parsec is fun**. You basically think about how your data is structured, write that down (if you're familiar with the term, it looks like [EBNF][ebnf]), and it compiles and works. Want to parse a letter or a digit? `letter <|> digit`. Many letters or digits? `many (letter <|> digit)`. It's short, elegant and maintainable - you can't ask for much more. Parsec is probably also the reason regular expressions aren't used much at all (although they are supported as well).

[ebnf]: http://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_Form

### [Software transactional memory (STM)][STM]

When you have two independent threads sharing the same data, the usual approach to avoid data inconsistency or corruption is using locks. However, while solving the one problem, new ones - for example deadlocks - are introduced, which can be very hard to debug. Concurrency is usually non-deterministic (a thread may modify data before or after another thread has read it, depending on how busy they are), and this is no different in Haskell: it is executed on an impure level, and therefore it has the same safety as in any other language. Values *inside* the concurrent operation *may* be pure, but in general the whole thing isn't.

STM is a library that largely solves this problem. STM actions are often similar to normal concurrent operations, but with an important difference: they can be composed, and executed as a single atomical operation. Other threads either see the action as not started yet, or already finished - there is no way of having the intermediate state. For this reason, other threads cannot corrupt any of the used data - **STM allows concurrency without locks by design**, and therefore doesn't suffer from their problems, while being applicable to mostly the same problems.

Of course STM is not a silver bullet: the performance is not as good as with raw locks - which are of course also available - and it's not suitable for huge computations where thousands of variables are exchanged in a single atomic operation. It is a very useful addition to, and not a replacement for, the classical locking mechanisms.

[STM]: http://en.wikipedia.org/wiki/Software_transactional_memory

### QuickCheck

**QuickCheck is a quality assurance library** to test functions. It does this by **automatically generating appropriate datasets** (enabled to do so by Haskell's type system), and then checking whether a function's property holds for this generated data. Often times, functions have only a couple of different corner cases (think of "empty list, list with only one element, list of many"), and QuickCheck is very likely to find them. As an addition to a programmer who understands the code and *thinks* it's alright gives a very high confidence level of the function actually being correct.



Double-edged things about Haskell
=================================



Relearning how to program
-------------------------

Learning Haskell without a doubt feels like learning to program all over again. At least from my perspective, I couldn't imagine having it learned as not a hobby. Tinkering around until you understand all the new concepts may be something that's not for everyone, and you have to do that over and over again.



Going back
----------

After becoming proficient in Haskell, you will miss things in any other language you write. You will miss the basic things like `Maybe` as much as you'll miss the advanced features like the type safety, the ability to use functions as data, parameters and return values easily.

I am serious about this: learning Haskell *will* make you feel somewhat disabled in most other languages (i.e. probably all but Lisp), which may not be a good thing.



Going forward
-------------

A wise man on the internet once said that in the beginning, Haskell feels like thousands of people keep telling you how ingenious Haskell is, while the entirety of the language seems to be about hacking your way back around its limitations. You will think about how quickly you could've written more performant code in C in half the time - and you will be right assuming so - so sometimes it's a challenge to not lose interest. Becoming somewhat productive takes a hobbyist at least a couple of months, for me it was roughly a year until I felt as confident in Haskell as in C++, my previous main language. But then, how long did it take me to learn my very first imperative language?



The mathiness of the community
------------------------------

If you join `#haskell` on `irc.freenode.net`, you'll most likely encounter a discussion about category theory, logic or some other academic topic quite quickly. This has led to the prejudice that these advanced concepts are necessary to learn Haskell, *which they are absolutely not*. In fact, many of these concepts are applicable to many other programming languages, only that nobody recognizes them or bothers to talk about it. Haskell sure creates an environment where people gather that are interested in such matters, but it's not a requirement in order to learn how to use it.



Code levels
-----------

If you see well-written code by someone who has a much higher skillset than you in say C, C++ or Java, you'll understand what it's doing after reading it a couple of times, even if you wouldn't be able to write it yourself. In Haskell, code using concepts you're not familiar with may be completely unreadable, no matter how well-written and well-documented it is. Above I said you don't need to learn or use advanced math to write Haskell, but if you want to, you've surely come to the right place. On the other hand, the basic modules are all written in just as basic style, so you won't encounter this all that much until you download your first super flexible, extremely useful and well-documented library, learn to use it, and then dare to take a look at its inside.

Mind you this is not to say you can't use this code: you can still read the type and the documentation. There are a few libraries around that are as useful and easy to use as their internals are - probably for that very reason - complicated.



Everyone thinks you're weird
----------------------------

First, get a mirror. Yes, you. Now.

"I like this language that has no loops, all variables are constant while functions take only one argument, and evaluates program parts in whatever order it wants to."

Now look at the mirror. That's what people will look like when you tell them that. Right now you probably think this is a joke. It's not.



The code looks different
------------------------

I think Lisp looked unappealing, but I found the concepts of the language interesting enough to give it a shot anyway. Syntax is something you get used to easily. It's of course hard to start something that you don't like looking at, but I recommend not making this a limitation. Read the introductory chapters of a book, and see whether you like what the text tells you between the code segments, and make your judgement based on that.



Learning monads
---------------

This thing called *monad* is for many people (including me) the first brick wall they hit when learning Haskell. It's sort of a running gag for people to say that everyone who finally understands monads feels like writing a tutorial about how it's *actually* done, and these tutorials tend to be more confusing than helpful to others. I can't explain what monads are or what they are useful for in a sentence here, but then how would you explain someone what polymorphism is in five lines? There are certain concepts you have to work with a little to understand their value, and when it is appropriate to use them. Let me conclude this with a word of encouragement: while the first encounters with monads are quite confusing, after some time you *will* understand them, not only that - when you do so, they're so crystal clear that you won't be able to recall what your problem with them was.





What makes Haskell bad
======================



You *have* to like it
---------------------

From how I see it, it is impossible to learn Haskell when you don't like learning it. Certain concepts, even basic ones, require you to play around with them, and you won't understand how they work even if you read the book chapter a hundred times. If you don't like Java and someone wants you to learn and write Java, then you'll be a bad Java programmer after reading some book twice (I'm talking about myself here). You'll be able to write production code, you'll learn by practically applying your skills, and after some time you'll be alright at it. Not so in Haskell. A beginner's book will have at least a couple of sections that are simply over your head at first, but if you skip or skim them you won't be bad at Haskell, you simply will not be able to produce anything useful.



The library phase
-----------------

Once you're past the first steps (e.g. after finishing LYAH), there's nothing to guide you really. It's what I call the library phase: many problems already have libraries to solve them, but you have to find out about them first, and then learn their API. Some concepts of functional problems require approaches that you wouldn't expect as an imperative programmer, making it hard to find the according library; `#haskell` will be very helpful here though.



Small hacks
-----------

Haskell's safety and robustness comes at a price. Sure, it will most likely do the right thing for all possible cases up to eternity, but sometimes that's just not what you want: a small dirty hack can accomplish a lot. So when you're trying to find a pragmatic solution to a small problem, this is probably the better approach. `ls -a | grep ^\\. | wc -l` is much more likely what you want when counting hidden files than writing the Haskell equivalent.



Ugly warts
----------

Alright, here it comes, things that range from debatable to straight up awful, and are mostly attributed to historical accidents:

- Mathematically, all monads are funcors, but Haskell does not enforce this hierarchy. You can easily derive a functor from any monad, but if you write a library you always have to take them into account separately.

- Exceptions are defined in `Control.Monad.Error`, and errors in `Control.Exception`.

- There is no unsigned arbitrary-sized integer type.

- Some standard functions throw an error (you remember, the things from `Exception`) if the input is mal-formed, instead of using other means (such as returning a `Maybe` value).

- `fail`. We don't talk about this one.












