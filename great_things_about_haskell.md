Foreword
========



The following is an attempt to write up why I think Haskell is a great language. While it is certainly somewhat biased, I'll try my best to be as objective as possible.

One thing worth noting is that most of the things I'm mentioning *could* be done in many other languages as well, but it's not common practice, mostly because the language isn't designed to house this kind of concept for practical purposes. So each time you think "this argument is bad, I could do that in X as well", think about how likely it would be that you actually do it - if you still think I'm wrong, I'll happily take your suggestion into consideration.




Great things about Haskell
==========================



The type system
---------------

### Strong vs. weak typing

The good thing about strong typing, like encountered in C, C++ or Java, is that it catches a whole class of bugs during compilation: trying to compare apples with oranges, the compiler will raise a type mismatch error. If you *do* want to compare apples to oranges, you first have to at least convert one into the other. The downside of this is that you have to specify a type signature for every function ("this function takes an integer and returns a boolean"), and if you have to convert many apples to oranges, the code will be full of "toOranges(apple)" calls.

On the other side, there's weak typing, as seen in e.g. Python and JavaScript. There you don't have to give type annotations, saving you a lot of writing, making the code clearer in many cases. The cost of this is of course the introduction of bugs of the type mentioned above.

Haskell has the best of both worlds: at its core, it is a strongly typed language: you can't compare apples to oranges. However, Haskell doesn't require type annotations: based on the operations you use in a function, Haskell infers the type of all the operations and their combination on its own - it writes the type signatures automatically during compilation! You gain the **safety of strong typing at the convenience of weak typing**. Speaking of strong typing: Haskell's typing is even stronger than the ones of the languages mentioned above. Comparing the integer 0 with the string "0" will result in an error; Haskell *never* converts anything automatically. However, the need to do explicit conversions is surprisingly rare, so there is no real code clutter. In fact, if you feel like you have too many typecasts in a small block of code, there's probably a better way of writing it.

Also worth noting is that you can turn that principle around: by giving a function an explicit type signature, you tell the compiler what kind of function you want. You then write the implementation of that function to the best of your knowledge, and hit compile. Two possible outcomes: either your compiler is fine with what you wrote, or it complains. However, it tells you exactly where it complains and why. Often times, the compiler will guide you through writing your function, not just the types! "Hey, there's something wrong here, expected that". You look at the few options you have, think about them, put one there, and retry until it compiles. **If it compiles, chances are *very* high that the function does what you think it should do**. Yanking this up even more, you get typed holes - you basically leave a placeholder in your code. The compiler tells you what type the hole has, and suggests functions that are in scope to fill that hole. It's almost interactive coding: make hole, read error, fill hole with a sensible solution; that solution can of course contain holes itself. You can keep doing this until your function is finished. Even better, functions with complex types may have only one sensible realization, so you get your code for free by just describing what the function should map to what.



### Custom types

Imagine how you would implement a function to search an array in your preferred language. If you find the desired item, you of course return it, but what happens when there is no such item? Chances are that you have to resort to a nonsensical value like `-1`. But what happens if your array includes negative numbers? How can you tell the "not found" `-1` from the "found, result is" `-1`? You'll probably need to either return a tuple of the value found and a boolean that indicates whether that value was actually present in the array, or search for the corresponding array index instead, and afterwards returning the element at that index or handle the error if the "found" index is `-1`.

In Haskell, there is the `Maybe` type for cases like this. Variables of type `Maybe` fall into two kinds of values: they're either `Nothing`, or `Just <some value>`. This solves the above problem: `Nothing` stands for not found, whereas `Just x` is returned when the value `x` has been found. And it doesn't stop there: functions returning `Maybe` compose. Connect to database server? Might fail (`Nothing`), might be a connection (`Just <handler>`). Login? Might fail. Select database? Might fail. What would most languages do? Exceptions? Nested ifs? Early returns? Haskell just chains these operations to each other. If one fails, their combination fails, and whatever comes after the failure is skipped.

Big deal about this `Maybe`, you might say. But that was just the canonical example of a Haskell type. In fact, although `Maybe` is a standard library type, you can define it yourself very easily, with the same performance and everything. Many things you commonly use in Haskell are just defined in the source code of libraries, they are not built-in. You could have written it yourself, and in fact, that's what a large part of Haskell is about: figure out your data structure and fill in the gaps. Custom data types are a powerful **abstraction** that does not only not get in your way, but can actively help you developing algorithms.

The other great thing about commonly using custom types is the **added safety**. Suppose you have a database that contains a list of names, and the corresponding bank accounts, each entry of which comes with some meta information like an ID, and then the actual data. The problem here is that all IDs are integers. Imagine what would happen if you add 1 to an ID because you mistake it for an array index, or you use a person's ID to fetch a bank account. These kinds of bugs can be extremely devious and hard to find in most languages, because an int is an int. Not so in Haskell: create a new type wrapping int (which produces no runtime overhead by the way) for each of people and bank accounts, and the compiler will complain (type mismatch, critical error) if you use a person's ID in a function that works only with bank accounts. (You could of course write a struct holding only an int in C as well every time you do something like this, but the point is in Haskell you *will*, in C you probably won't.)



### Reusability

Many things in programming have similar interfaces. You can add two numbers, adding 0 to anything doesn't change it, and it doesn't matter where you put the parentheses in `1+2+3`. You can multiply numbers, 1 doesn't change the result, and parentheses in `1*2*3` don't matter. So much for elementary operations. But there's more, in fact a lot more. When you concatenate a list and the empty list, you get back the list. When you concatenate three lists, it doesn't matter how you group the concatenation, you'll always get the same result. There are many other objects that obey these two simple rules. So why not have a common interface? Well, it's called `Monoid` in Haskell (and math). There are functions that take any monoid and put them together, using the appropriate monoid operation.

Monoids are of course just an example, there are many other so-called *type classes*. `Eq` is for things you can compare for equality; as you may expect, most things are part of this. `Ord` are ordered types, for example real numbers. Complex numbers aren't in `Ord` for example, because you can't order them in a meaningful manner. `Enum` (for enumerable) are things that have a next and previous element.

Functions are rarely restricted more than they have to be. If you write a function, chances are that it doesn't only work for the type you have intended, but for the whole class of types that behaves similarly, and in addition, there are laws for the basic operations that ensure that these generic functions do what you think they should do, no matter what kind of an abstruse type you feed them.



Glue
----

What is commonly referred to as *glue* is the ability of a language to combine different parts of a program. In Haskell, functions are generally very small - a handful of lines - and do one very specific thing. When programming, you then combine these simple functions. Want the first five positive list elements? Chain `take 5` together with `filter (> 0)`. Haskell has many individual, very reusable functions, and glueing them together makes it possible to have a vast amount of different functions using only a few basic ones.

A consequence of this concept is that most functions are composed of smaller functions. When you know precisely how and that the smaller functions work, then you most likely know that about your combined function. It is very common to build a library of small bits, and then just export the actually useful combinations; however, only the small bits have to be tested. (Most likely, the typechecker complains when you combine them the wrong way anyway.)

If that still doesn't convice you, think of a Linux terminal. How do you use it? There's a small function for every detail, and you then chain those together. One just prints a file (`cat`), you then feed that to another function that filters out some elements (`grep` etc.), and finally you count how many entries are still left (`wc`). How many lines are in a file containing an 'a'? `cat file | grep "a" | wc`. That's precisely the kind of plumbing you do in Haskell all the time.



Purity
------

Purity in a nutshell means that a function cannot have side effects; a side effect is something that modifies the environment. The standard hypothetical Haskell function to do this is called `launchMissiles`. A pure function cannot launch missiles as that would modify the environment; maybe a more friendly version of that function would be reading a file or printing something in the console.

While this sounds like a huge restriction when you haven't used it much, it also has obvious benefits in this case, namely type signatures tell you a lot about what a function actually does. This argument is closely linked to the one given above where the compiler can help you write your code by suggesting functions that fit in a gap. Suppose you have a function of which you know that it maps "a list that can contain anything" to an integer. With side effects, you can't tell anything. It could look up the list's contents in a database and return the time taken. In a pure language, a function cannot talk to the outside world, which eliminates all these cases. What's left is either a constant function (that maps every list to the same number), or a function that counts elements. When you know this, you don't have to understand the code, you only have to understand it enough to exclude the other options; in this case, the function is most likely calculating the length of the list. Strong typing combined with purity means that *the function type often gives away what a function does on its own*, even if you don't know, don't want to bother with, or don't care about the implementation.

Purity also means that a function always has the same result given the same input, leaving a lot of space for compiler optimization: if it's called twice, you only have to calculate it once and distribute the results. In impure languages, deciding whether a (possibly deeply nested) function modifies the environment is hard work for the compiler, in Haskell it's a piece of cake in arbitrarily complicated cases.




Immutability
------------

In C, `x = 3; x = 4;` sets `x` to `4`. The same line in Haskell would imply that `3 = 4` and the compiler would raise a critical error. All variables in Haskell are immutable, you can define them once, but then they're set in stone.

Immutability, next to purity, is probably the other concept that seems extremely odd and impractical. But again, it guarantees certain things about the code. In fact, mutability is the number one enemy of concurrent programming, leading to the concept of locking mechanisms, but those are hard to build and even harder to debug. On the other hand, you never have to write a lock for an immutable variable.




It's evolving
-------------

In the beginning days of Haskell, there was a serious problem with input/output (IO): how can this concept possibly be present in a *pure* language? IO is about modifying the outside world after all. Well, I don't know how, but some people recognized the mathematical concept of a *monad* is a suitable abstraction for this issue. Using monads completely revolutionized Haskell.

Nowadays, programming in Haskell without using monads is almost unthinkable.Luckily, using them doesn't require you to understand the math behind them. It's like you don't have to know advanced math to calculate 3+5. After reading the sections about how flexible the type system is, it probably won't surprise you that monads aren't a language feature, they're built using things that are already present in it.

Every couple of years, someone comes along and finds something really interesting that can be applied to Haskell. Some things turn out to be impractical and become history, others add a new concept to the standard libraries and are used by the entire community from that point on.

At the time of writing this (early 2013), the concept of a so-called *comonad* is in the air. A comonad is something like a backwards monad in a sense, and it is probably very useful in practice. Unfortunately, only a few people truly understand their meaning and are able to put them to good use; there are not a lot of tutorials yet, no book even mentions their name. But here's the thing: it's exciting to see the language evolve. From all I know, once comonads break through, they may be the next big thing in Haskell, and in five years someone will write about how cute it was when nobody understood these things that are now on page 200 in every Haskell book.

In Haskell, you may be there when people discover the printing press, electricity and transistors, and you will see yourself grow on the new way of writing code.



Read-Evaluate-Print-Loops (REPL)
--------------------------------

Haskell has two implementation of a REPL ([GHCi][ghci] and [Hugs][hugs]), a principle you may know from [iPython][ipython]. It's a command line tool similar to a terminal, where the commands are the functions you import. Typical Haskell workflow is working on a small function, loading it into the REPL, and seeing whether it compiles (fix errors, repeat), and whether it does what it should do. It's very helpful to play around with combinations of your functions before you plug them into your actual code. And this isn't just tinkering: this is what writing Haskell looks like. You have your editor, and your REPL, and you bounce back and forth between them. You immediately see if something is fishy after writing only ten lines; nobody writes more than a page of code in Haskell without giving it a test shot.



LYAH
----

Learn You a Haskell for Great Good, short LYAH, is the best introductory book for any programming language I've ever come across. It's fun to read, and in the meantime it teaches you Haskell by accident. [Oh, and it's available for free online.][lyah]



The community
-------------

Haskell's community is known for their friendliness, and that they give helpful answers to even the most basic beginner questions. If you present them with your problem (e.g. on StackOverflow, the Haskell mailing lists or #haskell on irc.freenode.net) and show what you've attempted, you will most likely get an answer that not only explains how to solve your problem, but gives you a deeper insight into what you were trying to accomplish and how it relates to other things you might encounter.

If you're not a beginner, don't fear! I have yet to find an advanced topic nobody in #haskell is able to write an essay about. This of course includes Haskell, but also parallels to logic and mathematics (specifically category theory).



Notable standard libraries
--------------------------

Below are a number of noteworthy libraries, in the sense that functionality provided by them is in some way special to Haskell.

### Parsing

There are a couple of parser libraries for Haskell, and Parsec is probably the most general purpose one among them. I'm not sure you can state this objectively, but using Parsec is *fun*. You basically think about how your data is structured, write that down (if you're familiar with the term, it looks like [EBNF][ebnf]), and it compiles and works. Want to parse a letter or a digit? `letter <|> digit`. Many letters or digits? `many (letter <|> digit)`. It's short, elegant and maintainable - you can't ask for much more. Parsec is probably also the reason regular expressions aren't used much at all (although they are supported as well).

### Software transactional memory (STM)

STM is a tool for inter-thread communication in concurrent programming. Usually, you have to worry about data consistency between threads, and as a consequence locks/mutexes are introduced, leading to a framework where bugs are fiendishly difficult to find and very easy to implement. Meet STM: an STM action is an atomical operation, in the sense that it either hasn't been performed yet, or has been performed completely. There is no way a thread can see an intermediate state, hence there is no need for locks/mutexes. By its very design, it eliminates some of the most complicated bugs of cuncurrent programming.

### QuickCheck

QuickCheck is a quality assurance library to test functions. It does this by *automatically* generating appropriate datasets (enabled to do so by Haskell's type system), and then checking whether a function's property holds for this generated data. Often times, functions have only a couple of different corner cases (think of "empty list, list with only one element, list of many"), and QuickCheck is very likely to find them. As an addition to a programmer who understands the code and *thinks* it's alright gives a very high confidence level of the function actually being correct.



Double-edged things about Haskell
=================================



### Relearning how to program

Learning Haskell without a doubt feels like learning to program all over again. At least from my perspective, I couldn't imagine having it learned as not a hobby. Tinkering around until you understand all the new concepts may be something that's not for everyone. And you have to do that over and over again.



### Going back

After becoming proficient in Haskell, you will miss things in any other language you write. You will miss the basic things like `Maybe` as much as you'll miss the advanced features like the type safety, the ability to use functions as data, parameters and return values easily.

I am serious about this: learning Haskell *will* make you feel somewhat disabled in most other languages. (And by most I probably mean all but Lisp)



### Going forward

A wise man on the internet once said that in the beginning, Haskell feels like thousands of people keep telling you how ingenious Haskell is, while the entirety of the language seems to be about hacking your way back around those obvious limitations. You will think about how quickly you could've written more performant code in C in half the time - and you will be right to do so - so sometimes it's a challenge to not lose interest. Becoming somewhat productive takes a hobbyist at least a couple of months, for me it was roughly a year until I felt as confident in Haskell as in C++, my previous main language.



### The mathiness of the community

If you join `#haskell` on `irc.freenode.net`, you'll most likely encounter a discussion about category theory, logic or some other academic topic quite quickly. This has led to the prejudice that these advanced concepts are necessary to learn Haskell, *which they are absolutely not*. In fact, many of these concepts are applicable to many other programming languages, only that nobody recognizes them or bothers to talk about it. Haskell sure creates an environment where people gather that are interested in such matters, but it's not a requirement in order to learn how to use it.



### Code levels

If you see well-written code by someone who has a much higher skillset than you in say C, C++ or Java, you'll understand what it's doing after reading it a couple of times, even if you wouldn't be able to write it yourself. In Haskell, code using concepts you're not familiar with may be completely unreadable, no matter how well-written and well-documented it is. Above I said you don't need to learn or use advanced math to write Haskell, but if you want to, you've surely come to the right place. On the other hand, the basic modules are all written in pretty basic style, so you won't encounter this all that much until you download your first super flexible, extremely useful and well-documented library, learn to use it, and then dare to take a look at its inside.



### Everyone thinks you're weird

First, get a mirror. Yes, you. Now.

"I like this language that has no loops, all variables are constant, and evaluates program parts in whatever order it wants to."

Now look at the mirror. That's what people will look like when you tell them that. Right now you probably think this is a joke. It's not.



### The code looks different

I think Lisp looked unappealing, but I found the concepts of the language interesting enough to give it a shot anyway. Syntax is something you get used to easily. It's of course hard to start something that you don't like looking at, but I recommend not making this a limitation. Read the introductory chapters of a book, and see whether you like what the text tells you between the code segments, and make your judgement based on that.





What makes Haskell bad
======================



### You have to like it

From how I see it, it is impossible to learn Haskell when you don't love learning about it. Certain concepts, even basic ones, require you to play around with them, and you won't understand how they work even if you read the book chapter a hundred times. If you don't like Java and someone wants you to learn and write Java, then you'll be a bad Java programmer after reading some book twice (I'm talking about myself here). You'll be able to write production code, you'll learn by practically applying your skills, and after some time you'll be alright at it. Not so in Haskell. A beginner's book will have at least a couple of sections that are simply over your head at first, but if you skip or skim them you won't be bad at Haskell, you simply won't be able to produce anything useful.



### The library phase

Once you're past the first steps (e.g. after finishing LYAH), there's nothing to guide you really. It's what I call the library phase: many problems already have libraries to solve them, but you have to find out about them first, and then learn their API. Some concepts of functional problems require approaches that you wouldn't expect as an imperative programmer, making it hard to find the according library; #haskell will be very helpful here though.



### Small hacks

Haskell's safety and robustness comes at a price. Sure, it will most likely do the right thing for all possible cases up to eternity, but sometimes that's not what you want: a small command line hack can accomplish a lot. So when you're trying to find a pragmatic solution to a small problem, this is probably the better approach. In short: `ls -a | grep ^\\. | wc -l` is much more likely what you want when counting hidden files than writing the Haskell equivalent.














[ghci]: http://www.haskell.org/ghc/docs/latest/html/users_guide/ghci.html#ghci-introduction
[ipython]: http://ipython.org/
[hugs]: http://www.haskell.org/hugs/
[ebnf]: http://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_Form
[lyah]: http://learnyouahaskell.com/