About Haskell, 4 years later
============================

I wrote a rather long article about what I thought was great about Haskell a couple of months after it became my main language for most programming endavours.

Since then, a lot has changed in my life: I'm not a student but a professional programmer now, working on a large scale system written in a popular object-oriented language. I've participated in a *lot* of IRC discussions about the nitty details of the Haskell language. I've seen many talks, and read many papers: about new programming techniques, how FP and Haskell in particular differ from OOP on an architectural level, and how to sell Haskell.

This is an article in the same spirit as the old one, but its contents are vastly different.

- I'm not euphoric about the language anymore, in the way you're not euphoric about that car you bought two years ago anymore. Instead, I'm deeply impressed by its design and practicality, and still confuzzled about why many of its features are only just now beginning to be adopted by other languages.

- I see the political aspect of adopting new technologies for a mission critical project on a daily basis. There is technical debt, the availability of skilled programmers, inertia, skepticism, competing technologies promising similar benefits.

- In my daily work, I am constantly comparing my OOP solutions to how I would have done them in Haskell, meeting both strengths and weaknesses of both paradigms.

- I have the privilege to work with a very well-written OOP codebase filled with many patterns I would consider idiomatic usage of the language. I think this greatly helps avoiding the potential "Haskell is better than this legacy code solution" pitfall.



Avoidance of implicit state
---------------------------



Avoidance of invalid state
--------------------------

In Haskell, there is a very strong social convention that any function should be well-behaved, as long as its input is well-typed.



Types actively helping development
----------------------------------

Coming from a Java background, dealing with types and generics is not exactly a thing of pleasure.

One argument in favour of strong type systems is usually how many bugs they avoid. This, unfortunately, is very hard to quantify, as we don't notice the bugs we didn't write. But let us consider some other points of view of the same thing:

- When a functions definition is not absolutely clear, it should be subject to testing. But this is sometimes a problem: whenever one wants to check that some bad condition does not happen, one needs to formulate that in the test program. But the typechecker often simply rejects the test code, because the bad condition is not even representable in code! *Lots* of trivial test cases are eliminated this way.

- In a well-tested library, the number of bugs that the testsuite catches in Haskell is usually tiny, with the majority of bad conditions caught at compile time. In my pop-OOP code, it's just the other way round. Let's compare the two situations:

   - A typechecker finds *logical errors* in the program. It reports a type error even if the program "should work for all practical purposes", i.e. including even the most obscure corner cases.

   - In popular OOP languages, the typechecker (or the type system) is very weak, and the burden of describing desirable behaviour is on the programmer. A test only checks what the implementor thought was worth checking.

   - In Haskell, the type system is very strong. Here, it is the burden of the programmer to design the code in a way that it does not allow undesirable state to be representable, by designing good data types for a specific problem.
