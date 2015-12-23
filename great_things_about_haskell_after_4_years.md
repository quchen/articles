About Haskell, 4 years later
============================

I wrote a rather long article about what I thought was great about Haskell a couple of months after it became my main language for most programming endavours.

Since then, a lot has changed in my life: I'm not a student but a professional programmer now, working on a large scale system written in popular object-oriented languages. I've participated in a *lot* of IRC discussions about the nitty details of the Haskell language. I've seen many talks, and read many papers: about new programming techniques, how FP and Haskell in particular differ from OOP on an architectural level, and how to sell Haskell.

This is an article in the same spirit as the old one, but its contents are vastly different.

- I'm not euphoric about the language anymore, in the way you're not euphoric about that car you bought two years ago anymore. Instead, I'm deeply impressed by its design and practicality, and still confuzzled about why many of its features are only just now beginning to be adopted by other languages.

- I see the political aspect of adopting new technologies for a mission critical project on a daily basis. There is technical debt, the availability of skilled programmers, inertia, skepticism, competing technologies promising similar benefits.

- In my daily work, I am constantly comparing my OOP solutions to how I would have done them in Haskell, meeting both strengths and weaknesses of both paradigms.

- I have the privilege to work with a very well-written OOP codebase filled with many patterns I would consider idiomatic usage of the language. I think this greatly helps avoiding the potential "Haskell is better than this legacy code solution" pitfall.



Easiness of refactoring
-----------------------

A program is never finished. There comes a time in the lifecycle of any implementation at which point it needs to change significantly, and because our brains cannot hold a lot of information at the same time, we need something to help us cope with the complexity. I've seen many ways of doing this.

**Write tests.** A test describes an invariant we expect our code to follow. The more tests we have the more solid our code becomes. Tests can be seen as one form of a specification we can check our intentional changes against. A good testsuite can give us high confidence in the soundness of our refactorings, but unfortunately they also tend to be a lot of work, and require constant maintenance. A non-technical manager might think it's the responsibility of the QA to do this sort of work, and miss the value of how much time and money the ability to do very quick checks of complicated features can provide to a project.

**Debugger-driven development:** find your way to the error, wrap it in an `if(specialCondition)`, and be done with it. This way of coding does work very well in the short term, but after not so long the code will be a mess beyond what a person can understand. New features become increasingly expensive in terms of money, and also in terms of morale of those who have to maintain it. These quick fixes are very appealing, but they infect a code base quickly, and make the cost of changes unforseeable.

**Types.** A type tells not only us, but also the compiler, a lot about what something can possibly do; and more importantly, what it *cannot* do. Making the most of a type system means structuring the program in such a way that errors are not representable within the program: even the most careless programmer could not put it to bad use. A programming lanuage with a good type system helps you with its types, and prevents you from doing stupid things. None of the standard programming languages have anything I'd even remotely consider a good type system.

Now let's refactor our codebase. We'd like to perform open heart surgery, because there is a new requirement how the core of our program should handle that new parameter.

If we wrote **tests**, we can implement the feature to the best of our knowledge, and hit go. We now have to work our way through the phenomenological evidence that "test says no" provides us with, find out the purpose of each test, think about how the code could have produced the condition. This is laborious, but depending on the quality of the testsuite, a sure thing. After the new feature is in, the testsuite needs to adapt to the new conditions.

If we went with the **debugger**, we're completely and utterly fucked. We can either refactor everything (no-go from a business perspective), or try to bolt our change in in the same manner. It will be bad, the QA will have lots of work to do, and the headaches will bounce back and forth until we cross our fingers that production will somehow work.

If we wrote **types**, we do the change, and follow compiler errors. This is what we do in Haskell most of the time: just do the core change, don't worry about anything else, and let the compiler tell you where it doesn't allow things to be the way they were now that you've done your thing. It's a revealing moment in the life of most Haskell programmers to write a nontrivial program, never run it in the development process, and once it typechecks in the end it *works*. Imagine you run a marathon, and in the end you realize you don't need a shower because you didn't sweat. I imagine most marathon runners can't imagine this happen, after all they've got years of experience of how running works.




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
