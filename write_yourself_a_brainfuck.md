Write yourself a Brainfuck in an hour
=====================================




Introduction
------------


This is a basic tutorial on how to create a fully featured Brainfuck interpreter in Haskell. Brainfuck is a (Turing complete!) programming language that aims at having very little syntax, and because of its simple instruction set it is very easy to implement.

This tutorial is *very* basic: having read LYAH is good enough. In fact, we'll see that we don't even need any Functors/Applicatives/Monads - everything will be pattern-matching, custom data types, and of course function application. To be more specific, we'll be using LYAH chapters 1-6, 8, very little of 9, and the beginning of 14.

The tutorial is divided into three parts. First, we'll create a data type to represent Brainfuck source code, and how to convert Brainfuck source code into this format. In the second part, we'll model the tape and convert the Brainfuck source code into a more suitable representation. The final section will be evaluation, in which we walk around on the tape doing the operations the source dictates.




### Full language specification

Brainfuck source consists of a sequence of control characters, an infinitely long tape filled with zeros, and the so-called data pointer which points to the initial position on the tape. The current symbol in the code is read and executed, then the next symbol is looked at. This is repeated until the last instruction has been handled.

- `>` moves the data pointer one field to the right.
- `<` moves the data pointer one field to the left.
- `+` adds 1 to the current field.
- `-` subtracts 1 to the current field.
- `.` prints the character that corresponds to the value of the current field (ASCII).
- `,` reads a single character from the keyboard and stores its value in the current cell.
- `[` if the current value is zero, then jump forward to the command after the matching ] command.
- `]` if the current value is nonzero, then jump back to the command after the matching [ command.
- Any other character is treated as a comment.

That's it. Note how the only possible syntax error is mismatching brackets.



### A few examples of Brainfuck code

To give you a glimpse at the abomination we're about to create:

- `[-]` clears a cell containing a positive value: When the `[` is reached, the cell is either zero already (in which case the program jumps after the matching `]`, i.e. the program terminates), or nonzero. In the nonzero case, the `[` is simply ignored, `-` is evaluated and decrements the current cell by one, and `]` is reached. If the cell is zero now then ignore the `]` and terminate, otherwise jump back to after the `[`. From this you can see how `[ ]` acts as a loop.

- `>[-]<[->+<]` moves the data from one cell one cell to the right (overwriting its value). First, the data pointer moves one to the right with `>`, then there's the "clear cell" command `[-]` you know from above, and the pointer moves back. What we have now is a cell with some content, which has an empty neighbour to its right. Now the second loop starts: decrement the current cell, move one to the right, increment neighbour cell, move left again. This loop's body takes the content of the left cell and puts it into the right cell, one by one, until the left cell is empty. In the end, the pointer will be on the empty left cell, while the neighbour cell is filled with the data.

- To print the letter "a", which has ASCII value 97, use `++++++++++[>++++++++++<-]>---.`. This initializes cell 0 with 10, and then increments cell 1 ten times by another 10, giving us 100 in cell 1. Finally we subtract 3 and print the result. The [standard "Hello World" program][helloworld] is built based on this principle.

[helloworld]: http://en.wikipedia.org/wiki/Brainfuck#Hello_World.21

As you can see, the programs are not very readable. How lucky we are that we're not trying to write but to implement Brainfuck, which is much easier.





Part 1: Brainfuck type, and how to parse (to) it
------------------------------------------------

For the data type, we'll simply create one that has a constructor for each syntactic element:

```haskell
data BrainfuckCommand = GoRight      -- >
                      | GoLeft       -- <
                      | Increment    -- +
                      | Decrement    -- -
                      | Print        -- .
                      | Read         -- ,
                      | LoopL        -- [
                      | LoopR        -- ]
                      | Comment Char -- anything else
```

Let's also create a type synonym to represent an entire Brainfuck program,

```haskell
type BrainfuckSource = [BrainfuckCommand]
```

Our goal is now taking a string like `[-]` and convert it into the Haskell value `[LoopL, Decrement, LoopR]`. Since we're traversing the input string character by character (because each instruction is just one character long), we can just use `map` to convert between String and `BrainfuckSource`:

```haskell
parseBrainfuck :: String -> BrainfuckSource
parseBrainfuck = map charToBF
      where charToBF '>' = GoRight
            charToBF '<' = GoLeft
            charToBF '+' = Increment
            charToBF '-' = Decrement
            charToBF '.' = Print
            charToBF ',' = Read
            charToBF '[' = LoopL
            charToBF ']' = LoopR
            charToBF  c  = Comment c
```

That's pretty much it for this part. Note how anything that does not match a valid command is interpreted as a comment in the last line.



### Exercises


Here are some improvements you could make in your code. (Don't worry if you don't do them, the rest of the tutorial will not take them into account.)


1. (easy) Comments are not needed for evaluation, so we could just drop them from the source when we parse it. Look up what `mapMaybe` does (for example using [Hoogle][hoogle]) and use it to replace the `map` in `parseBrainfuck` so that comments are ignored.

  [hoogle]: http://www.haskell.org/hoogle/


2. (easy) You cannot write a `Show` instance for a type synonym like `BrainfuckSource` (why?). Rewrite it using a `data` declaration to a type that holds a `[BrainfuckCommand]`, and define a `Show` instance for that new type that prints the contained source. For example, `show $ BFSource [LoopL, Decrement, LoopR]` should yield "[-]" in GHCi.


3. Checking syntax

   1. (medium) The only possible syntax error in Brainfuck code is mismatching parentheses. For example `[-` has an opening but no matching closing parenthesis, and `-]` has a closing parenthesis but no corresponding opening one. Write a function `checkSyntax` of type `BrainfuckSource -> Maybe BrainfuckSource` that returns `Nothing` if the code is invalid, and otherwise `Just <valid code>`. Use this function to modify `parseBrainfuck` to reject invalid code, which then has the new type `String -> Maybe BrainfuckSource`.

   2. (hard) Modify the `Maybe BrainfuckSource` type to `Either String BrainfuckSource`. Correct code should yield `Right <valid code>`, while incorrect one should result in a `Left` value telling the user about the problem, for example "mismatched opening parenthesis".

   3. (hard) Make the error messages from above better: make the error messages tell the user about the position of the offending parenthesis, for example "the n-th source character is a closing parenthesis without an opening one".





Part 2: The tape
----------------

The tape is a long line of cells, each holding a number. The easiest type in Haskell to represent a number is a list, but remember what we want to do with the tape: traverse it in both directions, and update elements (potentially deep down) frequently, both things lists are particularly bad at: traversal in both directions is simply not possible (lists only go one way), and updating one element requires traversing the entire list up to that element, ditching the traversed spine (all the `:` we encountered on the way there), do the change, and then recreate all the `:` we just got rid of. That's O(n) for random access, which is also very awful.

But maybe we can use lists in some way, just not straightforward as `[a]`. We want our tape to have a "middle", i.e. it has an element we're currently looking at, and then we also have to put the elements we're not looking at somewhere. There's going to be a "left of the middle" and "right of the middle" part to that, and taken together that's our new type:

```haskell
data Tape a = Tape [a] -- Left of the pivot element
                    a  -- Pivot element
                   [a] -- Right of the pivot element
```

Now we've got our tape, but there's nothing we can do with it beyond making one. But wait, we'll need to do that anyway - the program starts with an empty tape, which has a pivot of zero, and infinite zeros in both directions:

```haskell
emptyTape :: Tape Int
emptyTape = Tape zeros 0 zeros
      where zeros = repeat 0
```

As a nice benefit for Haskell's laziness, you get infinite tape both to the left and right for free, and the compiler will worry about dealing with the details.

Alright, what else do we need? We want to move left and right on the tape (remember what `<>` do). The `moveRight` function does just that: it takes one element from the right list and puts it in focus, and puts the previous pivot on the left list. `moveLeft` is the same but the other way round:

```haskell
moveRight :: Tape a -> Tape a
moveRight (Tape ls p (r:rs)) = Tape (p:ls) r rs

moveLeft :: Tape a -> Tape a
moveLeft (Tape (l:ls) p rs) = Tape ls l (p:rs)
```

So that's the tape all encoded in Haskell. You probably guessed already that `moveRight` is related what `>` does, but that's part of the next section.

In the finished interpreter, we'll have to `Tape` objects: one for the data tape (the one with the numbers on it), and one for the source code (because when we encounter a `]` we have to walk backwards in the source code). While the data tape is infinite, the source tape is finite and starts out with an empty "left" side.


### Exercises

1. (easy) I mentioned you don't need `Functor` for this tutorial, but in case you want to have a go at it: write a `Functor` instance for `Tape` (don't forget to check the laws).

2. (medium) The `moveLeft` and `moveRight` functions have a problem: for some valid (i.e. well-typed) tapes they misbehave. In particular consider that, as mentioned above, the instruction tape is finite. What happens when we reach the end and focus right again? What would be ways of fixing the problem?

3. Streams

   1. (medium) Since the data tape is always infinite, a list is not absolutely the right type for it - it allows an empty list. A better representation for this would be a `Stream` type, which is identical to lists except that it has no "empty" element, i.e. all values are of infinite length. Implement that tape a `repeat` function for it analogous to `Data.List.repeat`.

   2. (hard) Modify the `Tape` type so it takes the type of the container to use as a type argument, allowing you to create a tape with `[]` or `Stream` in them. For each one, write `moveLeft` and `moveRight`. The list-based type will suffer from the issues raised in the exercise above, but what about the `Stream`-based tape rotations?





Part 3: Evaluation
------------------

Alright, the tools are finished, time to get the actual evaluation to work! Let's think about the type of the `runBrainfuck` function we'd like to write. It takes Brainfuck source which we've conveniently parsed to `BrainfuckSource` in part 1, and all it will do is read single characters (when encountering a `,` in the source) or print them (`.`), which are IO operations. Therefore, the type we're looking at is `BrainfuckSource -> IO ()`.

But `BrainfuckSource` is a list, which we've declared unsuitable for representing our data! What to do? Well, write the list onto a `Tape`:

```haskell
runBrainfuck :: BrainfuckSource -> IO ()
runBrainfuck = run emptyTape . bfSource2Tape
      where bfSource2Tape (b:bs) = Tape [] b bs
            -- (`run` is defined below)
```

We've already added the `run` function, which evaluates one instruction, and starts off with an empty tape. We'll now construct this function piece by piece. First of all, the type of `run` should be so that it takes the data `Tape` and the instruction `Tape` as arguments so it can work with them:

```haskell
-- Interpret the command currently focussed on the instruction tape
run :: Tape Int              -- Data tape
    -> Tape BrainfuckCommand -- Instruction tape
    -> IO ()
```
Now let's evaluate our first instruction, `GoRight`! What should it do to the data tape? Well, nothing besides moving the pivot:

```haskell
run dataTape source@(Tape _ GoRight _) =
      advance (moveRight dataTape) source

run dataTape source@(Tape _ GoLeft  _) =
      advance (moveLeft dataTape) source
```

That's `>` and `<` already handled: when encountered, the focus on the data tape will move one cell to the right or left, respectively. Up next, we need to move on on the source tape, because otherwise we'd be interpreting the same instruction over and over. That's what `advance` is for.

```haskell
advance :: Tape Int              -- Data tape
        -> Tape BrainfuckCommand -- Instruction tape
        -> IO ()

advance dataTape (Tape _ _ []) = return ()
advance dataTape source = run dataTape (moveRight source)
```

Note the first case, which is invoked when we run out of source code, i.e. reach the end of the program, in which case we just terminate instead of recursing on.

Now that that's covered, let's move on to the next two instructions, addition and subtraction:

```haskell
run (Tape l p r) source@(Tape _ Increment  _) =
      advance (Tape l (p+1) r) source

run (Tape l p r) source@(Tape _ Decrement  _) =
      advance (Tape l (p-1) r) source
```

Those were the two dead simple ones, now for the two IO operations. `.` should read the value of the pivot and print its corresponding character; the latter is done with `Data.Char.chr`, which you will have to import manually. For technical reasons you should also `import System.IO (hFlush, stdout)`, which we will use to get around buffering issues (if you don't understand why this is necessary: it's an IO thing to print characters as soon as we say it should, just ignore the associated lines and you'll be fine).

```haskell
run dataTape@(Tape _ p _) source@(Tape _ Print  _) = do
      putChar (chr p)
      hFlush stdout
      advance dataTape source
```

And similarly we'll implement `,`, using `chr` which is inverse to `ord` and gives us an Int associated with a `Char`:
```haskell
run dataTape@(Tape l _ r) source@(Tape _ Read  _) = do
      p <- getChar
      advance (Tape l (ord p) r) source
```

Now for the last part: the looping constructs. Those are slightly trickier because we have to keep track of how many sub-loops we've encountered so we find the right matching braces. With `seekLoopX` still undefined, we can at least write down how to react to `[` or `]` already:

```haskell
run dataTape@(Tape _ p _) source@(Tape _ LoopL  _)
      -- If the pivot is zero, jump to the
      -- corresponding LoopR instruction
      | p == 0 = seekLoopR 0 dataTape source
      -- Otherwise just ignore the `[` and continue
      | otherwise = advance dataTape source

run dataTape@(Tape _ p _) source@(Tape _ LoopR  _)
      | p /= 0 = seekLoopL 0 dataTape source
      | otherwise = advance dataTape source
```

What's left now is how to encode the `seekLoopX` functions. Conceptually, they should move along the source tape until a matching brace is found, and then just continue normal evaluation. The first parameter encodes the braces nesting level we're at to find matching braces - if we find another two opening `[` after the first one we encounter, we'll have to move over another two `]` to compensate.

```haskell
-- Move the instruction pointer left until a "[" is found.
-- The first parameter ("b" for balance) retains the current
-- bracket balance to find the matching partner. When b is 1,
-- then the found LoopR would reduce the counter to zero,
-- hence we break even and the search is successful.
seekLoopR :: Int                   -- Parenthesis balance
          -> Tape Int              -- Data tape
          -> Tape BrainfuckCommand -- Instruction tape
          -> IO ()
seekLoopR 1 dataTape source@(Tape _ LoopR _) = advance dataTape source
seekLoopR b dataTape source@(Tape _ LoopR _) =
      seekLoopR (b-1) dataTape (moveRight source)
seekLoopR b dataTape source@(Tape _ LoopL _) =
      seekLoopR (b+1) dataTape (moveRight source)
seekLoopR b dataTape source =
      seekLoopR b dataTape (moveRight source)

seekLoopL :: Int                   -- Parenthesis balance
          -> Tape Int              -- Data tape
          -> Tape BrainfuckCommand -- Instruction tape
          -> IO ()
seekLoopL 1 dataTape source@(Tape _ LoopL _) = advance dataTape source
seekLoopL b dataTape source@(Tape _ LoopL _) =
      seekLoopL (b-1) dataTape (moveLeft source)
seekLoopL b dataTape source@(Tape _ LoopR _) =
      seekLoopL (b+1) dataTape (moveLeft source)
seekLoopL b dataTape source =
      seekLoopL b dataTape (moveLeft source)
```

And finally we must not forget evaluating comments of course, but that one's trivial, because a comment just doesn't do anything:

```haskell
run dataTape source@(Tape _ (Comment _) _) = advance dataTape source
```

And there you have it, a fully featured Brainfuck interpreter! To use it, simply provide the source to `runBrainfuck . parseBrainfuck`, e.g. by specifying a source file with `readFile "filename.bf" >>= runBrainfuck . parseBrainfuck`. Try [Hello World][helloworld] from Wikipedia!



### Exercises

1. (easy) The `bfSource2Tape` function won't work when you give it the valid program represented by an empty string. What would be an easy way of fixing this? Hint: `Comment`s don't do anything and may save you from adding a `Maybe` type.

2. (easy) The call to `ord` produces an error when applied to a negative element, and you probably won't want to print character number 9001 in Brainfuck ever anyway. How could you modify the command to constrain the output to ASCII?

3. (medium) Refactor the code! To not drown the text in nifty details, the functions implemented in part 3 are very verbose. You can eliminate a lot of common cases among those functions. For example consider how a `[` if the pivot is 0 is the same as a comment, or how similar all but the first case for `seekLoopX` are.

4. There are many ways in which this interpreter could be improved.

   1. (medium) If you've done the exercises of parts 1 and 2, you can incorporate their results into the final code.

   2. (open-ended) Optimizations: You could combine multiple uses of `Increment` so that instread of adding 1 five times, you could just add a single 5; successive uses of `+` and `-` cancel out, as do `>` and `<`. And then there are higher-level optimizations as well of course, such as rewriting `[-]` to "set to zero or error if cell content is negative".

