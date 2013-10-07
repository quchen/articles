Useful Haskell techniques
=========================

(A collection of little techniques I find useful or interesting)








Poor man's supercompiler
------------------------

Suppose your program depends on a constant that takes long to calculate. You could of course do the calculation in the beginning each time the program is run, but that takes up time without a good reason; you could hard-code the data into your source code, but that means you lose the flexibility of changing the algorithm.

However, there is a middle ground: hardcode the value, but let Haskell do it automatically during compilation. This is a form of supercompilation: parts of the program are evaluated before the actual compilation happens.

The following is a brief description of how to achieve this with [Template Haskell (TH)][TH]. It assumes you've got some very basic TH knowledge; if not, reading a couple of tutorials will bring you on track in less than an hour.

[TH]: http://www.haskell.org/haskellwiki/Template_Haskell



### The recipe

The following is the recipe to follow; afterwards, two examples are given on how to apply them.

1. Pack the value to calculate into a single expression. This requires no work or preparation, as you've already got the value you want to calculate, you just want to do it during compilation now.

2. Wrap the expression in the appropriate TH data constructors. (The `Lift` typeclass, defined in `Language.Haskell.TH.Syntax`, is very helpful here.)

3. Splice the generated object in your actual code: just insert `$( wrapped )` in your main code. Note that the TH code has to be in another module than where you plan on inserting it for technical reasons.



### Example #1: primitive type

Let's call the TH module `TH`,

```haskell
-- File: TH.hs
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
module TH where
```


What's the best example in all of Haskell? Fibonacci! Step one: write the function you'd like to supercompile.

```haskell
-- Calculates the n-th Fibonacci number's last 10 digits in O(n).
fibo :: Integer -> Integer
fibo = (`rem` 10^10) . fibo' (0, 1)
      where fibo' (a,_) 0 = a
            fibo' (!a,!b) n = fibo' (b, a + b) (n - 1)
```

Step two: wrap it in a TH expression.

```haskell
-- Wraps an Integer in(to) a Q Exp
wrapTH :: Integer -> Q Exp
wrapTH n = [| n |]

-- | Wrap a certain Fibonacci number in an expression
fiboTH :: Integer -> Q Exp
fiboTH n = wrapTH (fibo n)
```

Step three: insert it in your main code.

```haskell
-- File: Main.hs

{-# LANGUAGE TemplateHaskell #-}

import TH

myFibo :: Maybe Integer
myFibo = $( fiboTH (10^6) )

main = print myFibo
```

You'll notice this compiles slowly, but runs very fast - just as desired.



### Example #2: custom type

Let's say you want to distinguish even and odd numbers on type level in your result. How would the previous example have to be modified?

```haskell
data EvenOdd a = Even a | Odd a deriving (Show)

evenOdd :: Integral a => a -> EvenOdd a
evenOdd x | even x    = Even x
          | otherwise = Odd  x

fibo :: Integer -> EvenOdd Integer
fibo = evenOdd . fibo' (0, 1)
      where fibo' (a,_) 0 = a
            fibo' (a,!b) n = fibo' (b, a + b) (n - 1)

wrapTH :: EvenOdd Integer -> Q Exp
wrapTH eo = [| eo |]

fiboTH :: Integer -> Q Exp
fiboTH n = wrapTH (fibo n)
```

Compile aaand ... error. TH doesn't know how to generate the code for an `EvenOdd` in `wrapTH`, because the `Lift` instance is missing. `Lift` defines what `wrapTH` is used for as a general version: how to make a `Q Exp` out of something, via `lift :: a -> Q Exp`. We didn't have any problem in the first example, as `Integer` is already an instance, but now we have to define it ourself. Luckily, it's not harder than writing `wrapTH`:

```haskell
-- Needs Language.Haskell.TH.Syntax
instance (Lift a) => Lift (EvenOdd a) where
      lift (Even x) = [| Even x |]
      lift (Odd  x) = [| Odd  x |]
```

Note that you can't just use `lift x = [| x |]`, as that would result in infinite recursion, although it conceptually looks the same as the above code - you have to spell things out here.

Now you're done, that pretty much covers this section. Compile, wait, run it! A final hint for using TH: compiling with `-ddump-splices` in GHC will print what every splice evaluates to during compilation, which is very handy for debugging (e.g. to find out whether everything is really calculated as wanted during compile time). Finally, here's the complete source of the example above:

```haskell
-- Main.hs

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

import TH

myFibo = $( fiboTH (10^5) )

main = print myFibo
```

```haskell
-- TH.hs

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

module TH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data EvenOdd a = Even a | Odd a deriving (Show)

evenOdd x | even x    = Even x
          | otherwise = Odd  x

instance (Lift a) => Lift (EvenOdd a) where
      lift (Even x) = [| Even x |]
      lift (Odd  x) = [| Odd  x |]

fibo :: Integer -> EvenOdd Integer
fibo = evenOdd . (`rem` 10^10) . fibo' (0, 1)
      where fibo' (a,_) 0 = a
            fibo' (a,!b) n = fibo' (b, a + b) (n - 1)

fiboTH :: Integer -> Q Exp
fiboTH n = lift (fibo n)
```














































Bouncy folds
------------

When you want to walk through a list element-per-element, the function to use is often `foldr`. However, sometimes that just won't cut it: sometimes, you want to have more information per folding step than just the accumulator and the current element. So let's look at a foldr in action, and observe what happens:

```haskell
  foldr f z (x:y:xs)       -- The outermost function is a fold.
= f x (foldr f z (y:xs))   -- This isn't a fold anymore, but an f!
```

This illustrates what I mean with "bouncy": evaluating `foldr` doesn't actually fold all over the list, but gives control to the folding function in the next step, which in turn results in a fold again, "bouncing" back and forth between the fold and the folding function. What I call a "bouncy fold" is deliberately using this effect to add a form of meta-state or -information to the fold. For example, `find` is easily implemented using `foldr`,

```haskell
find :: (a -> Bool) -> [a] -> Maybe a
find p = foldr go Nothing
      where go x acc | p x       = Just x
                     | otherwise = acc
```

How would you modify this code so that it finds not only the first, but the second occurrence of a certain element? How would you store whether you've previously found something already? One possible answer is a bouncy fold, which adds another parameter to the equation:

```haskell
find2nd :: (a -> Bool) -> [a] -> Maybe a
find2nd p xs = foldr go (const Nothing) xs False
      where go x acc foundOne | p x && foundOne = Just x
                              | p x             = acc True
                              | otherwise       = acc foundOne
```

This may be a little hard to understand just by looking at it, so let's see how it is evaluated. Suppose we wanted to to find the second even number in `[1..]`:

```haskell
  find2nd even (1:2:3:4:xs)
= foldr go (const Nothing) (1:2:3:4:xs) False      -- By definition of find2nd
= go 1 (foldr go (const Nothing) (2:3:4:xs)) False -- By definition of foldr
= foldr go (const Nothing) (2:3:4:xs) False        -- By definition of go:
                                                   --   1 is not even
= go 2 (foldr go (const Nothing) (3:4:xs)) False   -- By definition of foldr
= foldr go (const Nothing) (3:4:xs) True           -- By definition of go:
                                                   --   2 is even, change the
                                                   --   False to True to store
                                                   --   that a value has been
                                                   --   found
= go 3 (foldr go (const Nothing) (4:xs)) True      -- By definition of foldr
= foldr go (const Nothing) (4:xs)                  -- By definition of go:
                                                   --   3 is not even
= go 4 (foldr go (const Nothing) xs) True          -- By definition of foldr
= Just 4                                           -- By definition of go:
                                                   --   "foundOne" flag was
                                                   --   previously set to True,
                                                   --   4 is even, therefore
                                                   --   return 'Just 4'.
```

Notice how the evaluation bounces between `foldr` applications and `go` applications. Each of them minds its own business in its step, but afterwards gives control to the other one. A large amount of non-folds can be made folds using this technique, the most famous example is probably implementing `foldl` in terms of `foldr` - you may have seen it as a (difficult!) exercise in RWH. The problem is that using a normal `foldr`, you can't construct the order of function applications that `foldl` requires. What to do? Well, instead of using `foldr`'s accumulator, just add a new accumulator outside of the `foldr`, similarly to the `True/False` parameter `foundOne` in the previous example!

```haskell
myFoldl f z xs = (foldr go id xs) z -- Parentheses for clarity
      where go x acc outerAcc = acc (f outerAcc x)
```

`foldr` gives control to `go`, which completely ignores the `foldr` and puts whatever it likes in the `outerAcc` - and what's put in there looks just like what `foldl` does to the accumulator. The procedure then bounces back to the `foldr`, takes off another element, and puts it onto the `outerAcc` according to `foldl` rules, until the whole list has been traversed, which looks like

```haskell
  (foldr go id []) z
= id z
= z
```

The last step throws the foldr away, leaving only the outer accumulator.

This technique is quite general, and once you get the hang of it it's sometimes hard not to use it, although a normal `fold` would do. When you use a bouncy fold, you should spend some time to make sure that you actually need one, because it's most likely less readable even in the best case. (I'm not sure how good GHC is at optimizing this, but surely it's not better than for normal folds.)

Here are a few practice problems you can try solving using bouncy folds, (2 and 3 are inspired by the demo tests of [Codility](http://codility.com/)). Note that the solution takes the form `foldr ...` - no outer wrappers are allowed, such as `doStuff $ fold ...`!

1. (Easy) Collect every second element of a list. Example: `every2nd [1..5]  ==>  [1,3,5]`

2. (Hard) Find the index of the list element so that the sum of all the elements before and after it are equal. Example: `equi [1,2,3,4,6]  ==>  Just 3`, as the third element divides the list in `[1,2,3]` which sums to the same as `[6]`.

3. (Harder) Find the index of the list element after which all further elements have already occurred before (and including) that element. Example: `covering [1,2,3,4,3,5,2,1,1]  ==>  5`, because `[1,2,3,4,3,5]` contains all unique elements of the list. (Equivalently, `sort . covering == sort . nub`.)

Solutions for these are given in the last section in this file.









































Avoiding `length` in list functions
-----------------------------------

Suppose you want to find out whether two lists have the same length. The straightforward method of doing this is of course

```haskell
sameLength :: [a] -> [b] -> Bool
sameLength xs ys = length xs == length ys
```

However, this does a lot of redundant operations - namely add up a lot of `1`s.

There is another counting method, similar to what some humans used when their number systems didn't have enough large numbers (say only 1-3), but they had 20 sheep. In the morning, they let their sheep out, and for every one of them they put one pebble in a bowl. When they gathered the sheep back in the evening, they would remove one pebble per sheep, and in the end they can easily check whether the counts match, allowing them to count their sheep while never requiring them to know their actual number of sheep. This principle translated to Haskell reads

```haskell
sameLength' :: [a] -> [b] -> Bool
sameLength' []     []     = True
sameLength' (_:xs) []     = False
sameLength' []     (_:ys) = False
sameLength' (_:xs) (_:ys) = sameLength' xs ys
```

Another example where `length` could be used is in a function that drops the last `n` elements from a list, naively implemented as

```haskell
dropLast :: Int -> [a] -> [a]
dropLast n xs = take (length xs - n) xs
```

This has problems beyond just being inefficient: the program will not terminate if you feed it an infinite list, although it's a perfectly fine assumption that dropping `n` elements off that one is just the same list again. Here's the trick to solve the problem, `zipWith const`:

```haskell
dropLast' :: Int -> [a] -> [a]
dropLast' n xs = zipWith const xs (drop n xs)
```

We know that the resulting list will have `n` elements less than xs. So let's just take the elements from `xs` until it has the same length as `xs` with `n` elements dropped.

(Another implementation would've been `dropLast'' n = reverse . take n . reverse`, but the double reverse of course has terrible runtime behavior, and doesn't work on infinite lists as well.)

A last example of efficient omission of `length` is a very smart version of a function that splits a list in two halves of equal (+- 1) length by [dmwit](http://dmwit.com/). The naive implementation is

```haskell
split2 xs = splitAt (length xs `div` 2) xs
```

Again, this doesn't work on infinite lists, and has to calculate a long chain of `1+` in the finite case. But we already know what the final result should look like: it's a pair of two lists, namely the first and the second half. How do we get the first half? Take only half the elements of course! The second half is then obtained by dropping instead of taking. Here's the solution, using a trick similar to what we've used in `dropLast` before:

```haskell
-- half picks out every 2nd element of a list, starting with the first one.
-- The resulting list is of course half as long as the original one.
-- Example: half [1..10] = [1,3,5,7,9]
--          half [2..10] = [2,4,6,8]
-- Note that it is biased towards short left lists, as seen in the second
-- example omitting '10'; this behavior mimics the fact that 'div' is integer
-- division (hence floors on uneven numbers). To add ceiling (making the split
-- left biased), add the clause 'half [x] = [x]'.
half :: [a] -> [a]
half (x:_:xs) = x : half xs
half _        = []

-- end xs ys drops one element from xs for every element of ys. The result
-- is equivalent to @drop (length ys) xs@ for finite lists, but `end` also
-- works on infinite lists.
end :: [a] -> [b] -> [a]
end []     _      = []
end xs     []     = xs
end (_:xs) (_:ys) = end xs ys

-- Splits a lift in halfs of equal (+-1) length.
split2' :: [a] -> ([a], [a])
split2' xs = (firstHalf, secondHalf)
      where h = half xs
            firstHalf  = zipWith const xs h
            secondHalf = end xs h
```

(For an infinte list, this results in a pair of lists, the second of which is bottom, and the first one is the list itself.)

So the take-away message of this section is that when your goal is to drop/take elements from a list depending on some other list, consider using `zipWith` instead of auxiliary functions like `length`.

Here's an exercise problem (solution see last section): write a *rotate left* function that takes the first `n` elements of a list and moves them to its end. Example:

```haskell
rotateL  0 [1..10] = [1,2,3,4,5,6,7,8,9,10]
rotateL  3 [1..10] = [4,5,6,7,8,9,10,1,2,3]
rotateL 13 [1..10] = [4,5,6,7,8,9,10,1,2,3]
```













Exercise solutions
------------------

### Bouncy folds

1. Picking every second element of a list - the idea here is that the external value switches between `True` and `False` every element, and only if it's `True` that element is added to the result.

    ```haskell
    every2nd :: [a] -> [a]
    every2nd xs = foldr go (const []) xs True
          where go x acc flag | flag      = x : acc (not flag)
                              | otherwise = acc (not flag)
    ```

2. Equilibrium index - the external value consists of the triple `(sum left, sum right, current index)`. Each step checks whether the left sum is equal to the right sum, starting with `(0, sum xs, 0)`.

    ```haskell
    equi :: (Eq a, Num a) => [a] -> Maybe Int
    equi xs = foldr go (const Nothing) xs (0, sum xs, 0)
         where go x acc (sumL, sumR, !i) | sumL == sumR-x = Just i
                                         | otherwise = acc (sumL+x, sumR-x, i+1)
    ```

3. Covering index - the external value holds three parameters: a `Set` of already occurred unique elements, the running index (i.e. the current position in the list), and the covering index candidate (the position where the last new item was found). If a new item is found it is added to the set and the candidate index is updated to the current running index.

    ```haskell
    -- Could be done with lists only, which
    -- would need only Eq, but also be far less performant.
    covering :: (Ord a) => [a] -> Int
    covering xs = foldr go third xs (S.empty, 0, 0)
          where third ~(_,_,x) = x
                go x fold (u, !ri, ci)
                      | S.notMember x u = fold (S.insert x u, ri+1, ri)
                      | otherwise       = fold (           u, ri+1, ci)
    ```


### Avoiding `length`

The idea here is cycling the list, dropping the first `n` elements, and then taking as many elements from the result as the original list was long. The naive way would of be `take (length xs)`, the more elegant solution is

```haskell
rotateL n xs | n >= 0 = takeN . drop n $ cycle xs
      where takeN ys = zipWith const ys xs
rotateL _ _ = error "Negative offset"
```

