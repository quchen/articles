# How to write unmaintainable Haskell code

A decent amount of the classic [*How to write unmaintainable code*](http://thc.org/root/phun/unmaintain.html) is biased towards imperative programming. This is an attempt at reaching equality.

1. Use rewrite rule pragmas to alter functions. The compiler doesn't check whether the rewrite rule makes sense at all, so feel free to sprinkle your code with **meaningful** rewrites.
```haskell
    {-# RULES "reverse map/append"
        forall f xs ys. map f (xs ++ ys) = map f ys ++ map f xs #-}
```

2. The IO monad is your friend! Take the restrictivity of Haskell away, when you're done use QuickCheck to show  it's referentially transparent and wrap it in unsafePerformIO.
```haskell
    len xs = unsafePerformIO $ do
        count <- newIORef 0
        let getLength ys = case ys of
            []      -> return ()
            (y:ys') -> modifyIORef' count (+1) >> getLength ys'
        getLength xs
        readIORef count
```
(Make sure to use `modifyIORef'` or your code may be unsafe.)

3. `Writer` < `State`

4. `State` < `IORef`

5. `IORef` < `RWST IO`

6. A suitable type to write something is `Writer`. A suitable type to modify state is `State`. `IORefs` are good in general. I'm sure you already guessed how to combine this with the previous points.

7. QuickCheck is a statistical test that generates false sense of security, and should be avoided for that reason.

8. All laws in Haskell are meant to be broken. Got a monad, but `m >>= return` just won't be `m` again? A `-- hack, careful` should be sufficient documentation.

9. Haskell's expressiveness allows coding in a very concise style. For example it renders comments redundant and makes you able to call all your types `T` and typeclasses `C` and them import them qualified.

10. Never import a module qualified under the same name twice. Combines well with the previous point.

11. Import the same module multiple times, qualified under different (suggestive) names.

12. Boilerplate code should be avoided; GHC will complain when it requires explicit type annotations.

13. If you want your program to be run just as you've written it, disable compiler optimization rewrites. The nicer way of doing this is by using the `-fno-enable-rewrite-rules` flag when compiling, however it is more effective to define a rule that makes GHC go into an infinite loop when compiling, forcing the compilation to be done like this.
```haskell
    loop a b = ()
    {-# RULES "loop" forall x y. loop x y = loop y x #-}
```
Note that you have to use loop somewhere so it's not optimized away. A good way is having `return $ loop 1 2` as the last function in main.

14. Naming conventions can help make code more readable. For example in `(xs:x)`, `xs` stands for "x singular", and `x` contains the rest.

15. Use built-in functions as identifiers. Make sure to mention the name in the docs multiple times. Then create a base case that doesn't work for that operator.
```haskell
    times 0  _  _ = 1
    times n (+) x = x + times (n-1) (+) x
    -- 2*3 = ?
```

16. Redefine Prelude elements. The following one will teach proper use of `succ` in the future.
```haskell
    a + b = a +. b'
        where (+.) = (Prelude.+)
              b' | b == 1    = b +. 1
                 | otherwise = b
```
... unless ...
```haskell
    succ x = Prelude.succ . Prelude.succ $ x
    -- Eta reduction needs -XMonomorphismRestriction :-(
```
Another thing worth noting is that `otherwise` is not a language feature, but simply defined to be `True` in `Data.Bool`.
```haskell
    otherwise = False
```

17. Make all functions pointfree. If you can't do it yourself, ask Lambdabot.

18. Unicode is your friend!
Make mathematicians cry:
```haskell
type ℕ = Integer
type ℝ = Double
```
Prettyprint special values:
```haskell
(∞) = 1/0
```
Use creative symbolism:
```text
(☃) = show
(☠) = undefined
(≸) = (==)
a ‽ b = a `seq` (a, b)
```
And last the really golden part about Unicode: characters that look alike, for example none of the following is in ASCII: аеорсух АВЕКМНОРСТУХ.
```text
let map mар maр = mар : map mар maр; mаp mар = map mар mар in mаp "map"
```

19. `LANGUAGE` pragmas alter the language, so they are not and should not be part of the source code. If you need language extensions, specify them as compiler flags in your `.cabal` or `makefile`.

20. Use `BangPatterns` syntax without importing the language extension.
```haskell
    f !x = f x
```

21. The `RebindableSyntax` GHC extension makes certain operators not refer to their Prelude versions, but to whatever is in scope. This means you can redefine functions like `ifThenElse`, `(>>=)` and `(>>)`. The last two are especially devious as do-blocks are still desugared using the same rules, but the meaning of operators can be completely different - even non-monadic definitions are possible. As a bonus, you can do the re-definition *inside* a do block, affecting only the following code.
