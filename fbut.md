Frequently brought up topics in #haskell
========================================



`ByteString.Char8` is bad
-------------------------

### Short version

"The short answer is 'any time you write `Data.ByteString.Char8` in your code, your code is probably wrong'" - *merijn on #haskell*

### The problem

`ByteString` is a library for representing raw data, suitable for storing it or sending it over the network. Raw data is seldomly what you actually want; it is an intermediate form for the actual data you're trying to pack into single bytes. The one crucial assumption about encoding and decoding data is that doing them one after another gets you the original data back, i.e.

```haskell
decode . encode = id
```

This is where `ByteString.Char8` fails: when converting a `[Char]` to a `ByteString.Char8`, all the `Char` are truncated silently to their first byte. Truncation means loss of information, therefore

```haskell
unpack . pack /= id
```

Unfortunately, this is merely a remark and not a 36pt bold underlined warning in the documentation.

### Why is this bad?

You might say "I'm only using ASCII data anyway, and the issue above only exists for multi-byte Unicode". This is a very dangerous argument, because you never know in what context your code might be used by an unsuspecting third party (and you should consider yourself in 3 months one of those).

To give you an analogy, suppose you have a number library for manipulating `Int`, but it turns out that `(*)` is not commutative for numbers larger than `100`. Would it be wise to say "I'll just use it for small numbers so it'll be fine"? Is it OK to not catch exceptions that "should never happen"? Do you not have airbags in your car because when you drive you're always very careful?

### The right way

Use `ByteString`, not `ByteString.Char8`. If what you want is a conversion `String -> ByteString`, then use a serialization library such as [Binary][binary] that takes care of the conversion from `[Char]` to `[Word8]` to `ByteString` and back. The same goes for `Text`, which can be serialized using `ByteString.Encoding`.

`ByteString.Char8` has very few limited uses, for example if you're communicating over a HTTP connections the commands are all ASCII, so using `Char8` saves you from converting between `ByteString` and `String` explicitly all the time. (Note that HTTP requests can still contain Unicode in their bodies, so HTTP isn't a ticket to using `Char8` in general.)

[binary]: http://hackage.haskell.org/package/binary




``(a `op`)`` is not ``\x -> a `op` x``
--------------------------------------

These two forms are seemingly identical, but there is a subtle difference. The first one is just sugar for `op a`, while the second one is a lambda (and not direct application of `op`). This leads to different strictness properties in the presence of ⊥:

```haskell
> let op = undefined

-- Section
> (() `op`) `seq` ()
>>> *** Exception: Prelude.undefined

-- Prefix
> (op ()) `seq` ()
>>> *** Exception: Prelude.undefined

-- Lambda
> (\x -> () `op` x) `seq` ()
>>> ()
```

The reason for this behaviour is that a lambda can be thought of as an additional wrapper around the application of `op`, and this wrapper is already WHNF. For this reason, `op` is never forced, and the `seq` terminates without complaints.



"I don't understand Monads"
---------------------------

There are many articles about Monads and how to use them out there, and as many articles pointing that out ("Monad tutorial fallacy"), and probably even more meta-levels. Here is my practical advice for demystifying Monads.

A Haskell Monad is a typeclass, and typeclasses unify types that act alike in some sense. For typeclasses like `Eq` that "alike-ness" is fairly obvious, but for Monads it isn't. So what should you do? Use Monad instances, and don't worry about the Monad part. How does `do` notation work for `Maybe`? What does `>>=` do in this scenario? Now write some small example code that just *uses* these features. Up next: how does `do` notation work for `Writer`? What does `>>=` do in this scenario? Now write some small example code that just *uses* these features. Up next: how does `do` notation work for `State`? What does `>>=` do in this scenario? Now write some small example code that just *uses* these features. You can see where this is going, but for the sake of it, here's the essence of it: **Learn Monad instances separately without worrying about how they are "monadic". After some time you will develop an intuition for what `do`, `<-`, `return`, `>>=` etc. have in common.** And you guessed it, the Monad typeclass unifies that common-ness. And that's how I think you should approach Monads.

Here is the order in which I recommend looking at how to use standard Monad instances:

1. `Maybe`; you can also have a look at `Either` which is pretty similar.
2. `State`, `Writer`.
3. `Reader`, `IO`. `IO` in particular is a good Monad to get an *intuitive* feeling for, as `IO` is primitive (i.e. hardcoded) and therefore the "I don't understand the implementation so I can't understand this one" is not an excuse.



Tabs vs. spaces
---------------

Short version: use spaces.

Long version: You can write valid Haskell with spaces, tabs, or anything in between. The between part is a pain in the ass, but you can make only-tab-indented code work. So why shouldn't do that?

1. If you were referred to this document, you're most likely a beginner. Beginners make *lots* of mistakes with tabs and are surprised why their code breaks and annoy others with this on IRC. Don't be one of those guys.

2. Spaces look the same to everyone, tabs don't. Chances are your tabbed code looks like crap with different settings; if it doesn't you have lots of newlines at the appropriate locations so it looks like crap in the first place.

3. The Haskell community has decided to use spaces. All common libraries use spaces, and the only reason there are a few tabs left in GHC's source is because of potential merge conflicts when editing them all out in bulk.

If you have any further questions, feel free to contact me at `tabs-vs-spaces@example.com`.


`(- 4)` is not `\x -> x - 4`
----------------------------

A single negative sign is a special case in Haskell syntax, and simplifies entering negative numbers. Unfortunately this rule breaks sections with `(-)`.

```haskell
-- The number "-4".
a :: Num a => a
a = (-4)
-- or simply "a = -4"

-- The function "subtract 4 from the argument".
b :: Num a => a -> a
b = \x -> x - 4

-- A useful Prelude definition for the "subtract from" function.
subtract :: Num a => a -> a -> a
subtract a b = b - a

-- The "subtract 4" function written with the above.
c :: Num a => a -> a
c = subtract 4
```

So in summary, if you want a section with the `(-)` operator, use `subtract` like in example `c`. A final word of caution, using `subtract` in infix looks reasonable, but produces wrong (negative) results due to reversed arguments - ``3 `subtract` 1`` is `-2`.



I'm looking for a good Regex library
------------------------------------

No. Stop. What you want to do is parse something, right? Use a parser! Regex is widely used in other languages a lot, but *very* unpopular in Haskell:

- Regex is very hard to read once written.
- Writing Regex is error-prone, and the lack of readability makes it hard to debug them.
- Even if correct, the lack of readability makes them unmaintainable.
- Regex can split and transform text, but you'll always get out text again. This makes Regex more like a lexer, not a parser. You'l still have to convert your Regex chunks into actual data afterwards.

Want an example? Here's a regex to check US phone numbers in Python ([source][regex-source]):

```python
r'^(1[-\s.])?(\()?\d{3}(?(2)\))[-\s.?\d{3}[-\s.]?\d{4}$'
```

Actually no, there's a something missing. First task, find it. Afterwards, you may object that the code lacks documentation, so of course it's unreadable. Split in multiple lines, use comments:

```python
r'^'
r'(1[-\s.])?' # optional '1-', '1.' or '1'
r'(\()?'      # optional opening parenthesis
r'\d{3}'      # the area code
r'(?(2)\))'   # if there was opening parenthesis, close it
r'[-\s.]?'    # followed by '-' or '.' or space
r'\d{3}'      # first 3 digits
r'[-\s.]?'    # followed by '-' or '.' or space
r'\d{4}$'     # last 4 digits
```

What's the back reference to getting the area code again? The answer is don't use Regex. If you want to do crap, Regex is the right tool for the job. If you want to parse use parsers, for example [Parsec][parsec].



[regex-source]: http://pypix.com/tools-and-tips/advanced-regular-expression-tips-techniques/
[parsec]: http://hackage.haskell.org/package/parsec



I don't like this `Show` instance
---------------------------------

Most likely this means you're expecting the wrong thing from `Show`. `Show` is not for prettyprinting, it's for converting things to `String`, often in a way where the resulting `String` is valid Haskell and could be re-inserted into code. Because of this, **`Show` is first and foremost a debugging class**. For prettyprinting, there are other libraries, such as [`pretty`][pretty] or [`pretty-show`][pretty-show].

[pretty]:      http://hackage.haskell.org/package/pretty
[pretty-show]: http://hackage.haskell.org/package/pretty-show