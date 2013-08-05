Frequently brought up topics in #haskell
========================================



`ByteString.Char8` is bad
-------------------------

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

Use `ByteString`, not `ByteString.Char8`. If what you want is a conversion `String -> ByteString`, then use a serialization library such as [Binary][binary] that takes care of the conversion from `[Char]` to `[Word8]` to `ByteString` and back.

`ByteString.Char8` has very few limited uses if what you want is to interface some other dirty programs etc; I think it's safe to say that if you don't absolutely know that you *do* need `ByteString.Char8`, it is not a good idea to use it.

[binary]: http://hackage.haskell.org/package/binary



