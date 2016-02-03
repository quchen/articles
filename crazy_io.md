Haskell crazy IO quiz
=====================

This is a collection of weird bugs in Haskell programs due to lazy IO. The
answer to all these snippets is "don't use lazy IO". The quiz part is finding
out what goes wrong.

Provided by Peaker in Freenode/#haskell:

```haskell
-- May throw an exception
do let action = fmap (== content) (readFile filename)
       handler (SomeException {}) = pure False
   isEqual <- action `catch` handler
   unless isEqual (writeFile filename content)
```

```haskell
-- Does not time out after 3 seconds
timeout (seconds 3) (readFile "foo")
```

```haskell
-- May print nothing or truncated result
do x <- withFile "foo" ReadMode (\h -> ... hGetContents h ...)
   print x
```

```haskell
-- May throw an exception
-- BS = ByteString
do x <- BS.withFile "foo" ReadMode (\h -> ... BS.hGetContents h ...)
   print x
```