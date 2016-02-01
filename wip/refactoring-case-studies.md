The following code is from an IRC conversation in early January 2016. Code of
this form is quite common, but fixing its issues is fairly quick and easy.
The impatient should scroll to the very bottom. :-)

```haskell
data Config = Config {
    nick :: String,
    user :: String,
    channels :: [String],
    server :: String,
    port :: GHC.Int.Int64
} deriving (Show)

nodesToStr :: X -> Maybe [String]

extractNode :: T.Text -> Node -> Either String Config -> Either String Config
extractNode _ _ l@(Left _) = l
extractNode k (NTValue (VString x)) (Right (Config n u c s p))
    | key == "nick" = Right $ Config val u c s p
    | key == "user" = Right $ Config n val c s p
    | key == "server" = Right $ Config n u c val p
    | otherwise = Left $ "Could not find " ++ T.unpack k
    where val = T.unpack x
        key = T.unpack k
extractNode k (NTValue (VInteger val)) (Right (Config n u c s p))
    | key == "port" = Right $ Config n u c s val
    | otherwise = Left $ "Could not find " ++ T.unpack k
    where key = T.unpack k
extractNode k (NTValue (VArray arr)) (Right (Config n u c s p))
    | key == "channels" = let a = nodesToStr arr in
                       if isNothing a then Left "Array did not only contain string" else Right $ Config n u (fromJust a) s p
    | otherwise = Left $ "Could not find " ++ T.unpack k
  where key = T.unpack k
```

Apart from the density of the code, here are the main problems:

- The `extractNode` definitions match way too many patterns, so we cannot
  see what the most relevant argument is.
- The `where` blocks and the `otherwise` branches contain duplicated code.
- The code isn't robust against changes in the `Config` type, for example
  switching the order of the `nick` and `user` fields would introduce
  a bug.
- The code combines `isNothing` and `fromJust`, does not leverage pattern
  matching.
- Too many `$` signs, bad for readability.
- No code alignment, bad for readability.
- Usages of and conversion from/to `String`, bad performance, visual
  conversion clutter.

And here is how we can fix them:

1. Instead of matching

   ```haskell
   k (NTValue (VString x)) (Right (Config n u c s p))`
   -- and then
   k (NTValue (VInteger x)) (Right (Config n u c s p))
   -- and then
   k (NTValue (VArray arr)) (Right (Config n u c s p))
   ```
   we do a single match on `k (NTValue ntValue) (Right (Config n u c s p))`
   and then another explicit `case`-match on the `ntValue`. This allows
   sharing all the other bindings (`k`, the `Right`, the `Config n u c s p`)
   over the `case`. As a side-effect, this also merges all the `where`
   blocks. Here's the result:

   ```haskell
   extractNode k (NTValue ntValue) (Right (Config n u c s p)) = case ntValue of
       VString x
           | key == "nick" -> Right $ Config val u c s p
           | key == "user" -> Right $ Config n val c s p
           | key == "server" -> Right $ Config n u c val p
           | otherwise -> Left $ "Could not find " ++ T.unpack k
       VInteger val
           | key == "port" -> Right $ Config n u c s val
           | otherwise -> Left $ "Could not find " ++ T.unpack k
       VArray arr
           | key == "channels" -> let a = nodesToStr arr in
                              if isNothing a then Left "Array did not only contain string" else Right $ Config n u (fromJust a) s p
           | otherwise -> Left $ "Could not find " ++ T.unpack k
     where
       val = T.unpack x
       key = T.unpack k
   ```

2. Time for some cosmetics: replace all `$` by parentheses, float up the
   `otherwise` clauses, use `key` instead of `T.unpack k`, align the `->`.

   ```haskell
   extractNode k (NTValue ntValue) (Right (Config n u c s p)) = case ntValue of
       VString x
           | key == "nick"   -> Right (Config val u c s p)
           | key == "user"   -> Right (Config n val c s p)
           | key == "server" -> Right (Config n u c val p)
       VInteger val
           | key == "port" -> Right (Config n u c s val)
       VArray arr
           | key == "channels" -> let a = nodesToStr arr in
                              if isNothing a then Left "Array did not only contain string" else Right (Config n u (fromJust a) s p)
       _otherwise -> Left ("Could not find " ++ key)
     where
       val = T.unpack x
       key = T.unpack k
   ```

3. Use pattern matching instead of `isNothing` and `fromJust`, and record
   syntax instead of the `Config n u c s p` business.

   ```haskell
   extractNode k (NTValue ntVal) (Right config) = case ntVal of
       VString val
           | key == "nick"   -> Right config { nick   = val }
           | key == "user"   -> Right config { user   = val }
           | key == "server" -> Right config { server = val }
       VInteger val
           | key == "port" -> Right config { port = val }
       VArray arr
           | key == "channels" = case nodesToStr arr of
               Nothing    -> Left "Array did not only contain string"
               Just chans -> Right config { channels = chans }
       _otherwise -> Left ("Could not find " ++ key)
     where
       val = T.unpack x
       key = T.unpack k
   ```

4. Replace all `String` with `Text`. This gets use of the `where` block
   entirely, at the cost of using the `-XOverloadedStrings` extension.

   ```haskell
   extractNode k (NTValue ntVal) (Right config) = case ntVal of
       VString val
           | key == "nick"   -> Right config { nick   = val }
           | key == "user"   -> Right config { user   = val }
           | key == "server" -> Right config { server = val }
       VInteger val
           | key == "port" -> Right config { port = val }
       VArray arr
           | key == "channels" = case nodesToStr arr of
               Nothing    -> Left "Array did not only contain string"
               Just chans -> Right config { channels = chans }
       _otherwise -> Left ("Could not find " <> key)
   ```

The final result then looks like this:

```haskell
{-# LANGUAGE XOverloadedStrings #-}

data Config = Config {
    nick     :: Text,
    user     :: Text,
    channels :: [Text],
    server   :: Text,
    port     :: GHC.Int.Int64
} deriving (Show)

nodesToStr :: X -> Maybe [String]

extractNode ::
       T.Text
    -> Node
    -> Either Text Config
    -> Either Text Config
extractNode _ _ l@(Left _) = l
extractNode k (NTValue ntVal) (Right config) = case ntVal of
    VString val
        | key == "nick"   -> Right config { nick   = val }
        | key == "user"   -> Right config { user   = val }
        | key == "server" -> Right config { server = val }
    VInteger val
        | key == "port" -> Right config { port = val }
    VArray arr
        | key == "channels" = case nodesToStr arr of
            Nothing    -> Left "Array did not only contain string"
            Just chans -> Right config { channels = chans }
    _otherwise -> Left ("Could not find " <> key)
```
