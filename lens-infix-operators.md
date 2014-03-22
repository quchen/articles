`Lens` infix operators
======================

An overview of what the different parts of [lens-4.0][lens] infix operators
stand for.

I'll be calling anything that is used to look at data a "lens", e.g. `Lens`,
`ASetter`, `Traversal`. When a more specific term is required, I'll be using the
correct data type (which in particular starts witha capital letter).

(Once this list is properly polished, it might be worth a pull request.)

[lens]: http://hackage.haskell.org/package/lens



## General

- `@`: Provide access to the index of the data field looked at.

 *Example:* `("hello","world","!") ^@. _2` is `(1,"world")`, since "world" is
            the entry at the second (`_2`) entry of the zero-indexed tuple,
            hence the retrieved index `1`.

- `#`: Takes an `ALens` parameter instead of whatever else the normal operator
        does. (`#` alone is a synonym for `review`, which builds a data
        structure out of a lens.)



## Getting

- `^`: General "view data" operator.

- `.`: Single value.

 *Example*: `(1, "hello") ^. _2` views the second element, `"hello"`.

- `..`: Multiple values as a list.

 *Example:* `"Hello, World!" ^.. folded . filtered isLower` keeps only lower
            case letters, result: `"elloorld"`.

- `?`: Return the first value of a lookup, or `Nothing` if there is none.

 *Example*: `[1..10] ^? folded` picks the first element of the list, resulting
            in `Just 1`. `[] ^? folded` on the other hand is `Nothing`.

- `?!`: Unsafe version of `?`: crashes when there is no value.

 *Example:* `[1..10] ^?! folded` is `1`, while `[] ^?! folded` is a runtime
            error.

- `!`: Perform a monadic action with the data.

 *Example:* `["Hello","World!"] ^! folded . act putStrLn` prints the list, one
            element at a time.

- `!!`: `!` for folds, so that multiple actions can be performed in one go.

 *Example:* `[getLine, getLine] ^!! folded . acts` reads two lines, and returns
            the results together in a list.

- `!?`: Like `!!` combined with `?`: perform *all* actions, but return only the
        *first* result (safely).

 *Example:* `[getLine, getLine] ^!? folded . acts` reads two lines, and returns
            `Just <first line>`.



## Setting

- `~`: General "set value" operator.

- `=`: Same as `~`, but set an implicit `MonadState` state.

- `.`: Specify the new value directly.

 *Example:* `(_2 .~ "Mrs. Robinson") ("Hello", "World")` [greets Mrs. Robinson.]
            [hello-mrs-robinson]

- `?`: Use `Just` the specified value, i.e. `.` with an added `Just`. This
       mostly seems to be there for symmetry with the getter-`?`.

 *Example:* `(_2 ?~ "kidding") ("Hello", "World")` greets someone and takes it
            back immediately. Safe in Haskell, not so safe otherwise.

- `%`: *Mod*ify, chosen as a pun for other languages use `%` as the *mod*ulo
       operator.

 *Example:* `(traversed %@~ replicate) "Hello"` replaces each character with an
            *index*-time replication of itself, yielding
            `["","e","ll","lll","oooo"]`.

- `%%`: ?

- `<`: Also return the new value of the modified field. Useful to check on what
       was actually modified deep inside a structure.

 *Example:* `(traversed <%~ Sum . length) (words "let it be")` maps every list
            entry to its length, and also the monoidal summary of all
            modifications. Stripping the `Sum` constructors, the result is
            `(7, [3,2,2])`.

- `<<`: Like `<`, but returns the old value instead of the new one.

 *Example:* `(traversed <<%~ length) (words "let it be")` collects the
            unmodified values before applying the function, yielding
            `("letitbe", [3,2,2])`.



### Convenience definitions

The following symbols are all used as shortcuts to do something specific with a
value. `<op>~ x` generally applies `(<op> x)` to the fields pointed at, so for
example `&&~ x` applies `(&& x)`, and `<>~ x` `mappend`s `x`.

- Standard operations
      - Standard arithmetic: `+`, `-`, `*`, `//` (Divide; `/` was not taken
        because `/=` is inequality), `^`, `^^`, `**`
      - Boolean: `||`, `&&`
      - Monoidal: `<>`
- Specialized operations (located in *.Lens submodules):
      - File paths: `</>`, `<.>`
      - Bitwise arithmetic: `.|.`, `.&.`



## More examples

- `<%~` modifies (`%`) a field inside a structure, and also returns the new
  value of that field (`<`). When applied to a `Traversal`, it modifies all
  targeted values, and also returns a "monoidal summary" (e.g. a list) of all
  the modified values.

- `<<>~` is first and foremost in the library because in a language that has
  robot monkey operators `(:[])` and Kleisli fish `>=>`, a rat operator must not
  be missing.

  It also `mappend`s (`<>`) a value to a field and returns the new value along
  with the modified structure (the first `<`).



## Other operators

- `&`: Like ordinary `$`, but flipped. Useful for chaining lens operations, as
       they typically read left-to-right.

 *Example:* `(["hello", "world"], 42) & _1.mapped <>~ "!" & _2 +~ 10` first
            appends "!" to all list elements, and then adds 10 to the second
            tuple component, resulting in `(["hello!", "world!"], 52)`.

- `<|`, `|>`: Prepend (cons) and append (snoc). Be careful to specify the
              arguments in the right order or you'll be greeted with a giant
              error message.

 *Example:* `0 <| [1,2,3]` evaluates to `[0,1,2,3]`,
             `[1,2,3] |> 4` to `[1,2,3,4]`.

- `??`: Most useful when applied to the function Functor, which allows seeing
        `??` as a placeholder for the first of a two-argument function.

 *Example:* `f ?? x` is equivalent to `\y -> f y x`.

- `<.>`, `.>`, `<.`: Composition of indexed functions.

- `<~`: Execute a `MonadState` action, and store the result in the state fields
        pointed at by the lens. Named to resemble with the `<-` symbol, which
        assigns the result of a monadic action to an ordinary name.

 *Example:* `_2 <~ fmap (uncurry (++)) get }` will execute the action on the
            right hand side; the result is the concatenation of the two tuple
            elements of the state. This is then set as the second field of the
            state.




[hello-mrs-robinson]: http://youtu.be/bE1dz6_u2JI
