`Lens` infix operator meanings
==============================

I'll be calling anything that is used to look at data a "lens", e.g. `Lens`,
`ASetter`, `Traversal`. When a more specific term is required, I'll be using the
correct data type (which in particular starts witha capital letter).



## General

- `@`:  Provide access to the index of the data field looked at.
- `#`:  Takes an `ALens` parameter instead of whatever else the normal operator
        does.



## Getting

- `^`:  View data.
- `.`:  Single value.
- `..`: Multiple values as a list.
- `?`:  Return the first value of a lookup, or `Nothing` if there is none.
- `?!`: Unsafe version of `?`: crashes when there is no value.
- `!`:  Perform a monadic action with the data.
- `!!`: ?
- `!?`: ?



## Setting

- `~`:  Set value. Used to distinguish between ordinary values and `MonadState`
        states.
- `=`:  Same as `~`, but targets the state of a `MonadState` instead of a value
        directly.
- `.`:  Specify the new value directly
- `?`:  Use `Just` the specified value, i.e. `.` with an added `Just`.
- `%`:  *Mod*ify, chosen as a pun for other languages use `%` as the *mod*ulo
        operator.
- `%%`: ?
- `<`:  Also return the new value of the modified field.
- `<<`: Also return the old value of the modified field.



### Convenience definitions

The following symbols are all used as shortcuts to do something specific with a
value. For example, `&&~ x` applies `(&& x)`, and `<>~ x` `mappend`s `x` to the
field pointed at.

- `+`:  Add a value
- `-`:  Subtract a value
- `*`:  Multiply by a value
- `//`: Divide by a value (`/` was not taken because `/=` is not the "divide by"
        operation, but inequality).
- `^`:  Raise the target `Num` to a nonnegative integral power.
- `^^`: Raise the target `Fractional` to an integral power.
- `**`: Raise the target `Floating` to a power.
- `||`: Logical or.
- `&&`: Logical and.
- `<>`: Append a value using `mappend`.



## Examples

- `^@.` views (`^`) both the index (`@`) and the actual value (`.`) pointed to
  by the lens.

  Example: `("hello","world","!!!") ^@. _2` is `(1,"world")`, since
  "world" is the entry at the second (`_2`) entry of the zero-indexed tuple,
  hence the retrieved index `1`.

- `<%~` modifies (`%`) a field inside a structure, and also returns the new
  value of that field (`<`). When applied to a `Traversal`, it modifies all
  targeted values, and also returns a "monoidal summary" (e.g. a list) of all
  the modified values.

- `<<>~` is first and foremost in the library because in a language that has
  robot monkey operators `(:[])` and Kleisli fish `>=>`, a rat operator must not
  be missing.

  It also `mappend`s (`<>`) a value to a field and returns the new value along
  with the modified structure (the first `<`).