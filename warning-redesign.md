GHC warning flag redesign
=========================



This is me brainstorming about the redesign of GHC's warning mechanism, see also [the corresponding GHC Wiki page][ghcwiki-warn].



Use `-W*` instead of `-fwarn-*`
-------------------------------

[*Implemented, shipping with GHC 8.0.*][minus-w-synonyms]

Instead of the `-fwarn-*` namespace to set dynamic runtime flags that represent warnings, use `-W*`. This is shorter, easier to remember, more in sync with `-Wall` and friends, and closer to what other compilers have.



Add responsible warning flags to their message texts
----------------------------------------------------

When enabling many warnings at once (e.g. by `-Wall`), we receive warnings we don't know the purpose of, or that we know we want to ignore under certain circumstances. However, the connection between the warning and how to modify it is not always clear.

We want to include the specific flag that triggered a warning in the message for all warnings, like so:

```
X.hs:1:1: Warning [-fwarn-type-defaults]:
    Defaulting the following constraint to type 'Integer'
    ...
```

There are a couple of things to keep in mind when implementing this:

- What if a warning was enabled by multiple different flags, like `-W -Wall`?
- When warnings were enabled by a group like `-Wall`, we should report not only the flag itself, but also the group that entailed it



Allow `-Werror` on a per-warning basis
--------------------------------------

GHC currently offers only very crude control over how to behave under these special conditions. We'd like to have the following capabilities in GHC:

- Declare specific warnings to be errors, e.g. `-Werror=tabs`
- Declare all warnings to be errors with exceptions, e.g. `-Wall -Wwarn=unused-do-binds`



More principled warning groups
------------------------------

Warnings can have many reasons:

- Indicating code that is almost always buggy, e.g. incomplete pattern matches
- Enforcing best practices, e.g. avoiding tabs, requiring top-level type signatures
- Cleaning the code, e.g. overlapped patterns, unrecognized pragmas
- Head start warnings about future incompatibilities, as e.g. the AMP or the MFP introduced them

We'd like to have more warning groups so that users can easily express that they want all "almost certainly a bug" warnings on, without reading every available flag in the user's guide.

Things that come to mind right now:

- `-Wcompat`, warnings that indicate potentially breaking code with future changes; think of it as the temporal inverse of a deprecation
- `-W`, "useful most of the time" warnings
- `-Wall`, "useful most of the time and then some" warnings
- `-WALL` to enable *all* warnings
- `-w` to disable all warnings


[ghcwiki-warn]: https://ghc.haskell.org/trac/ghc/wiki/Design/Warnings
[minus-w-synonyms]: https://github.com/ghc/ghc/commit/2206fa8cdb1209320f3690690b610320b4810de6
