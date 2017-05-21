# Make macros mean something – readable backwards compatibility with CPP

A common problem in maintaining libraries is making sure it builds with a range
of past compiler versions. Since GHC comes bundled with the `base` library, we
often see changes in the location or even availability of core definitions.

Libraries often resort to using CPP, the C preprocessor, to change key parts of
the code so it compiles, and maybe does not even produce warnings in any of its
possible forms. Maybe you’ve sometimes seen code like

```haskell
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
```

which is necessary since before GHC 7.8.1 (which comes with `base-4.8.0.0`)
Applicative wasn’t in the Prelude, so we had to import it manually if we wanted
to write instances for it. However, starting with GHC 7.8, importing it often
resulted in a rednudant import (breaking `-Wall -Werror`), so we’d only like to
include it if necessary, namely when the compiler is old.



# The standard macros

GHC itself has a number of standard macros for checking various settings, such
as:

```c
MIN_VERSION_GLASGOW_HASKELL(X,Y,Z,W) // Is GHC >= X.Y.Z.W?

__GLASGOW_HASKELL__ >= 710 // Before GHC 7.10, we had to use this instead

linux_HOST_OS // Defined if we’re on Linux
mingw32_HOST_OS // Defined if we’re on Windows
```

This is aided by Cabal/Stack generating macros that allow CPP to check with
which dependencies the library is built,

```c
MIN_VERSION_aeson(1,0,2) // Is aeson >= 1.0.2 used?
```

(The macro definitions are in
`dist/<arch-os>/Cabal-<version>/build/autogen/cabal_macros.h`.)

Since GHC 8.0 GHC automatically generates these macros as well, so we don’t have
to rely on Cabal/Stack. [More details about this can be found in the GHC user’s
guide.][ghc-cpp-macros]



# Defining Semantic macros

In reality, nobody wants to check whether there is some version of `base` used
for compilation – we’d much rather want to know whether e.g. Applicative is a
superclass of Monad. After some searching we find out that that came in base
4.8, so we add our `#if MIN_VERSION_base(4,8,0)` where suitable. However, when
reading code written like this, it’s very hard to see why that macro (and not
one checking for 4.7, say) is used, for example in this snippet from my own
code:

```haskell
#if MIN_VERSION_base(4,8,0)
import Data.Foldable (Foldable (..))
import Data.Monoid   hiding ((<>))
import Prelude       hiding (foldr, foldr1)
#endif
```

Wouldn’t it be nice to have something like this instead?

```haskell
#if !FOLDABLE_TRAVERSABLE_IN_PRELUDE
import Data.Foldable (Foldable (..))
import Prelude       hiding (foldr, foldr1)
#endif
#if !MONOID_IN_PRELUDE
import Data.Monoid hiding ((<>))
#endif
```

This clearly communicates the intention, and if we get the boundaries wrong we
can fix them in one place, namely where the macros are defined.

We can build this by creating a new header file somewhere within our project; I
like to call it `version-compatibility-macros.h`, with the following contents:

```haskell
#define APPLICATIVE_MONAD               MIN_VERSION_base(4,8,0)
#define FOLDABLE_TRAVERSABLE_IN_PRELUDE MIN_VERSION_base(4,8,0)
#define MONOID_IN_PRELUDE               MIN_VERSION_base(4,8,0)
#define NATURAL_IN_BASE                 MIN_VERSION_base(4,8,0)

#define SEMIGROUP_IN_BASE               MIN_VERSION_base(4,9,0)
#define MONAD_FAIL                      MIN_VERSION_base(4,9,0)
```

All that’s left is making GHC/Cabal/Stack aware of these macros by adding
`include-dirs: <dir>` to the `.cabal` file, where `<dir>` contains the above
header file, and we can `#include` it in any of our Haskell source files and use
the macros. (If for whatever reason GHC is used standalone, `-I<dir>` adds
`<dir>` to the `#include` search path.)

I think it would be a good idea for GHC to support these macros natively so we
don’t have to resort to writing our little header file and worrying about the
include path, but for now this is as good as it gets.








[ghc-cpp-macros]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/phases.html#standard-cpp-macros
