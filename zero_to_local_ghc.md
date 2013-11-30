From zero to local GHC
======================

This text is for you if you've just installed a new OS, and the desired environment is having GHC installed in a local folder. Note that the easy way to get Haskell running is just [getting the Haskell Platform][platform].

The guide was created while I was doing the procedure myself on Linux (Xubuntu 13.10) for GHC 7.6.3 in November 2013; the commands for other systems and times may differ, but the general approach should be the same.

The order in which programs are built is chosen so that all installed libraries have their documentation built in `.cabal/share/doc`, including Cabal and Haddock (and their dependencies).


[platform]: http://www.haskell.org/platform/





Bootstrapping GHC
-----------------

The first (and longest) part will setup a local GHC installation (including Cabal and Haddock).



### Obtain a bootstrapping GHC

First you'll need a temporary GHC, which can be obtained whatever way you like. Probably the best choices

1. Get a pre-packaged GHC build from your repos

2. Download precompiled GHC from the website. However, The problem with the current GHC build servers is that they're outdated. If you download the binary builds, you'll probably encounter an error à la "libgmp.so.3 not found" when invoking GHC. When you do not see this error you're either very lucky or have an outdated system.

Either of these will do; whatever you install here will be safe to delete later.



### Get Cabal

Next, download the Cabal source via the [cabal-install tool download][cabal-install]. The readme included in the download will mention how to proceed, and the errors along the way will hint at what libraries you should install from the repositories (for example Perl is required). Although this Cabal installation is temporary, I suggest putting it in a permanent place anyway; should you delete your `.cabal` later and lose all your Haskell executables, you'll have a working version waiting for you.

Run `cabal update`, then run `cabal install alex happy haddock hscolour`. (The latter is required so that building GHC yourself includes source links in its documentation.)

In order for other programs to call your local installations later, add `~/.cabal/bin` to your `$PATH`. (I recommend doing this with a higher priority than your other entries, so that when a program is both in say `~/bin` and `~/.cabal/bin`, the latter is preferred, as that is where the most recent versions will be.)

[cabal-install]: http://www.haskell.org/cabal/download.html



### Compile GHC from source

Download the GHC source, and run the `configure` script with custom parameters to suit your local needs (in particular you'll want to have  a look at the `--prefix` and `--bindir` flags, see `./configure --help` for additional info). When you and the configuration script are happy, make sure you have the other dependencies listed in the [GHC building guide][ghc-building-guide].

Now run `make` to start the full build. You can use the `-jN` parameter for `make`, where `N` is the number of threads you want to use; for ludicrous speed, use `N = <number of cores +1>`. If the build fails it's likely due to a missing library; install it and run `make` again. When the build finishes, run `make install`.  Congratulations, you now have a self-compiled GHC in your `--prefix` directory. As for the time required for compilation, it shouldn't take *too* long. On my laptop from 2010 with 8 cores, `make -j9` finished in under an hour.

[ghc-building-guide]: https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation





Starting over
-------------

The previous steps solve the problem of not having GHC. However, your environment is still dirty from the library compilations before you had Haddock (and in order to build Haddock as well).



### Cleaning up

First of all, you can now uninstall your bootstrapping GHC. Unfortunately, that will break the GHC manpage; add `<prefix>/share/man/` to your `$MANPATH` to fix this, and run `sudo mandb` to rebuild the cache for good measure afterwards.

Next, clean your local directories by deleting `~/.{ghc,cabal}`.



### Configuring Cabal

Now run `cabal update` again to get a fresh `~/.cabal/config`. There are a couple of options you should consider enabling here:

- `library-profiling` compiles all installed modules for profiling automatically. This requires an additional compilation run for every installed module, potentially doubling compilation time. You should enable this anyway, because without it profiling a program you don't have the dependencies compiled for profiling will be a painful process. (... that will end with you reinstalling everything with profiling enabled. How I know this will happen? Take a guess.)
- `shared` adds yet another separate compilation step, in order to build librares for shared use. Unless you know you want this, you probably don't.
- `documentation` automatically builds local Haddock docs for installed libraries. This may seem redundant, but you'll be *so* happy you enabled it on that train ride or transatlantic flight, and the documentation step when installing new libraries is usually much shorter than the actual compilation.



### Reinstalling the universe

Alright, it's time to install again!

```sh
# (Haddock is automatically built with GHC and in BINDIR)

# Rebuild the core tools with the new GHC because why not
cabal install -jN cabal-install alex happy

# Packages contained in the Haskell Platform (here: 2013.2)
cabal install -jN async attoparsec case-insensitive cgi fgl GLUT GLURaw hashable haskell-src html HTTP HUnit mtl network OpenGL OpenGLRaw parallel parsec QuickCheck random regex-base regex-compat regex-posix split stm syb text transformers unordered-containers vector xhtml zlib

# Custom packages
cabal install -jN hlint dlist pipes lens ...
```