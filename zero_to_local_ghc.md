From zero to local GHC
======================

This text is for you if you've just installed a new OS, and the desired environment is having GHC installed in a local folder. Note that the easy way to get Haskell running is just [getting the Haskell Platform][platform].

The guide was created while I was doing the procedure myself on Linux (Xubuntu 13.10) for GHC 7.6.3 in November 2013; the commands for other systems and times may differ, but the general approach should be the same.

The order in which programs are built is chosen so that all installed libraries have their documentation built in `.cabal/share/doc`, including Cabal and Haddock (and their dependencies).


[platform]: http://www.haskell.org/platform/





Getting GHC
-----------

The first (and longest) part will setup a local GHC installation (including Cabal and Haddock).



### Obtain a bootstrapping GHC

To obtain GHC, you first need to obtain GHC. The two possible ways to do so are:

1. Download the binary release, and install it locally using the usual `--prefix/--bindir` flags in `./configure` (see `--help`). The problem with the current GHC build servers is that they're outdated though. If running GHC complains à la "libgmp.so.3 not found", you most likely have a more recent version of said library, and GHC was linked with an old one. Abandon ship, proceed below.

2. Get GHC from your distro's repos (you'll use this one to compile GHC yourself later).

In the following, paragraphs marked as *PRE-BUILT* or *SELF-BUILT* apply depending on which one of the above you choose.



### Get Cabal

Download the Cabal source via the [cabal-install tool download][cabal-install]. The readme included in the download will mention how to proceed, and the errors along the way will hint at what libraries you should install from the repositories (for example Perl is required). Although this Cabal installation is temporary, I suggest putting it in a permanent place anyway; should you delete your `.cabal` later and lose all your Haskell executables, you'll have a working version waiting for you.

Run `cabal update`, then run `cabal install alex happy haddock`.

*SELF-BUILT*: Also install `hscolour`, which is required so that building GHC yourself includes source links in its documentation.

In order for other programs to call your local installations later, add `~/.cabal/bin` to your `$PATH`. (I recommend doing this with a higher priority than your other entries, so that when a program is both in say `~/bin` and `~/.cabal/bin`, the latter is preferred, as that is where the most recent versions will be.)

[cabal-install]: http://www.haskell.org/cabal/download.html



### *SELF-BUILT*: Compile GHC from source

Download the GHC source, and run the `configure` script with custom parameters to suit your local needs (in particular you'll want to have  a look at the `--prefix` and `--bindir` flags, see `./configure --help` for additional info). When you and the configuration script are happy, make sure you have the other dependencies listed in the [GHC building guide][ghc-building-guide].

Now run `make` to start the full build. You can use the `-jN` parameter for `make`, where `N` is the number of threads you want to use; for ludicrous speed, use `N = <number of cores +1>`. If the build fails it's likely due to a missing library; install it and run `make` again. When the build finishes, run `make install`.  Congratulations, you now have a self-compiled GHC in your `--prefix` directory. As for the time required for compilation, it shouldn't take *too* long. On my laptop from 2010 with 8 cores, `make -j9` finished in under an hour.

[ghc-building-guide]: https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation





Starting over
-------------

The previous steps solve the problem of not having GHC. However, your environment is still dirty from the library compilations before you had Haddock (and in order to build Haddock as well).



### Cleaning up

*SELF-BUILT*: You can now uninstall your bootstrapping GHC.

Delete `~/.{ghc,cabal}`, which gives you a clean slate.



### Configuring Cabal

Now run `cabal update` again to get a fresh `~/.cabal/config`. There are a couple of options you should consider enabling here:

- `library-profiling` compiles all installed modules for profiling automatically. This requires an additional compilation run for every installed module, potentially doubling compilation time. You should enable this anyway, because without it profiling a program you don't have the dependencies compiled for profiling will be a painful process. (... that will end with you reinstalling everything with profiling enabled. How I know this will happen? Take a guess.)
- `shared` adds yet another separate compilation step, in order to build librares for shared use. Unless you know you want this, you probably don't.
- `documentation` automatically builds local Haddock docs for installed libraries. This may seem redundant, but you'll be *so* happy you enabled it on that train ride or transatlantic flight. Note that while the doc generation is short when you've just installed a couple of libraries, it can grow significantly larger when there are lots of them (because the index is rebuilt each time you install something new, checking all the old packages). Also, if you want to generate source links in the documentation, you'll have to invoke `cabal install` with the `--haddock-hyperlink-source` flag every time; there is unfortunately no way to specify this in the config.



### Reinstalling the universe

Alright, it's time to install again!

```sh
#!/usr/bin/env sh

# For some reason, documentation is only generated when Haddock is installed;
# having only the Haddock executable skips updating the library index.
# For this reason, install Haddock separately in the beginning.
cabal install -jN haddock

# Rebuild the core tools with the new GHC, mostly so you get all the docs
# generated
cabal install -jN --haddock-hyperlink-source \
      cabal-install alex happy hscolour

# Packages contained in the Haskell Platform (here: 2013.2)
cabal install -jN --haddock-hyperlink-source \
      async attoparsec case-insensitive cgi fgl GLUT GLURaw hashable \
      haskell-src html HTTP HUnit mtl network OpenGL OpenGLRaw parallel parsec \
      QuickCheck random regex-base regex-compat regex-posix split stm syb text \
      transformers unordered-containers vector xhtml zlib

# Custom packages
cabal install -jN --haddock-hyperlink-source \
      hlint dlist pipes lens ...
```



### Adding the manpage

Add `<prefix>/share/man/` to your `$MANPATH` so that `man ghc` works. *SELF-BUILT*: Run `sudo mandb` if `man ghc` still points to the previously removed bootstrapping GHC.