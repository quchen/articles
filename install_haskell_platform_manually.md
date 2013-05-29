Installing GHC+Platform manually
================================

This is a very basic walkthrough for installing GHC for Linux from the precompiled binaries, followed by a compilation of the Haskell Platform (HP). For some time I've had this crude text file telling myself how to do it every once in a while (for example in case of new HP or unfixable dependency hell), and I decided to give it some markdown and upload it, so here it is.

The reasons for a manual installation are simple: you know where stuff goes, and you can take care of the configuration yourself. I use a local installation of everything because it's easy to delete and set up again, plus it's suitable for environments where you don't have root access and so on.




Preparation
-----------

- Download the [Haskell Platform source distribution tarball][platform.tar] and the [precompiled GHC tarball][ghc.tar] corresponding to it and extract the files somewhere.

- Delete (backup!) `~/.ghc` and `~/.cabal` so your home dir looks like there hasn't been any Haskell before.

[ghc.tar]: http://www.haskell.org/ghc/download
[platform.tar]: http://www.haskell.org/platform/linux.html





GHC
---

Go to the directory you extracted GHC to, and run

    ./configure --prefix=PREFIX --bindir=BINDIR

This will prepare the GHC installation. A note on the directories:

- **PREFIX** is `/usr/local` by default. This will be where all the related files are put, e.g. the precompiled Base libraries. If you want to install GHC globally you can leave it as it is; for a local installation, change it to `/home/user/ghc-VERSION` or something.
- **BINDIR** defaults to `PREFIX/bin`. This is where the executables (e.g. ghc, ghci, runghc) will be put. BINDIR should be in your `$PATH`. For local installations, BINDIR could be something like `/home/user/bin`.

Next, run

    make install

This should not take long, as it basically copies a couple of hundred of megs around. (Make sure to use `sudo` if necessary.)





Haskell Platform
----------------

Change to where you extracted the Platform to, and run

    ./configure --prefix=PREFIX --enable-shared --enable-profiling

The `shared` and `profiling` parameters enable installation of shared Base libraries and ones for profiling purposes. The more important part is profiling, because you can't build any program with profiling which has a dependency not built with it. If you don't do this and have to profile at some point (more sooner than later, probably), then the only economical solution is a reinstallation everything with the option present this time.

If you want a local installation, make sure to adjust the PREFIX to your likings (e.g. `/home/user/haskell-platform-VERSION`, default is `/usr/local`). Next, run

    make
    make install

The make step will take its time, especially with profiling and shared enabled. I'm not sure multithreaded make is available, but you can just pass `-jX` and hope for a faster build just in case. (Again, you may need `sudo`.)



Configuration
-------------

1. Run `cabal install cabal-install`. You'll get a couple of errors, but the script will generate `~/.cabal`. Open `~/.cabal/config`, enable `shared`, `library-profiling` and `documentation`, so that these are automatically built when you install future libraries. (If `cabal` cannot be found, make sure the BINDIR parameter from the GHC installation is in your `$PATH` environment variable.)

2. Run `cabal update` to fix the error raised in the previous step.

3. Run `cabal install cabal-install` again; everything should work fine this time.

4. If you had one before, remember to restore your old `.ghc/ghci.conf`, as it has been removed in step 1.

Done :-)