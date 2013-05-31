Installing GHC+Platform manually
================================

This is a very basic walkthrough for installing GHC for Linux from the precompiled binaries, followed by a compilation of the Haskell Platform (HP). For some time I've had this crude text file telling myself how to do it every once in a while (for example in case of new HP or unfixable dependency hell), and I decided to give it some markdown and upload it, so here it is.

The reasons for a manual installation are simple: you know where stuff goes, and you can take care of the configuration yourself. I use a local installation of everything because it's easy to delete and set up again, plus it's suitable for environments where you don't have root access and so on.




Preparation
-----------

- Download the [Haskell Platform source distribution tarball][platform.tar] and the [precompiled GHC tarball][ghc.tar] corresponding to it and extract the files somewhere.

- Delete (backup!) `~/.ghc` and `~/.cabal` so your home dir looks like there hasn't been any Haskell before.

- Make sure you don't have an old version of `cabal` installed somewhere else. You can check thish with `which cabal`, which should yield a blank result. If not, delete the listed files.

[ghc.tar]: http://www.haskell.org/ghc/download
[platform.tar]: http://www.haskell.org/platform/linux.html





GHC
---

Go to the directory you extracted GHC to, and run

    ./configure --prefix=PREFIX --bindir=BINDIR

This will prepare the GHC installation. A note on the directories:

- **PREFIX** is `/usr/local` by default. This will be where all the related files are put, e.g. the precompiled Base libraries. If you want to install GHC globally you can leave it as it is; for a local installation, change it to `/home/user/haskell/ghc` or something.
- **BINDIR** defaults to `PREFIX/bin`. This is where the executables (e.g. ghc, ghci, runghc) will be put. BINDIR should be in your `$PATH`. For local installations, BINDIR could be something like `/home/user/bin`.

Next, run

    make install

This should not take long, as it basically copies a couple of hundred of megs around. (Make sure to use `sudo` if necessary.)





Haskell Platform
----------------

Change to where you extracted the Platform to, and run

    ./configure \
    --prefix=PREFIX \
    --enable-shared \
    --enable-profiling

The `shared` and `profiling` parameters are to build the respective versions of the libraries in the process. Especially profiling is important, since a library compiled with it requires all dependencies to be as well. (If you don't enable it now and find out later you need it, the only viable solution will be starting from scratch again.)

If you want a local installation, make sure to adjust the PREFIX to your likings (e.g. `/home/user/haskell/platform`, default is `/usr/local`). Next, run

    make
    make install

The make step will take its time, especially with profiling and shared enabled. I'm not sure multithreaded make is available, but you can just pass `-jX` and hope for a faster build just in case. (Again, you may need `sudo`.)



Configuration
-------------

1. The Platform has installed a new `cabal`, located in `PLATFORM-PREFIX/bin`. Use this to run `./cabal update` to get the latest package information. This will also initialize your `.cabal` directory.

2. In `~/.cabal/config`, enable `shared`, `library-profiling` and `documentation`, so that subsequent library installations automatically support these features.

3. Again in the Platform's `bin` directory, run `./cabal install cabal-install`. This will get you the newest version of Cabal, with the binary located as usual in `.cabal/bin`.

4. If you had one before, remember to restore your old `.ghc/ghci.conf`, as it has been removed in the very beginning.

Done :-)