Installing GHC+Platform manually
================================

This is a walkthrough for installing GHC+Platform on Linux from the precompiled
binaries+source with documentation, profiling and shared libraries.

(Don't stop reading halfway, because there are a couple of "I told you so"
pitfalls to avoid.)


Preparation
-----------

- Download the precompiled GHC tarball, and the Haskell platform source
  distribution tarball, and extract them somewhere.

- Delete/rename ~/.ghc and ~/.cabal so your home dir looks like there hasn't
  been any Haskell before.





Install GHC
-----------

Go to the directory you extracted GHC to, and run

    ./configure \
    --prefix=PREFIX \
    --bindir=BINDIR \
    --enable-shared \
    --enable-profiling

This will prepare the GHC installation. A note on the directories:

- **PREFIX** is /usr/local by default. This will be where all the related files
  are put, e.g. the precompiled Base libraries. If you want to install GHC
  globally you can leave it as it is; for a local installation, change it to
  /home/user/ghc or something.
- **BINDIR** defaults to PREFIX/bin. This is where the executables (e.g. ghc,
  ghci, runghc) will be put. BINDIR should be in your $PATH. For local
  installations, BINDIR could be something like /home/user/bin.

The *shared* and *profiling* parameters enable installation of shared Base
libraries and ones for profiling purposes. The more important part is profiling,
because otherwise Base doesn't support it, hence no program that depends on Base
(i.e. any program you can possibly write) doesn't. The only economical solution
is a reinstallation of all of Haskell with profiling enabled. I suggest avoiding
that.

Next, run

    make install

This should not take long.





Platform
--------

Change to where you extracted the Platform to, and run

    ./configure \
    --prefix=PREFIX \
    --enable-shared \
    --enable-profiling

If you want a local installation, make sure to adjust the PREFIX to your likings
(e.g. /home/user/haskell-platform, default is /usr/local). Next, run

    make
    make install

The make step will take its time, especially with profiling and shared enabled.
I'm not sure multithreaded make is supported, but you can just pass -jX and hope
for a faster build just in case.



Configuration
-------------

1. Run 'cabal install cabal-install'. You'll get a couple of errors, but the
   script will generate ~/.cabal. Open ~/.cabal/config, enable 'shared',
   'library-profiling' and 'documentation', so that these are automatically
   built when you install future libraries.

2. Run 'cabal update' to fix the error raised in the previous step.

3. Run 'cabal install cabal-install' again; everything should work fine this
   time.

4. If you had one before, remember to restore your old .ghc/ghci.conf, as it has
   been removed in step 1.

Done :-)