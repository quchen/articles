Haddock+Doctest+Cairo = ♥
=========================

tl;dr
=====

[Here is an example that I think is pretty cool. Most of the package looks like this.][gen-pages]
It mixes doctests with picture output, and links to those pictures in the
documemtation. This is not only a great appetizer for casually browsing the lib,
but also features loads of small examples that can be copypasted and played
around with, cutting the whole path from _I want to use this_ to _I use this_
short.

The whole idea goes back to Bas and Roel van Dijk’s bindings to
[OpenCV (Haddock link)](https://hackage.haskell.org/package/opencv-0.0.2.1/docs/OpenCV-ImgProc-FeatureDetection.html#v:houghCircles)
which they presented at Munihac a long time ago.




Who are we writing docs for?
===============


What makes good documentation? It documents well! But actually there are many
target audiences, and what _well_ means differs wildly. Let’s look at some
target groups!

The committed user
------------------

The committed user wants to use your library. The main reason for this can e.g.
be that your library is a defacto standard, that it’s been recommended by a
friend, that it was mentioned on Reddit to solve that user’s very problem.

The committed user has already decided to use your library, and documentation
serves two goals,

  1. Don’t be so annoying that they reconsider
  2. Get them up to speed reasonably fast

The first point can be handled mostly by not being awfully documented, while the
second one would be a huge bonus on top. I cannot overstate how cool it is to
have code examples.

The author
----------

The author wrote the lib, knows their way around, and doesn’t need no docs! The
code is self-documenting, clearly structured, and there’s no need to run
Haddock.

The author, 6 months in
-----------------------

The author once wrote the lib, knew their way around, and didn’t need no docs!
The code can be understood by reading it for a bit, and the structure seems
sensible, but it’s a bit hard to see the big picture.

The author 6 months in is _a committed user_, nothing more. They long for
examples to get up to speed again. They need that extra explanation in Haddock.
And lastly, even power users like eye candy.

The window shopper
------------------

You’ve heard about this library, and you’re eating your Müsli in front of the
computer. You see a link to some interesting lib. You click on it to see what
it’s about.

Documentation has a very much different goal here:

  1. It’s the frontend of your library.
  2. Convert interested, or maybe even non-interested, people to lib users. Eye
     candy! Show off cool examples!

What’s the union of those groups?
===============

1. Runnable code examples. Lots of them.

   Great for showing off your API in action. Great for copy+pasting when you’re
   a bit lost. Great for not having to read the entire documentation to discover
   that pearl in another module.

   Really, Docetst is invaluable. Use Doctest. [Github](https://github.com/sol/doctest), [Hackage](https://hackage.haskell.org/package/doctest).

2. Deeper explanations in Haddock text.

   Haddock syntax is a bit awkward, but get over it: you’re the author, not the
   target group. Definitions without Haddock comments look undocumented, give
   them a Haddock comment, even if it just restates the obvious. While doing it,
   consider linking to related functions. And of course add a doctest o:-)

3. Make sure the documentation is available online, somewhere. Should Hackage
   fail to build docs, put them on Github Pages or wherever.
   [Here](https://github.com/quchen/generative-art/blob/72507a48231637e8967255ab9f291301d6454ad5/.github/workflows/haskell.yml)
   is my Github Actions file that automatically uploads the [docs linked to in
   the tl;dr section][gen-pages] to Github Pages.

4. If you can paint pictures, do so!

   You can run [Cairo](https://hackage.haskell.org/package/cairo) (or [Diagrams](https://hackage.haskell.org/package/diagrams) or whatever) in your doctests. If that’s
   applicable to your lib, do it!

5. If you didn’t know: Haddock supports $\LaTeX$! Here’s an [example](https://quchen.github.io/generative-art/generative-art-0.1.0.0/Geometry-Core.html#t:Transformation).


[gen-pages]: https://quchen.github.io/generative-art/generative-art-0.1.0.0/Geometry-Algorithms-Delaunay.html
