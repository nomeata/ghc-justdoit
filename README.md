ghc-justdoit: a GHC plugin to write the code for you
=========================================

This is a prototype of a code synthesis plugin for GHC, which uses LJT proof
search to instantiate a type.

Synopsis
--------

    {-# OPTIONS_GHC -fplugin=GHC.JustDoIt.Plugin #-}
    module Test where

    import GHC.JustDoIt

    foo :: ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
    foo = (…)

Missing bits
------------

 * The LJT might not be complete, due to insufficient backtracking.
 * The implementation is very much unoptimized.
 * It returns one solution, but not necessary the “best” one. But what is the “best” one?
 * It ignores any recursive type, so it cannot do anything with lists. It would be much more useful if it could do some best-effort thing here as well.

If someone wants to pick it up from here, that’d be great!


Related work
------------

 * [Djinn](http://hackage.haskell.org/package/djinn) and [djinn-ghc](http://hackage.haskell.org/package/djinn-ghc)
 * [exference](http://hackage.haskell.org/package/exference)
 * [curryhoward](https://github.com/Chymyst/curryhoward) for Scala
 * [hezarfen](https://github.com/joom/hezarfen) for Idris

Contact
-------

Please reports bugs and missing features at the [GitHub bugtracker]. This is
also where you can find the [source code].

`ghc-justdoit` was written by [Joachim Breitner] and is licensed under a
permissive MIT [license].

[GitHub bugtracker]: https://github.com/nomeata/ghc-justdoit/issues
[source code]: https://github.com/nomeata/ghc-justdoit
[Joachim Breitner]: http://www.joachim-breitner.de/
[license]: https://github.com/nomeata/ghc-justdoit/blob/LICENSE


