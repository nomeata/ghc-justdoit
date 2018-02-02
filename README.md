ghc-justdoit: a to write the code for you
=========================================

Longer README will follow.

Synopsis
--------

    {-# OPTIONS_GHC -fplugin=GHC.JustDoIt.Plugin #-}
    module Test where

    import GHC.JustDoIt

    foo :: ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
    foo = (…)

Related work
------------

 * [Djinn](http://hackage.haskell.org/package/djinn) and [djinn-ghc](http://hackage.haskell.org/package/djinn-ghc)
 * [exference](http://hackage.haskell.org/package/exference)
 * [curryhoward](https://github.com/Chymyst/curryhoward) for Scala

Contact
-------

Please reports bugs and missing features at the [GitHub bugtracker]. This is
also where you can find the [source code].

`bSpokeLight` was written by [Joachim Breitner] and is licensed under a
permissive MIT [license].

[GitHub bugtracker]: https://github.com/nomeata/ghc-justdoit/issues
[source code]: https://github.com/nomeata/ghc-justdoit
[Joachim Breitner]: http://www.joachim-breitner.de/
[license]: https://github.com/nomeata/ghc-justdoit/blob/LICENSE


