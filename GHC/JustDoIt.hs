module GHC.JustDoIt ( JustDoIt, justDoIt, (…) ) where

class JustDoIt a where justDoIt' :: a

justDoIt :: JustDoIt a => a
justDoIt = justDoIt'

(…) :: JustDoIt a => a
(…) = justDoIt'
