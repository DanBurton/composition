-- | This module is for convenience and demonstrative purposes
-- more than it is for providing actual value.
-- I do not recommend that you rely on this module
-- for performance-sensitive code.
module Data.Composition (
  -- * Math
    (∘)

  -- * Colons and dots
  , (.:)
  , (.:.)
  , (.::)
  , (.::.)
  , (.:::)
  , (.:::.)
  , (.::::)
  , (.::::.)

  -- * Asterisks
  , (.*)
  , (.**)
  , (.***)
  , (.****)
  , (.*****)
  , (.******)
  , (.*******)
  , (.********)

  -- * composeN
  , compose1
  , compose2
  , compose3
  , compose4
  , compose5
  , compose6
  , compose7
  , compose8
  , compose9

  ) where

import Prelude ((.))

-- | The mathematical symbol for function composition.
(∘) :: (b -> c) -> (a -> b) -> a -> c
(∘) = (.)

infixr 9 ∘

-- | Compose two functions. @f .: g@ is similar to @f . g@
-- except that @g@ will be fed /two/ arguments instead of one
-- before handing its result to @f@.
--
-- This function is defined as
--
-- > (f .: g) x y = f (g x y)
--
-- Example usage:
--
-- > concatMap :: (a -> [b]) -> [a] -> [b]
-- > concatMap = concat .: map
--
-- Notice how /two/ arguments
-- (the function /and/ the list)
-- will be given to @map@ before the result
-- is passed to @concat@. This is equivalent to:
--
-- > concatMap f xs = concat (map f xs)
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)

infixr 8 .:

-- | Equivalent to '.:'
--
-- The pattern of appending asterisks is
-- straightforward to extend to similar functions:
-- (compose2 = .*, compose3 = .**, etc).
-- However, @.:@ has been commonly adopted amongst Haskellers,
-- and the need for compose3 and beyond is rare in practice.
(.*) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.*) = (.) . (.)

infixr 8 .*

(.**) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.**) = (.) . (.*)

(.***) = (.) . (.**)
(.****) = (.) . (.***)
(.*****) = (.) . (.****)
(.******) = (.) . (.*****)
(.*******) = (.) . (.******)
(.********) = (.) . (.*******)

infixr 8 .**
infixr 8 .***
infixr 8 .****
infixr 8 .*****
infixr 8 .******
infixr 8 .*******
infixr 8 .********


-- | @composeN f g@ means give @g@ @N@ inputs
-- and then pass its result to @f@.
compose1 :: (b -> c) -> (a -> b) -> a -> c
compose1 = (.)

compose2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
compose2 = (.*)

compose3 :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
compose3 = (.**)

compose4 = (.***)
compose5 = (.****)
compose6 = (.*****)
compose7 = (.******)
compose8 = (.*******)
compose9 = (.********)

-- | One compact pattern for composition operators is to
-- "count the dots after the first one",
-- which begins with the common '.:', and proceeds by first
-- appending another @.@ and then replacing it with @:@
(.:.) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.:.) = (.**)

(.::) = (.***)
(.::.) = (.****)
(.:::) = (.*****)
(.:::.) = (.******)
(.::::) = (.*******)
(.::::.) = (.********)

infixr 8 .:.
infixr 8 .::
infixr 8 .::.
infixr 8 .:::
infixr 8 .:::.
infixr 8 .::::
infixr 8 .::::.
