

module Test.Prelude
    ( -- * Standard
      -- ** Operators
      (Prelude.$)
    , (Prelude.$!)
    , (Prelude.&&)
    , (Prelude.||)
      -- ** Functions
    , Prelude.not
    , Prelude.otherwise
    , Prelude.fst
    , Prelude.snd
    , Prelude.maybe
    , Prelude.either
    , Prelude.flip
    , for
    , Prelude.const
    , Prelude.error
    , Prelude.odd
    , Prelude.even
    , Prelude.uncurry
    , Prelude.curry
    , Data.Tuple.swap
    , Prelude.until
    , Prelude.asTypeOf
    , Prelude.undefined
    , Prelude.seq
      -- ** Type classes
    , Prelude.Ord (..)
    , Prelude.Eq (..)
    , Prelude.Bounded (..)
    , Prelude.Enum (..)
    , Prelude.Show (..)
    , Prelude.Read
    , Prelude.Functor (..)
    , Prelude.Monad (..)
    , (Control.Monad.=<<)
    , Data.String.IsString (..)
      -- ** Numeric type classes
    , Prelude.Num (..)
    , Prelude.Real (..)
    , Prelude.Integral (..)
    , Prelude.Fractional (..)
    , Prelude.Floating (..)
    , Prelude.RealFrac (..)
    , Prelude.RealFloat(..)
      -- ** Data types
    , Prelude.Maybe (..)
    , Prelude.Ordering (..)
    , Prelude.Bool (..)
    , Prelude.Char
    , Prelude.IO
    , Prelude.Either (..)
      -- ** Containers
    , HashMap
    , HashSet
    , Seq
      -- ** Numbers
    , Word, Word8, Word32, Word64
    , Prelude.Int
    , Int32, Int64
    , Prelude.Integer
    , Prelude.Rational
    , Prelude.Float
    , Prelude.Double
      -- ** Numeric functions
    , (Prelude.^)
    , (Prelude.^^)
    , Prelude.subtract
    , Prelude.fromIntegral
    , Prelude.realToFrac
      -- ** Monoids
    , Monoid (..)
    , (<>)
      -- ** Category
    , module Control.Category
      -- ** Arrow
    , Control.Arrow.first
    , Control.Arrow.second
    , (Control.Arrow.***)
    , (Control.Arrow.&&&)
      -- ** Maybe
    , Data.Maybe.mapMaybe
    , Data.Maybe.catMaybes
    , Data.Maybe.fromMaybe
    , Data.Maybe.isJust
    , Data.Maybe.isNothing
    , Data.Maybe.listToMaybe
    , Data.Maybe.maybeToList
      -- ** Either
    , Data.Either.partitionEithers
    , Data.Either.lefts
    , Data.Either.rights
      -- ** Ord
    , Data.Function.on
    , Data.Ord.comparing
      -- ** Applicative
    , Control.Applicative.Applicative (..)
    , (Control.Applicative.<$>)
    , (Control.Applicative.<|>)
      -- ** Monad
    , (Control.Monad.>=>)
     -- ** Strings
    , Prelude.String
      -- ** Print
    , Prelude.print
      -- ** Command line args
    , (Control.Lens.Getter.^.)
    , (Control.Lens.Setter..~)
    , (Control.Lens.Setter.%~)
    , (Control.Lens.Combinators.&)
    , Control.Lens.TH.makeLenses
    , (<|)
    , viewl
    , ViewL (..)
    , (L.++)
    , module Test.Tasty
    , module Test.Tasty.HUnit
    , module Debug.Trace
    , module Control.Concurrent.STM
    ) where

import qualified Prelude
import Prelude (Char, Eq, Bool)


import Debug.Trace

import Data.Monoid (Monoid (..))
import Data.Monoid ((<>))
import qualified Control.Arrow
import Control.Applicative
import Control.Category
import qualified Control.Monad


import Data.Word (Word8, Word32, Word64, Word)
import Data.Int (Int32, Int64)


import qualified Data.Maybe
import qualified Data.Either
import qualified Data.Ord
import qualified Data.Function
import qualified Data.Tuple
import qualified Data.String


-- Lenses
import qualified Control.Lens.Getter
import qualified Control.Lens.Setter
import qualified Control.Lens.Combinators
import qualified Control.Lens.TH


-- Containers
import Data.Sequence (Seq, (<|), viewl, ViewL (..))
import Data.HashMap.Strict (HashMap)
import Data.HashSet  as Set (HashSet)


-- Lists
import Data.List.Stream as L


import Test.Tasty
import Test.Tasty.HUnit


import Control.Concurrent.STM

for = Prelude.flip L.map
