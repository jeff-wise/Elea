

module Elea.Prelude
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
    , Hashable
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
      -- ** Hashing
    , hash, hashWithSalt
      -- ** Print
    , Prelude.print
      -- ** Command line args
    , TimeOfDay, Day
    , Pico
    , (Control.Lens.Getter.^.)
    , Control.Lens.Getter.view
    , (Control.Lens.Setter..~)
    , (Control.Lens.Setter.%~)
    , Control.Lens.Setter.over
    , (Control.Lens.Combinators.&)
    , Control.Lens.TH.makeLenses
    , module Debug.Trace
    , (<|), (|>), (><)
    , viewl
    , ViewL (..)
    , (L.++)
    , (>$>)
    , module Control.Concurrent.STM
    , module Control.Concurrent
    , module Data.Traversable
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


import Data.Foldable as F
import Data.Traversable


import Data.Word (Word8, Word32, Word64, Word)
import Data.Int (Int32, Int64)


import Control.Concurrent.STM
import Control.Concurrent

import qualified Data.Maybe
import qualified Data.Either
import qualified Data.Ord
import qualified Data.Function
import qualified Data.Tuple
import qualified Data.String


-- Hashing
import Data.Hashable (Hashable, hash, hashWithSalt, hashUsing)


-- Lenses
import qualified Control.Lens.Getter
import qualified Control.Lens.Setter
import qualified Control.Lens.Combinators
import qualified Control.Lens.TH


-- Containers
import Data.Sequence (Seq, (<|), (|>), viewl, ViewL (..), (><))
import Data.HashMap.Strict (HashMap)
import Data.HashSet  as Set (HashSet, toList)


-- Lists
import Data.List.Stream as L


-- Time
import Data.Time.Calendar (Day(..))
import Data.Time.LocalTime (TimeOfDay(..))

-- For added instances/functions
import Data.Fixed (Pico, Fixed, E12, showFixed)




-- added functionality
instance Hashable a ⇒ Hashable (HashSet a) where
  hashWithSalt = hashUsing Set.toList  


instance Hashable a ⇒ Hashable (Seq a) where
  hashWithSalt = hashUsing F.toList  


instance Hashable TimeOfDay where
  hashWithSalt s (TimeOfDay hours mins secs) =
    s `hashWithSalt` hours 
      `hashWithSalt` mins 
      `hashWithSalt` secs


-- PICO, for Time of Day seconds
instance Hashable (Fixed E12) where
  hashWithSalt = hashUsing (showFixed Prelude.False)


instance Hashable Day where
  hashWithSalt = hashUsing toModifiedJulianDay




(>$>) ∷ a → (a → b) → b
(>$>) a f = f a

infixr 0 >$>




