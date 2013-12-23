

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
    , for
    , Prelude.const
    , Prelude.error
    , putStrLn
    , getArgs
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
    , Prelude.Show
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
      -- * Re-exports
      -- ** Packed reps
    , ByteString
    , LByteString
    , LText
      -- ** Containers
    , Map
    , HashMap
    , HashSet
    , Seq
    , Vector
    , UVector
    , Unbox
    , Hashable
      -- ** Numbers
    , Word
    , Word8
    , Word32
    , Word64
    , Prelude.Int
    , Int32
    , Int64
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
      -- ** Arrow
    , Control.Arrow.first
    , Control.Arrow.second
    , (Control.Arrow.***)
    , (Control.Arrow.&&&)
    , module Control.Category
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
    , GHC.Exts.Down (..)
      -- ** Applicative
    , Control.Applicative.Applicative (..)
    , (Control.Applicative.<$>)
    , (Control.Applicative.<|>)
      -- ** Monad
    , (Control.Monad.>=>)
      -- ** Transformers
    , Control.Monad.Trans.Class.lift
    , Control.Monad.IO.Class.MonadIO
    , Control.Monad.IO.Class.liftIO
      -- ** Exceptions
    , Control.Exception.Exception (..)
    , Data.Typeable.Typeable (..)
    , Control.Exception.SomeException
    , Control.Exception.IOException
    , Control.Exception.Lifted.throwIO
    , Control.Exception.Lifted.try
    , Control.Exception.Lifted.catch
    , Control.Exception.Lifted.bracket
    , Control.Exception.Lifted.onException
    , Control.Exception.Lifted.finally
      -- ** Files
    , F.FilePath
    , (F.</>)
    , (F.<.>)
    , F.hasExtension
    , F.basename
    , F.filename
    , F.directory
      -- ** Strings
    , Prelude.String
      -- ** Hashing
    , hash
    , hashWithSalt
      -- ** Print
    , Prelude.print
      -- ** Command line args
    , readArgs
    , module Data.List.Stream
    , TimeOfDay, Day
    , module Control.Lens
    ) where

import qualified Prelude
import Prelude (Char, Eq, Bool)

import Data.Hashable (Hashable, hash, hashWithSalt, hashUsing)
import Data.Vector.Unboxed (Unbox)

import Data.Monoid (Monoid (..))
import qualified Control.Arrow
import Control.Applicative
import Control.Category
import qualified Control.Monad
import qualified Control.Exception
import qualified Control.Exception.Lifted
import qualified Data.Typeable

import qualified Filesystem.Path.CurrentOS as F

import Data.Word (Word8, Word32, Word64, Word)
import Data.Int (Int32, Int64)

import qualified Data.Text.IO

import qualified Data.Maybe
import qualified Data.Either
import qualified Data.Ord
import qualified Data.Function
import qualified Data.Tuple
import qualified Data.String

import qualified Control.Monad.Trans.Class
import qualified Control.Monad.IO.Class
import Control.Monad.IO.Class (MonadIO (liftIO))

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy
import qualified Data.Text as T
import qualified Data.Text.Lazy
import Data.Vector (Vector)
import qualified Data.Vector.Unboxed
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.HashMap.Strict (HashMap)
import Data.HashSet  as Set (HashSet, toList)
import qualified ReadArgs

import qualified System.Environment
import qualified Data.Text
import qualified Data.List
import qualified GHC.Exts

import Data.Time.Calendar (Day(..))
import Data.Time.LocalTime (TimeOfDay(..))

import Data.Fixed (Fixed, E12, showFixed)


import qualified Data.Foldable as F

import Data.Monoid ((<>))

-- added imports
import Data.List.Stream

import Control.Lens


for = Prelude.flip map

type LText = Data.Text.Lazy.Text
type LByteString = Data.ByteString.Lazy.ByteString
type UVector = Data.Vector.Unboxed.Vector


getArgs :: MonadIO m => m [T.Text]
getArgs = liftIO (Data.List.map Data.Text.pack <$> System.Environment.getArgs)


putStrLn :: MonadIO m => T.Text -> m ()
putStrLn = liftIO . Data.Text.IO.putStrLn


readArgs :: (MonadIO m, ReadArgs.ArgumentTuple a) => m a
readArgs = liftIO ReadArgs.readArgs



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


