module BellKAT.Definitions.Structures.Quantum
    ( Quantum
    , CreatesBellPairs(..)
    , Tests(..)
    , TestsQuantum
    , TestsOrderedLayeredQuantum(..)
    , OrderedQuantum
    , OrderedLayeredQuantum(..)
    , TestsOrderedQuantum
    , subjectTo
    ) where

import           Data.Orphans ()

import           BellKAT.Definitions.Structures.Basic
import           BellKAT.Definitions.Core

class CreatesBellPairs a tag | a -> tag where
    -- | embedding Bell pair creation represented by `CreateBellPairArgs` into the structure `a`
    tryCreateBellPairFrom :: CreateBellPairArgs tag -> a

-- | `Quantum` is a `ParallelSemigroup` with `BellPair` creation
-- `a` is the type of the carrier and `t` is a tag
class (Semigroup a, ParallelSemigroup a, CreatesBellPairs a t) => Quantum a t | a -> t where

class Tests a test tag | a -> tag, a -> test where
    test :: test tag -> a

class (Quantum a tag, Tests a test tag) => TestsQuantum a test tag | a -> test, a -> tag where

class (Quantum a tag, OrderedSemigroup a) => OrderedQuantum a tag

-- | `Quantum` that has two domains: 
--
--  * `a` for the top-level behavior having `Semigroup` and `ParallelSemigroup` structures)
--
--  * `Layer a` for the one-layer behavior having `OrderedSemigroup` structure
--
-- `liftLayer` is an embedding from a Layer
class (Semigroup a, ParallelSemigroup a, OrderedSemigroup (Layer a)) => OrderedLayeredQuantum a t | a -> t where
    data Layer a

    orderedTryCreateBellPairFrom :: CreateBellPairArgs t -> Layer a
    liftLayer :: Layer a -> a

class OrderedLayeredQuantum a tag => TestsOrderedLayeredQuantum a test tag where
    orderedTest :: test tag -> Layer a

class (Tests a test tag, OrderedQuantum a tag) => TestsOrderedQuantum a test tag where

-- | Notation for predicate
subjectTo :: Quantum a t => Predicate t -> (Predicate t -> a) -> a
subjectTo pt f = f pt
