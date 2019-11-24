module Lib
    ( someFunc
    , validCycles
    , choice
    ) where

import qualified Data.Bimap as Bimap
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import System.Random

someFunc :: IO ()
someFunc = putStrLn "Hello, world!"

type Participant = String
type Group = String
type ParticipantGroupings = Map.Map Participant Group
type Cycle = [Participant]
type SingleAssignment = (Participant, Participant)
type Assignment = Bimap.Bimap Participant Participant

-- Returns true iff each key is not present on their respective side.
neitherPresent :: (Ord a, Ord b) => (a, b) -> Bimap.Bimap a b -> Bool
neitherPresent (a, b) bimap = a `Bimap.notMember` bimap && b `Bimap.notMemberR` bimap

-- Returns true iff the giver and receiver are not in the same group.
respectsGroupings :: SingleAssignment -> ParticipantGroupings -> Bool
respectsGroupings (giver, receiver) g =
  fromMaybe False $ (/=) <$> giver `Map.lookup` g <*> receiver `Map.lookup` g

-- Returns the list of neighboring pairs in a list.
-- e.g. [1, 2, 3] -> [(1, 2), (2, 3), (3, 1)]
cyclePairs :: [a] -> [(a, a)]
cyclePairs xs = zip xs (tail xs ++ [head xs])

-- Returns true iff the given cycle forms a valid Secret Santa assignment for the given groupings.
isValidCycle :: ParticipantGroupings -> Cycle -> Bool
isValidCycle g = isValidCycle' . foldr (flip isValidSingleAssignment) (Just Bimap.empty) . cyclePairs
  where
    isValidCycle' :: Maybe Assignment -> Bool
    isValidCycle' Nothing  = False
    isValidCycle' (Just a) = Bimap.size a == Map.size g
    isValidSingleAssignment :: Maybe Assignment -> SingleAssignment -> Maybe Assignment
    isValidSingleAssignment Nothing  _  = Nothing
    isValidSingleAssignment (Just a) (giver, receiver) =
      if isValidSingleAssignment' a (giver, receiver)
      then Just $ Bimap.insert giver receiver a
      else Nothing
    isValidSingleAssignment' :: Assignment -> SingleAssignment -> Bool
    isValidSingleAssignment' a sa = sa `neitherPresent` a && sa `respectsGroupings` g

-- Returns all valid cycles given grouping constraints.
validCycles :: ParticipantGroupings -> [Cycle]
validCycles g = filter (isValidCycle g) $ permutations $ Map.keys g

-- Sample a value from a list uniformly at random.
choice :: (RandomGen g) => [a] -> g -> (Maybe a, g)
choice []     g = (Nothing, g)
choice (x:xs) g = let (z, _, g') = foldl' choice' (x, 1, g) xs in (Just z, g')
  where
    choice' :: (RandomGen g) => (a, Double, g) -> a -> (a, Double, g)
    choice' (x, c, g) y =
      let (r, g') = random g
          c' = c + 1
      in if r <= (1 / c') then (y, c', g') else (x, c', g')
