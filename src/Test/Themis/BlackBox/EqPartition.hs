module Test.Themis.BlackBox.EqPartition where

{-
Represents equivalance partitionining and automatic
generation of assertions and test cases, from the
defined partitions
-}

import Data.List (nub, sortBy)

import Test.Themis.Assertion
import Test.Themis.Test


-- | Represents a value in a named partition
data Partition a = Partition String a
  deriving Eq

-- | Template functions for the partition
partition f (Partition name value) = f name value

-- | Template function for the partition with flipped paramaters
withPartition c f = partition f c

-- | A parameter that can be splitted into different partitions
-- one parameter consists of negative and positive test cases
data Parameter a = Parameter String [Partition a] [Partition a]
  deriving Eq

-- | Template function for the parameter
parameter f (Parameter name valid invalid) = f name valid invalid

-- | Template function for the paramter with flipped arguments
withParameter p f = parameter f p

{-
* Combine the test cases and sort them by frequency of occurrence (typ- Rules for test case restriction
ical usage profile). Prioritize the test cases in this order. That way only
the relevant test cases (or combinations appearing often) are tested.
* Test cases including boundary values or boundary value combinations
are preferred.
* Combine every representative of one equivalence class with every
representative of the other equivalence classes (i.e., pairwise combinations6
instead of complete combinations).
* Ensure that every representative of an equivalence class appears in at
least one test case. This is a minimum criterion.
* Representatives of invalid equivalence classes should not be combined
with representatives of other invalid equivalence classes.
-}

-- | Zip together two parameters into a tuple, combining valid and invalid partitions together
zipParams :: (Eq a, Eq b) => Parameter a -> Parameter b -> Parameter (a,b)
zipParams p1 p2 = withParameter p1 $ \p1name p1valids p1invalids ->
  withParameter p2 $ \p2name p2valids p2invalids ->
    let valids = map (\(c1,c2) -> withPartition c1 (\c1name c1value ->
                                  withPartition c2 (\c2name c2value ->
                                    Partition (concatNames c1name c2name) (c1value, c2value))))
                     (longerPartitions p1valids p2valids)
        invalids1 = [ Partition (concatNames vname iname) (valid, invalid) | (Partition vname valid) <- p1valids, (Partition iname invalid) <- p2invalids ]
        invalids2 = [ Partition (concatNames iname vname) (invalid, valid) | (Partition vname valid) <- p2valids, (Partition iname invalid) <- p1invalids ]
        names = concat [p1name, ", ", p2name]
    in Parameter names (nub valids) (nub (invalids1 ++ invalids2))
  where
    concatNames n1 n2 = concat [n1, ", ", n2]

    longerPartitions xs ys =
      let as = cycle xs `zip` ys
          bs = xs `zip` cycle ys
      in if (length as < length bs)
           then bs
           else as

-- Prioritization of the data that can the partitions can be ordered
newtype Prio a = Prio { unPrio :: a -> a -> Ordering }

zipParamsWithPrio :: (Eq a, Eq b) => Prio (a,b) -> Parameter a -> Parameter b -> Parameter (a,b)
zipParamsWithPrio (Prio prio) pa pb =
  let (Parameter name valids invalids) = zipParams pa pb
  in Parameter name (sortBy prio' valids) (sortBy prio' invalids)
  where
    prio' p1 p2 = withPartition p1 $ \_p1name p1val ->
                  withPartition p2 $ \_p2name p2val ->
                    prio p1val p2val

-- | Converts a reference function and parameter which represents an equivalence
-- classes. The result is a list of function that gets the actual implementation
-- of the desired function and produces an assertion list.
parameterToAssertions :: (a -> b) -> Parameter a -> ((a -> b) -> [Assertion b])
parameterToAssertions ref param underTest = withParameter param $ \name valids invalids ->
  map (\(Partition name part) -> Equals (ref part) (underTest part) (name ++ " valid")) valids ++
  map (\(Partition name part) -> Error (underTest part) (name ++ " invalid")) invalids

-- | Converts a valid partitions from a given parameter, using the given
-- Test Case creator function and produces a list of the given test cases
validParameterToTests :: (String -> a -> Test b) -> Parameter a -> [Test b]
validParameterToTests f = parameter $ \name valids _invalids ->
  map (\(Partition pname part) -> f (concat [name, " ", pname]) part) valids
