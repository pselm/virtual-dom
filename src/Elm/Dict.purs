module Elm.Dict 
    ( module Virtual
    , Dict, get, remove, update
    , intersect, diff, filter, partition
    , map, foldl, foldr
    ) where


-- For re-export

import Data.Map
    ( empty, isEmpty, size
    , member
    , singleton, insert 
    , toList, fromList
    , keys, values
    , union
    ) as Virtual


-- Internal

import Prelude (class Ord, flip)
import Data.Map (Map, lookup, alter, delete, member, insert, empty, toList, fromList)
import Elm.Maybe (Maybe)
import Elm.List (List)
import Data.Tuple (Tuple(..))


type Dict = Map


-- TODO: It feels as though there ought to be a better implementation available
-- of various functions below, perhaps via bifunctor, profunctor or traversable?


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

    animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]

    get "Tom"   animals == Just Cat
    get "Jerry" animals == Just Mouse
    get "Spike" animals == Nothing

-}
get :: forall k v. (Ord k) => k -> Dict k v -> Maybe v
get = lookup


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made. -}
remove :: forall k v. (Ord k) => k -> Dict k v -> Dict k v
remove = delete


{-| Update the value of a dictionary for a specific key with a given function. -}
update :: forall k v. (Ord k) => k -> (Maybe v -> Maybe v) -> Dict k v -> Dict k v
update = flip alter


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.
-}
intersect :: forall k v. (Ord k) => Dict k v -> Dict k v -> Dict k v
intersect t1 t2 =
    filter (\k _ -> member k t2) t1


{-| Keep a key-value pair when its key does not appear in the second dictionary.
-}
diff :: forall k v. (Ord k) => Dict k v -> Dict k v -> Dict k v
diff t1 t2 =
    foldl (\k v t -> remove k t) t1 t2


{-| Keep a key-value pair when it satisfies a predicate. -}
filter :: forall k v. (Ord k) => (k -> v -> Boolean) -> Dict k v -> Dict k v
filter predicate =
    let
        add key value dict =
            if predicate key value
                then insert key value dict
                else dict

    in
        foldl add empty


{-| Partition a dictionary according to a predicate. The first dictionary
contains all key-value pairs which satisfy the predicate, and the second
contains the rest.
-}
partition :: forall k v. (Ord k) => (k -> v -> Boolean) -> Dict k v -> {trues :: Dict k v, falses :: Dict k v}
partition predicate dict =
  let
      add key value pair =
          if predicate key value
             then pair { trues  = insert key value pair.trues }
             else pair { falses = insert key value pair.falses } 

   in
       foldl add {trues: empty, falses: empty} dict


{-| Apply a function to all values in a dictionary. -}
map :: forall k a b. (Ord k) => (k -> a -> b) -> Dict k a -> Dict k b
map f dict =
    -- There has got to be a way to do this without constructing an
    -- intermediate list.
    let
        tuples :: List (Tuple k a)
        tuples = toList dict

        mapper :: Tuple k a -> Tuple k b
        mapper (Tuple k v) = Tuple k (f k v)

        mapped :: List (Tuple k b)
        mapped = Prelude.map mapper tuples

    in
        fromList mapped


{-| Fold over the key-value pairs in a dictionary, in order from lowest
key to highest key.
-}
foldl :: forall k v b. (Ord k) => (k -> v -> b -> b) -> b -> Dict k v -> b
foldl f acc dict =
    -- There has got to be a way to do this without constructing an
    -- intermediate list.
   let
        tuples :: List (Tuple k v)
        tuples = toList dict

        folder :: Tuple k v -> b -> b
        folder (Tuple k v) = f k v

    in
        Elm.List.foldl folder acc tuples


{-| Fold over the key-value pairs in a dictionary, in order from highest
key to lowest key.
-}
foldr :: forall k v b. (Ord k) => (k -> v -> b -> b) -> b -> Dict k v -> b
foldr f acc dict =
    -- There has got to be a way to do this without constructing an
    -- intermediate list.
    let
        tuples :: List (Tuple k v)
        tuples = toList dict

        folder :: Tuple k v -> b -> b
        folder (Tuple k v) = f k v

    in
        Elm.List.foldr folder acc tuples
