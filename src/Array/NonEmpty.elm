module Array.NonEmpty
    exposing
        ( NonEmptyArray
        , append
        , filter
        , foldl
        , foldr
        , fromArray
        , fromList
        , get
        , getFirst
        , initialize
        , length
        , map
        , push
        , repeat
        , set
        , singleton
        , toArray
        , toIndexedList
        , toList
        )

import Array.Hamt as Array exposing (Array)


type NonEmptyArray a
    = NEA a (Array a)


singleton : a -> NonEmptyArray a
singleton first =
    NEA first Array.empty


{-| When the requested number of elements is smaller than one (0 or negative),
this function will still return a non empty array with one element.
-}
repeat : Int -> a -> NonEmptyArray a
repeat howMany element =
    NEA element (Array.repeat (howMany - 1) element)


initialize : Int -> (Int -> a) -> NonEmptyArray a
initialize howMany generator =
    NEA
        (generator 0)
        (Array.initialize
            (howMany - 1)
            ((+) 1 >> generator)
        )


fromArray : Array a -> Maybe (NonEmptyArray a)
fromArray array =
    let
        maybeFirst =
            Array.get 0 array

        length =
            Array.length array
    in
    case maybeFirst of
        Just first ->
            Array.slice 1 (length + 1) array
                |> NEA first
                |> Just

        Nothing ->
            Nothing


fromList : List a -> Maybe (NonEmptyArray a)
fromList =
    Array.fromList >> fromArray


push : a -> NonEmptyArray a -> NonEmptyArray a
push element (NEA first rest) =
    NEA first (Array.push element rest)


append : NonEmptyArray a -> NonEmptyArray a -> NonEmptyArray a
append (NEA first1 rest1) (NEA first2 rest2) =
    let
        newRest =
            rest1
                |> Array.push first2
                |> flip Array.append rest2
    in
    NEA first1 newRest


get : Int -> NonEmptyArray a -> Maybe a
get index (NEA first rest) =
    if index == 0 then
        Just first
    else
        Array.get (index - 1) rest


getFirst : NonEmptyArray a -> a
getFirst (NEA first rest) =
    first


set : Int -> a -> NonEmptyArray a -> NonEmptyArray a
set index element (NEA first rest) =
    if index == 0 then
        NEA element rest
    else
        NEA first (Array.set (index - 1) element rest)


toArray : NonEmptyArray a -> Array a
toArray (NEA first rest) =
    Array.append
        (Array.repeat 1 first)
        rest


toList : NonEmptyArray a -> List a
toList (NEA first rest) =
    first :: Array.toList rest


toIndexedList : NonEmptyArray a -> List ( Int, a )
toIndexedList (NEA first rest) =
    let
        indexedList =
            Array.toIndexedList rest

        fixIndex ( index, element ) =
            ( index + 1, element )

        fixedIndexes =
            List.map fixIndex indexedList
    in
    ( 0, first ) :: fixedIndexes


length : NonEmptyArray a -> Int
length (NEA first rest) =
    Array.length rest + 1


map : (a -> b) -> NonEmptyArray a -> NonEmptyArray b
map function (NEA first rest) =
    let
        newFirst =
            function first

        newRest =
            Array.map function rest
    in
    NEA newFirst newRest


filter : (a -> Bool) -> NonEmptyArray a -> Maybe (NonEmptyArray a)
filter function (NEA first rest) =
    let
        retainFirst =
            function first

        filteredRest =
            Array.filter
                function
                rest
    in
    if retainFirst then
        Just (NEA first filteredRest)
    else
        fromArray filteredRest


foldl : (a -> b -> b) -> b -> NonEmptyArray a -> b
foldl f init (NEA first rest) =
    let
        init2 =
            f first init
    in
    Array.foldl f init2 rest


foldr : (a -> b -> b) -> b -> NonEmptyArray a -> b
foldr f init nea =
    let
        array =
            toArray nea
    in
    Array.foldr f init array
