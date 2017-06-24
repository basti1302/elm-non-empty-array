module Array.NonEmpty
    exposing
        ( NonEmptyArray
        , append
        , filter
        , fromArray
        , fromList
        , get
        , getFirst
        , length
        , map
        , push
        , set
        , singleton
        )

import Array.Hamt as Array exposing (Array)


type NonEmptyArray a
    = NEA a (Array a)


singleton : a -> NonEmptyArray a
singleton first =
    NEA first Array.empty


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
