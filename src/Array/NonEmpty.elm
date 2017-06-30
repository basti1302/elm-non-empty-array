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
        , slice
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


slice : Int -> Int -> NonEmptyArray a -> Maybe (NonEmptyArray a)
slice startIndex endIndex nea =
    let
        (NEA first rest) =
            nea

        l =
            length nea

        s =
            normalizeSliceIndex startIndex nea

        e =
            normalizeSliceIndex endIndex nea
    in
    -- We mimick the constraints used by Elm's core array (as of 0.19, that is,
    -- Array.Hamt for 0.18):
    -- start >= end    => empty array
    -- start >= length => empty array
    -- end = start + n for n >= 1 => n elements, starting at start
    if s >= e then
        Nothing
    else if s >= l then
        Nothing
    else
        -- At this point, the following conditions are true:
        -- * 0 <= s < length
        -- * 0 <= e <= length
        -- * e > s
        -- This means that the returned slice will have at least one element.
        let
            -- let's try to find the first element of the new NEA:
            newFirst =
                if s == 0 then
                    -- This is the easy case, for s == 0 the slice's first
                    -- element is the current first element.
                    first
                else
                    -- otherwise, we need to fetch the slice's first element
                    -- from the rest array. We know that the element at the
                    -- given index exists (due to the conditions mentioned
                    -- above), but we need to unwrap the Maybe returned by
                    -- Array.get.
                    let
                        maybeNewFirst =
                            Array.get (s - 1) rest
                    in
                    case maybeNewFirst of
                        Just elem ->
                            elem

                        Nothing ->
                            -- This really should never happen, see above.
                            Debug.crash "Expected an element but there is none."

            newRest =
                if e > s + 1 then
                    Array.slice s (e - 1) rest
                else
                    Array.empty
        in
        Just (NEA newFirst newRest)


{-| Normalizes an index given to slice.

If the given index is greater than the length of the array, it is normalized to
the length.

If the given index is negative, it is interpreted as an index from the rightside
of the array, that is, as (length - index). If this also results in a
negative index (that is, if (Basics.abs index > length)), then it is normalized
to 0.

-}
normalizeSliceIndex : Int -> NonEmptyArray a -> Int
normalizeSliceIndex index nea =
    let
        l =
            length nea

        fromRight =
            l + index
    in
    if index < 0 && fromRight < 0 then
        0
    else if index < 0 then
        fromRight
    else if index > l then
        l
    else
        index


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
