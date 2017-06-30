module Array.NonEmpty
    exposing
        ( NonEmptyArray
        , append
        , filter
        , foldl
        , foldr
        , fromArray
        , fromElement
        , fromList
        , get
        , getFirst
        , indexedMap
        , initialize
        , length
        , map
        , push
        , repeat
        , set
        , slice
        , toArray
        , toIndexedList
        , toList
        , toString
        )

{-| An array that always contains at least one element.


# Non Empty Array

@docs NonEmptyArray


# Creation

@docs fromElement, initialize, repeat, fromList, fromArray


# Query

@docs length, get, getFirst


# Manipulate

@docs set, push, append, slice


# Conversions

@docs toArray, toList, toIndexedList


# Transform

@docs foldl, foldr, filter, map, indexedMap


# Display

@docs toString

-}

import Array.Hamt as Array exposing (Array)


{-| An array that is known, at compile-time, to be non empty. It always has at
least one element.
-}
type NonEmptyArray a
    = NEA a (Array a)


{-| Return the length of the array.
-}
length : NonEmptyArray a -> Int
length (NEA first rest) =
    Array.length rest + 1


{-| Creates a new, non empty array with a single element.
-}
fromElement : a -> NonEmptyArray a
fromElement first =
    NEA first Array.empty


{-| Initialize an array. `initialize n f` creates an array of length `n` with
the element at index `i` initialized to the result of `(f i)`.

When the requested number of elements is smaller than one (0 or negative), the
returned array will still contain one element, `f 0`.

    Just (initialize 4 identity)    --> fromList [0, 1, 2, 3]

    Just (initialize 4 (\n -> n*n)) --> fromList [0, 1, 4, 9]

    Just (initialize 4 (always 0))  --> fromList [0, 0, 0, 0]

    Just (initialize 0 identity)  --> fromList [0]

-}
initialize : Int -> (Int -> a) -> NonEmptyArray a
initialize howMany generator =
    NEA
        (generator 0)
        (Array.initialize
            (howMany - 1)
            ((+) 1 >> generator)
        )


{-| Creates an array with a given length, filled with a default element.

When the requested number of elements is smaller than one (0 or negative),
this function will still return a non empty array with one element.

    Just (repeat 5 0)     --> fromList [0, 0, 0, 0, 0]

    Just (repeat 3 "cat") --> fromList ["cat", "cat", "cat"]

    Just (repeat 0 "frog") --> fromList ["frog"]

Notice that `repeat 3 x` is the same as `initialize 3 (always x)`.

-}
repeat : Int -> a -> NonEmptyArray a
repeat howMany element =
    NEA element (Array.repeat (howMany - 1) element)


{-| Create a `NonEmptyArray` from an `Array`.

Given an empty array, this function will return `Nothing`.
Given an array with at least one element, this function will return `Just` that
array, converted to a `NonEmptyArray`.

-}
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


{-| Create a `NonEmptyArray` from a `List`.

Given an empty list, this function will return `Nothing`.
Given a list with at least one element, this function will return `Just` that
list, converted to a `NonEmptyArray`.

-}
fromList : List a -> Maybe (NonEmptyArray a)
fromList =
    Array.fromList >> fromArray


{-| Return a representation of the NonEmptyArray as a string.

    import Array.NonEmpty as NEA

    NEA.toString <| (initialize 3 identity) --> "NonEmptyArray [0,1,2]"

-}
toString : NonEmptyArray a -> String
toString nea =
    let
        content =
            nea
                |> map Basics.toString
                |> toList
                |> String.join ","
    in
    "NonEmptyArray [" ++ content ++ "]"


{-| Return `Just` the element at the index or `Nothing` if the index is out of
range.

    get  0 (initialize 3 identity) --> Just 0

    get  2 (initialize 3 identity) --> Just 2

    get  5 (initialize 3 identity) --> Nothing

    get -1 (initialize 3 identity) -->  Nothing

Notice that in contrast to mgold/elm-nonempty-list, where `head` and `tail` do
not return Maybes, this function still necessarily returns a Maybe, because the
given index can still be out of range. But see `getFirst` for a function that is
known at compile time to return a value.

-}
get : Int -> NonEmptyArray a -> Maybe a
get index (NEA first rest) =
    if index == 0 then
        Just first
    else
        Array.get (index - 1) rest


{-| Return the first element of a NonEmptyArray.

    getFirst (initialize 3 identity) --> 0

-}
getFirst : NonEmptyArray a -> a
getFirst (NEA first rest) =
    first


{-| Set the element at a particular index. Returns an updated array.
If the index is out of range, the array is unaltered.

    set 0 1302 (fromElement 42) --> fromElement 1302

-}
set : Int -> a -> NonEmptyArray a -> NonEmptyArray a
set index element (NEA first rest) =
    if index == 0 then
        NEA element rest
    else
        NEA first (Array.set (index - 1) element rest)


{-| Push an element onto the end of an array.

    Just (push 2 (fromElement 1)) --> fromList [1, 2]

-}
push : a -> NonEmptyArray a -> NonEmptyArray a
push element (NEA first rest) =
    NEA first (Array.push element rest)


{-| Converts the NonEmptyArray into a standard array.

    import Array.Hamt as Array

    fromArray (Array.fromList [1, 2])
        |> Maybe.map toArray
    --> Just (Array.fromList [1, 2])

-}
toArray : NonEmptyArray a -> Array a
toArray (NEA first rest) =
    Array.append
        (Array.repeat 1 first)
        rest


{-| Create a list of elements from an array.

    fromList [3, 5, 8]
        |> Maybe.map toList
    --> Just [3, 5, 8]

-}
toList : NonEmptyArray a -> List a
toList (NEA first rest) =
    first :: Array.toList rest


{-| Create an indexed list from an array. Each element of the array will be
paired with its index.

    fromList ["cat", "dog"]
        |> Maybe.map toIndexedList
    --> Just [(0, "cat"), (1, "dog")]

-}
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


{-| Reduce an array from the right. Read `foldr` as fold from the right.

    foldr (+) 0 (repeat 3 5) --> 15

-}
foldr : (a -> b -> b) -> b -> NonEmptyArray a -> b
foldr f init nea =
    let
        array =
            toArray nea
    in
    Array.foldr f init array


{-| Reduce an array from the left. Read `foldl` as fold from the left.

    foldl (::) [] (initialize 3 identity) --> [2, 1, 0]

-}
foldl : (a -> b -> b) -> b -> NonEmptyArray a -> b
foldl f init (NEA first rest) =
    let
        init2 =
            f first init
    in
    Array.foldl f init2 rest


{-| Keep only elements that satisfy the predicate. If no elements remain,
Nothing is returned, otherwise Just the array of remaining elements.

    isEven : Int -> Bool
    isEven n =
        n % 2 == 0

    filter isEven (initialize 5 identity) --> fromList [0, 2, 4]

-}
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


{-| Apply a function to every element in an array.

    map Basics.toString (repeat 5 1) --> repeat 5 "1"

-}
map : (a -> b) -> NonEmptyArray a -> NonEmptyArray b
map function (NEA first rest) =
    let
        newFirst =
            function first

        newRest =
            Array.map function rest
    in
    NEA newFirst newRest


{-| Apply a function to every element with its index as first argument.

    Just (indexedMap (*) (repeat 5 2)) --> fromList [0, 2, 4, 6, 8]

-}
indexedMap : (Int -> a -> b) -> NonEmptyArray a -> NonEmptyArray b
indexedMap function (NEA first rest) =
    let
        newFirst =
            function 0 first

        newRest =
            Array.indexedMap
                (\idx ->
                    function (idx + 1)
                )
                rest
    in
    NEA newFirst newRest


{-| Append two arrays to a new one.

    Just (append (repeat 2 42) (repeat 3 81)) --> fromList [42, 42, 81, 81, 81]

-}
append : NonEmptyArray a -> NonEmptyArray a -> NonEmptyArray a
append (NEA first1 rest1) (NEA first2 rest2) =
    let
        newRest =
            rest1
                |> Array.push first2
                |> flip Array.append rest2
    in
    NEA first1 newRest


{-| Get a sub-section of an array: `(slice start end array)`. The `start` is a
zero-based index where we will start our slice. The `end` is a zero-based index
that indicates the end of the slice. The slice extracts up to but not including
`end`. If the resulting slice would be empty, Nothing is returned.

    slice 0 3 (initialize 5 identity) --> fromList [0, 1, 2]

    slice 1 4 (initialize 5 identity) --> fromList [1, 2, 3]

    slice 2 2 (initialize 5 identity) --> Nothing

Both the `start` and `end` indexes can be negative, indicating an offset from
the end of the array.

    slice  1 -1 (initialize 5 identity) --> fromList [1, 2, 3]

    slice -2  5 (initialize 5 identity) --> fromList [3, 4]

This makes it pretty easy to `pop` the last element off of an array:
`slice 0 -1 array`

-}
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
