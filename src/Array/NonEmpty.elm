module Array.NonEmpty exposing
    ( NonEmptyArray
    , fromElement, initialize, repeat, fromList, fromArray
    , length, get, getFirst
    , set, update, push, append, slice, removeAt, removeAtSafe
    , selectedIndex, setSelectedIndex, setSelectedIndexAndReport, getSelected, updateSelected, mapSelected, indexedMapSelected
    , toArray, toList, toIndexedList
    , foldl, foldr, filter, map, indexedMap
    )

{-| An array that always contains at least one element.

Additionaly, it can track a currently selected index, which is guaranteed to point to an existing element in the array.

Most functions (like `map`) keep the currently selected index untouched, other functions (like `filter` or `slice`) discard the currently selected and reset it to zero. If not otherwise mentioned, the selected index is kept; if it is discarded, the function documentation states so explicitly.


# Non Empty Array

@docs NonEmptyArray


# Creation

@docs fromElement, initialize, repeat, fromList, fromArray


# Query

@docs length, get, getFirst


# Manipulate

@docs set, update, push, append, slice, removeAt, removeAtSafe


# Selected Index

@docs selectedIndex, setSelectedIndex, setSelectedIndexAndReport, getSelected, updateSelected, mapSelected, indexedMapSelected


# Conversions

@docs toArray, toList, toIndexedList


# Transform

@docs foldl, foldr, filter, map, indexedMap, mapSelected, indexedMapSelected


# Display

@docs toString

-}

import Array exposing (Array)


{-| An array that is known, at compile-time, to be non empty. It always has at
least one element.
-}
type NonEmptyArray a
    = NEA a Int (Array a)


{-| Return the length of the array.
-}
length : NonEmptyArray a -> Int
length (NEA first _ rest) =
    Array.length rest + 1


{-| Creates a new, non empty array with a single element. The selected index of
the resulting array will be 0.
-}
fromElement : a -> NonEmptyArray a
fromElement first =
    NEA first 0 Array.empty


{-| Initialize an array. `initialize n f` creates an array of length `n` with
the element at index `i` initialized to the result of `(f i)`.

When the requested number of elements is smaller than one (0 or negative), the
returned array will still contain one element, `f 0`.

    Just (initialize 4 identity) --> fromList [0, 1, 2, 3]

    Just (initialize 4 (\n -> n * n)) --> fromList [0, 1, 4, 9]

    Just (initialize 4 (always 0)) --> fromList [0, 0, 0, 0]

    Just (initialize 0 identity) --> fromList [0]

The selected index of the resulting array will be 0.

-}
initialize : Int -> (Int -> a) -> NonEmptyArray a
initialize howMany generator =
    NEA
        (generator 0)
        0
        (Array.initialize
            (howMany - 1)
            ((+) 1 >> generator)
        )


{-| Creates an array with a given length, filled with a default element.

When the requested number of elements is smaller than one (0 or negative),
this function will still return a non empty array with one element.

    Just (repeat 5 0) --> fromList [0, 0, 0, 0, 0]

    Just (repeat 3 "cat") --> fromList ["cat", "cat", "cat"]

    Just (repeat 0 "frog") --> fromList ["frog"]

Notice that `repeat 3 x` is the same as `initialize 3 (always x)`.

The selected index of the resulting array will be 0.

-}
repeat : Int -> a -> NonEmptyArray a
repeat howMany element =
    NEA element 0 (Array.repeat (howMany - 1) element)


{-| Create a `NonEmptyArray` from an `Array`.

Given an empty array, this function will return `Nothing`.
Given an array with at least one element, this function will return `Just` that
array, converted to a `NonEmptyArray`.

The selected index of the resulting array will be 0.

-}
fromArray : Array a -> Maybe (NonEmptyArray a)
fromArray array =
    let
        maybeFirst =
            Array.get 0 array

        length_ =
            Array.length array
    in
    case maybeFirst of
        Just first ->
            Array.slice 1 (length_ + 1) array
                |> NEA first 0
                |> Just

        Nothing ->
            Nothing


{-| Create a `NonEmptyArray` from a `List`.

Given an empty list, this function will return `Nothing`.
Given a list with at least one element, this function will return `Just` that
list, converted to a `NonEmptyArray`.

The selected index of the resulting array will be 0.

-}
fromList : List a -> Maybe (NonEmptyArray a)
fromList =
    Array.fromList >> fromArray


{-| Return `Just` the element at the index or `Nothing` if the index is out of
range.

    get 0 (initialize 3 identity) --> Just 0

    get 2 (initialize 3 identity) --> Just 2

    get 5 (initialize 3 identity) --> Nothing

    get -1 (initialize 3 identity) -->  Nothing

Notice that in contrast to mgold/elm-nonempty-list, where `head` and `tail` do
not return Maybes, this function still necessarily returns a Maybe, because the
given index can still be out of range. But see `getFirst` for a function that is
known at compile time to return a value.

-}
get : Int -> NonEmptyArray a -> Maybe a
get index (NEA first _ rest) =
    if index == 0 then
        Just first

    else
        Array.get (index - 1) rest


{-| Return the first element of a NonEmptyArray.

    getFirst (initialize 3 identity) --> 0

-}
getFirst : NonEmptyArray a -> a
getFirst (NEA first _ rest) =
    first


{-| Return the element at the selected index.

    initialize 3 identity
        |> setSelectedIndex 1
        |> getSelected
    --> 1

-}
getSelected : NonEmptyArray a -> a
getSelected nea =
    let
        (NEA first selected _) =
            nea

        elem =
            get selected nea
    in
    case elem of
        Just e ->
            e

        Nothing ->
            {- This should never happen, see above.
               But to avoid Debug.crash, we return first
            -}
            first


{-| Return the selected index.

    repeat 5 "x"
        |> setSelectedIndex 3
        |> selectedIndex
    --> 3

-}
selectedIndex : NonEmptyArray a -> Int
selectedIndex (NEA _ selected _) =
    selected


{-| Set the element at a particular index. Returns an updated array.
If the index is out of range, the array is unaltered.

    set 0 1302 (fromElement 42) --> fromElement 1302

-}
set : Int -> a -> NonEmptyArray a -> NonEmptyArray a
set index element (NEA first selected rest) =
    if index == 0 then
        NEA element selected rest

    else
        NEA first selected (Array.set (index - 1) element rest)


{-| Update the element at the given index using a function. Returns the array
unchanged if the index is out of bounds.

    Just <| update 1 ((+) 10) (initialize 3 identity)
    --> fromList [0, 11, 2]

    Just <| update 4 ((+) 10) (initialize 3 identity)
    --> fromList [0, 1, 2]

    Just <| update -1 ((+) 10) (initialize 3 identity)
    --> fromList [0, 1, 2]

This is basically Array.Extra.update from elm-community/array-extra, for
NonEmptyArray.

-}
update : Int -> (a -> a) -> NonEmptyArray a -> NonEmptyArray a
update index function nea =
    let
        element =
            get index nea
    in
    case element of
        Nothing ->
            nea

        Just elem ->
            set index (function elem) nea


{-| Update the element at the current selected index using a function.

    Just <| updateSelected ((+) 10) (initialize 3 identity)
    --> fromList [10, 1, 2]

-}
updateSelected : (a -> a) -> NonEmptyArray a -> NonEmptyArray a
updateSelected function nea =
    let
        element =
            getSelected nea

        index =
            selectedIndex nea
    in
    set index (function element) nea


{-| Set the selected index for the given array.
If the index is out of range, the array is unaltered (that is, the old selected
index is kept).

    initialize 5 identity
        |> setSelectedIndex 2
        |> getSelected
    --> 2

    initialize 5 identity
        |> setSelectedIndex 7
        |> getSelected
    --> 0

-}
setSelectedIndex : Int -> NonEmptyArray a -> NonEmptyArray a
setSelectedIndex selected =
    setSelectedIndexAndReport selected >> Tuple.first


{-| Set the selected index for the given array and reports if an index change
actually happened. If the index is out of range, the array is unaltered (that
is, the old selected index is kept).

The second value of the returned tuple is `True` if and only if the index
actually changed due to the operation. `False` is returned if you pass in an
index that is out of range, or when you pass in the index that is currently
selected anyway.

    initialize 5 identity
    |> setSelectedIndexAndReport 2
    |> Tuple.second
    --> True

    initialize 5 identity
    |> setSelectedIndexAndReport 7
    |> Tuple.second
    --> False

    initialize 5 identity
    |> setSelectedIndexAndReport 0
    |> Tuple.second
    --> False

-}
setSelectedIndexAndReport : Int -> NonEmptyArray a -> ( NonEmptyArray a, Bool )
setSelectedIndexAndReport selected nea =
    let
        (NEA first oldSelected rest) =
            nea
    in
    if selected >= 0 && selected < length nea then
        ( NEA first selected rest, selected /= oldSelected )

    else
        ( NEA first oldSelected rest, False )


{-| Push an element onto the end of an array.

    Just (push 2 (fromElement 1)) --> fromList [1, 2]

-}
push : a -> NonEmptyArray a -> NonEmptyArray a
push element (NEA first selected rest) =
    NEA first selected (Array.push element rest)


{-| Converts the NonEmptyArray into a standard array.

    import Array.Hamt as Array

    fromArray (Array.fromList [1, 2])
        |> Maybe.map toArray
    --> Just (Array.fromList [1, 2])

-}
toArray : NonEmptyArray a -> Array a
toArray (NEA first _ rest) =
    Array.append
        (Array.repeat 1 first)
        rest


{-| Create a list of elements from an array.

    fromList [3, 5, 8]
        |> Maybe.map toList
    --> Just [3, 5, 8]

-}
toList : NonEmptyArray a -> List a
toList (NEA first _ rest) =
    first :: Array.toList rest


{-| Create an indexed list from an array. Each element of the array will be
paired with its index.

    fromList ["cat", "dog"]
        |> Maybe.map toIndexedList
    --> Just [(0, "cat"), (1, "dog")]

-}
toIndexedList : NonEmptyArray a -> List ( Int, a )
toIndexedList (NEA first _ rest) =
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
foldl f init (NEA first _ rest) =
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

The selected index will be set to 0 for the resulting array.

-}
filter : (a -> Bool) -> NonEmptyArray a -> Maybe (NonEmptyArray a)
filter function (NEA first _ rest) =
    let
        retainFirst =
            function first

        filteredRest =
            Array.filter
                function
                rest
    in
    if retainFirst then
        Just (NEA first 0 filteredRest)

    else
        fromArray filteredRest


{-| Apply a function to every element in an array.

    map Basics.toString (repeat 5 1) --> repeat 5 "1"

-}
map : (a -> b) -> NonEmptyArray a -> NonEmptyArray b
map function (NEA first selected rest) =
    let
        newFirst =
            function first

        newRest =
            Array.map function rest
    in
    NEA newFirst selected newRest


{-| Apply a function to every element with its index as first argument.

    Just (indexedMap (*) (repeat 5 2)) --> fromList [0, 2, 4, 6, 8]

-}
indexedMap : (Int -> a -> b) -> NonEmptyArray a -> NonEmptyArray b
indexedMap function (NEA first selected rest) =
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
    NEA newFirst selected newRest


{-| Apply a function to every element in an array. The first argument to that
function is a Boolean that is True for the selected element. This makes it
easier to treat the select element different from other elements.

    Just <|
      (repeat 5 1
        |> mapSelected (\isSelected ->
          if isSelected then
            (*) 2
          else
            Basics.identity
        ))
      --> fromList [2, 1, 1, 1, 1]

-}
mapSelected : (Bool -> a -> b) -> NonEmptyArray a -> NonEmptyArray b
mapSelected function array =
    let
        innerFn index =
            function (index == selectedIndex array)
    in
    indexedMap innerFn array


{-| Apply a function to every element in an array. The first argument to that
function is a Boolean that is True for the selected element. This makes it
easier to treat the select element different from other elements. The second
argument is the element's index.

    Just <|
      (repeat 5 1
        |> indexedMapSelected (\isSelected ->
          if isSelected then
            (*)
          else
            (+)
        ))
      --> fromList [0, 2, 3, 4, 5]

-}
indexedMapSelected : (Bool -> Int -> a -> b) -> NonEmptyArray a -> NonEmptyArray b
indexedMapSelected function array =
    let
        innerFn index =
            function (index == selectedIndex array) index
    in
    indexedMap innerFn array


{-| Append two arrays to a new one.

    Just (append (repeat 2 42) (repeat 3 81)) --> fromList [42, 42, 81, 81, 81]

The selected index of the resulting array will be the selected index of the
first argument.

-}
append : NonEmptyArray a -> NonEmptyArray a -> NonEmptyArray a
append (NEA first1 selected1 rest1) (NEA first2 _ rest2) =
    let
        newRest =
            rest1
                |> Array.push first2
                |> (\a -> Array.append a rest2)
    in
    NEA first1 selected1 newRest


{-| Get a sub-section of an array: `(slice start end array)`. The `start` is a
zero-based index where we will start our slice. The `end` is a zero-based index
that indicates the end of the slice. The slice extracts up to but not including
`end`. If the resulting slice would be empty, Nothing is returned.

    slice 0 3 (initialize 5 identity) --> fromList [0, 1, 2]

    slice 1 4 (initialize 5 identity) --> fromList [1, 2, 3]

    slice 2 2 (initialize 5 identity) --> Nothing

Both the `start` and `end` indexes can be negative, indicating an offset from
the end of the array.

    slice 1 -1 (initialize 5 identity) --> fromList [1, 2, 3]

    slice -2 5 (initialize 5 identity) --> fromList [3, 4]

This makes it pretty easy to `pop` the last element off of an array:
`slice 0 -1 array`

The selected index will be set to 0 for the resulting array.

-}
slice : Int -> Int -> NonEmptyArray a -> Maybe (NonEmptyArray a)
slice startIndex endIndex nea =
    let
        (NEA first _ rest) =
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
                            {- This should never happen, see above.
                               But to avoid Debug.crash, we return first
                            -}
                            first

            newRest =
                if e > s + 1 then
                    Array.slice s (e - 1) rest

                else
                    Array.empty
        in
        Just (NEA newFirst 0 newRest)


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


{-| Removes the element at the given index.

    removeAt 1 (initialize 4 identity) --> fromList [0, 2, 3]

If the index is out of range, the array is returned unchanged. If there is only
one element in the array and index 0 is passed, Nothing will be returned.
Otherwise, the element at the given index is removed.

If the removed element was the selected index, the selected index will be set
to the previous element, or 0 if there is no previous element. Otherwise it will
be updated so that the same element as before is selected.

-}
removeAt : Int -> NonEmptyArray a -> Maybe (NonEmptyArray a)
removeAt index nea =
    if index < 0 || index > length nea then
        Just nea

    else
        let
            (NEA first selected rest) =
                nea
        in
        case ( index, Array.isEmpty rest ) of
            ( 0, True ) ->
                Nothing

            ( 0, False ) ->
                fromArray rest
                    |> Maybe.map (setSelectedIndex (selected - 1))

            otherwise ->
                let
                    updatedRest =
                        arrayExtraRemoveAt (index - 1) rest

                    updatedSelectedIndex =
                        case compare index selected of
                            EQ ->
                                max (selected - 1) 0

                            LT ->
                                selected - 1

                            GT ->
                                selected
                in
                Just (NEA first updatedSelectedIndex updatedRest)


{-| Removes the element at the given index.

    Just (removeAtSafe 1 (initialize 4 identity)) --> fromList [0, 2, 3]

    Just (removeAtSafe 0 (initialize 1 identity)) --> fromList [0]

If the index is out of range, the array is returned unchanged. If there is only
one element left in the array, the array is also returned unchanged.
Otherwise, the element at the given index is removed.

If the removed element was the selected index, the selected index will be set
to the previous element, or 0 if there is no previous element. Otherwise it will
be updated so that the same element as before is selected.

-}
removeAtSafe : Int -> NonEmptyArray a -> NonEmptyArray a
removeAtSafe index nea =
    if index < 0 || index > length nea then
        nea

    else
        let
            (NEA first selected rest) =
                nea
        in
        case ( index, Array.isEmpty rest ) of
            ( 0, True ) ->
                nea

            ( 0, False ) ->
                let
                    newFirst =
                        let
                            maybeNewFirst =
                                Array.get 0 rest
                        in
                        case maybeNewFirst of
                            Just elem ->
                                elem

                            Nothing ->
                                {- This should never happen, see above.
                                   But to avoid Debug.crash, we return first
                                -}
                                first

                    restLength =
                        Array.length rest

                    newSelected =
                        max (selected - 1) 0

                    newRest =
                        Array.slice 1 restLength rest
                in
                NEA newFirst newSelected newRest

            otherwise ->
                let
                    updatedRest =
                        arrayExtraRemoveAt (index - 1) rest

                    updatedSelectedIndex =
                        case compare index selected of
                            EQ ->
                                max (selected - 1) 0

                            LT ->
                                selected - 1

                            GT ->
                                selected
                in
                NEA first updatedSelectedIndex updatedRest


{-| Remove the element at the given index
-}
arrayExtraRemoveAt : Int -> Array a -> Array a
arrayExtraRemoveAt index xs =
    -- Stolen from
    -- https://github.com/elm-community/array-extra/blob/1.0.2/src/Array/Extra.elm
    -- (where this function is known as `removeAt`.
    let
        ( xs0, xs1 ) =
            arrayExtraSplitAt index xs

        len1 =
            Array.length xs1
    in
    if len1 == 0 then
        xs0

    else
        Array.append xs0 (Array.slice 1 len1 xs1)


{-| Split an array into two arrays, the first ending at and the second starting at the given index
-}
arrayExtraSplitAt : Int -> Array a -> ( Array a, Array a )
arrayExtraSplitAt index xs =
    -- Stolen from
    -- https://github.com/elm-community/array-extra/blob/1.0.2/src/Array/Extra.elm
    -- (where this function is known as `splitAt`.
    let
        len =
            Array.length xs
    in
    case ( index > 0, index < len ) of
        ( True, True ) ->
            ( Array.slice 0 index xs, Array.slice index len xs )

        ( True, False ) ->
            ( xs, Array.empty )

        ( False, True ) ->
            ( Array.empty, xs )

        ( False, False ) ->
            ( Array.empty, Array.empty )
