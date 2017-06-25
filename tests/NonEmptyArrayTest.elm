module NonEmptyArrayTest exposing (..)

import Array.Hamt as Array exposing (Array)
import Array.NonEmpty as NEA exposing (NonEmptyArray)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Random
import Test exposing (..)


suite : Test
suite =
    describe "The non empty array module"
        [ describe "singleton (array creation)"
            [ test "created as a singleton, it has length 1" <|
                \_ ->
                    let
                        nea =
                            NEA.singleton "a"
                    in
                    Expect.equal 1 (NEA.length nea)
            ]
        , describe "repeat (array creation)"
            [ fuzz (Fuzz.intRange 3 100) "repeat" <|
                \howMany ->
                    let
                        nea =
                            NEA.repeat howMany "Yo!"
                    in
                    expectAll
                        [ Expect.equal howMany (NEA.length nea)
                        , Expect.equal (Just "Yo!") (NEA.get 0 nea)
                        , Expect.equal (Just "Yo!") (NEA.get 1 nea)
                        , Expect.equal (Just "Yo!") (NEA.get 2 nea)
                        , Expect.equal Nothing (NEA.get howMany nea)
                        ]
            , fuzz (Fuzz.intRange -100 1) "repeat negative times or zero" <|
                \howMany ->
                    let
                        nea =
                            NEA.repeat howMany "Nope"
                    in
                    expectAll
                        [ Expect.equal 1 (NEA.length nea)
                        , Expect.equal (Just "Nope") (NEA.get 0 nea)
                        , Expect.equal Nothing (NEA.get 1 nea)
                        ]
            ]
        , describe "initialize (array creation)"
            [ fuzz (Fuzz.intRange 3 100) "initialize" <|
                \howMany ->
                    let
                        nea =
                            NEA.initialize howMany toString
                    in
                    expectAll
                        [ Expect.equal howMany (NEA.length nea)
                        , Expect.equal (Just "0") (NEA.get 0 nea)
                        , Expect.equal (Just "1") (NEA.get 1 nea)
                        , Expect.equal (Just "2") (NEA.get 2 nea)
                        , Expect.equal Nothing (NEA.get howMany nea)
                        ]
            , fuzz (Fuzz.intRange -100 1) "initialize with negative or zero number" <|
                \howMany ->
                    let
                        nea =
                            NEA.initialize howMany toString
                    in
                    expectAll
                        [ Expect.equal 1 (NEA.length nea)
                        , Expect.equal (Just "0") (NEA.get 0 nea)
                        , Expect.equal Nothing (NEA.get 1 nea)
                        ]
            ]
        , describe "fromArray (array creation)"
            [ test "created from an empty array, it is Nothing" <|
                \_ ->
                    let
                        mnea =
                            NEA.fromArray Array.empty
                    in
                    Expect.equal Nothing mnea
            , test "created from a non empty array, it is Just" <|
                \_ ->
                    let
                        mnea =
                            [ 1, 2, 3 ]
                                |> Array.fromList
                                |> NEA.fromArray
                    in
                    expectJust mnea
            , fuzz (Fuzz.intRange 1 1000) "created from an array, it has the same length" <|
                \num ->
                    let
                        mnea =
                            Array.repeat num "x"
                                |> NEA.fromArray
                    in
                    expectMaybe
                        (\nea -> Expect.equal num (NEA.length nea))
                        mnea
            ]
        , describe "fromList (array creation)"
            [ test "created from an empty list, it is Nothing" <|
                \_ ->
                    let
                        mnea =
                            NEA.fromList []
                    in
                    Expect.equal Nothing mnea
            , test "created from a non empty list, it is Just" <|
                \_ ->
                    let
                        mnea =
                            [ 1, 2, 3 ]
                                |> NEA.fromList
                    in
                    expectJust mnea
            ]
        , describe "push"
            [ test "push to singleton" <|
                \_ ->
                    let
                        nea =
                            NEA.singleton "a"
                                |> NEA.push "b"
                    in
                    expectAll
                        [ Expect.equal (Just "a") (NEA.get 0 nea)
                        , Expect.equal (Just "b") (NEA.get 1 nea)
                        , Expect.equal Nothing (NEA.get 2 nea)
                        ]
            ]
        , test "push to array with multiple elements" <|
            \_ ->
                let
                    mnea =
                        NEA.fromList [ "a", "b", "c" ]
                            |> Maybe.map (NEA.push "d")
                in
                expectMaybe
                    (\nea ->
                        expectAll
                            [ Expect.equal (Just "a") (NEA.get 0 nea)
                            , Expect.equal (Just "b") (NEA.get 1 nea)
                            , Expect.equal (Just "c") (NEA.get 2 nea)
                            , Expect.equal (Just "d") (NEA.get 3 nea)
                            , Expect.equal Nothing (NEA.get 4 nea)
                            ]
                    )
                    mnea
        , describe "append"
            [ test "append two single element arrays" <|
                \_ ->
                    let
                        nea1 =
                            NEA.singleton "a"

                        nea2 =
                            NEA.singleton "b"

                        result =
                            NEA.append nea1 nea2
                    in
                    expectAll
                        [ Expect.equal (Just "a") (NEA.get 0 result)
                        , Expect.equal (Just "b") (NEA.get 1 result)
                        , Expect.equal Nothing (NEA.get 2 result)
                        ]
            , test "append single- and multi-element arrays" <|
                \_ ->
                    let
                        nea1 =
                            NEA.singleton "a"

                        mnea2 =
                            NEA.fromList [ "b", "c", "d" ]

                        maybeResult =
                            Maybe.map
                                (\nea2 -> NEA.append nea1 nea2)
                                mnea2
                    in
                    expectMaybe
                        (\result ->
                            expectAll
                                [ Expect.equal (Just "a") (NEA.get 0 result)
                                , Expect.equal (Just "b") (NEA.get 1 result)
                                , Expect.equal (Just "c") (NEA.get 2 result)
                                , Expect.equal (Just "d") (NEA.get 3 result)
                                , Expect.equal Nothing (NEA.get 4 result)
                                ]
                        )
                        maybeResult
            , test "append multi- and single-element arrays" <|
                \_ ->
                    let
                        mnea1 =
                            NEA.fromList [ "a", "b", "c" ]

                        nea2 =
                            NEA.singleton "d"

                        maybeResult =
                            Maybe.map
                                (\nea1 -> NEA.append nea1 nea2)
                                mnea1
                    in
                    expectMaybe
                        (\result ->
                            expectAll
                                [ Expect.equal (Just "a") (NEA.get 0 result)
                                , Expect.equal (Just "b") (NEA.get 1 result)
                                , Expect.equal (Just "c") (NEA.get 2 result)
                                , Expect.equal (Just "d") (NEA.get 3 result)
                                , Expect.equal Nothing (NEA.get 4 result)
                                ]
                        )
                        maybeResult
            , test "append two multi--element arrays" <|
                \_ ->
                    let
                        mnea1 =
                            NEA.fromList [ "a", "b", "c" ]

                        mnea2 =
                            NEA.fromList [ "d", "e", "f" ]

                        maybeResult =
                            Maybe.map2
                                (\nea1 nea2 -> NEA.append nea1 nea2)
                                mnea1
                                mnea2
                    in
                    expectMaybe
                        (\result ->
                            expectAll
                                [ Expect.equal (Just "a") (NEA.get 0 result)
                                , Expect.equal (Just "b") (NEA.get 1 result)
                                , Expect.equal (Just "c") (NEA.get 2 result)
                                , Expect.equal (Just "d") (NEA.get 3 result)
                                , Expect.equal (Just "e") (NEA.get 4 result)
                                , Expect.equal (Just "f") (NEA.get 5 result)
                                , Expect.equal Nothing (NEA.get 6 result)
                                ]
                        )
                        maybeResult
            ]
        , describe "getFirst"
            [ test "get the first element via getFirst" <|
                \_ ->
                    let
                        nea =
                            NEA.singleton "element"
                    in
                    Expect.equal "element" (NEA.getFirst nea)
            , test "getFirst from array with multiple elements" <|
                \_ ->
                    let
                        firstElem =
                            NEA.fromList [ "a", "b" ]
                                |> Maybe.map NEA.getFirst
                    in
                    expectMaybe
                        (Expect.equal "a")
                        firstElem
            ]
        , describe "get"
            [ test "get the first element" <|
                \_ ->
                    let
                        mnea =
                            NEA.fromList [ "a", "b" ]

                        elemNegativeIndex =
                            Maybe.andThen (NEA.get -1) mnea

                        elem0 =
                            Maybe.andThen (NEA.get 0) mnea

                        elem1 =
                            Maybe.andThen (NEA.get 1) mnea

                        elem2 =
                            Maybe.andThen (NEA.get 2) mnea
                    in
                    expectAll
                        [ Expect.equal Nothing elemNegativeIndex
                        , Expect.equal (Just "a") elem0
                        , Expect.equal (Just "b") elem1
                        , Expect.equal Nothing elem2
                        ]
            ]
        , describe "set"
            [ test "set element at index in range" <|
                \_ ->
                    let
                        mnea =
                            NEA.fromList [ "a", "b", "c" ]
                                |> Maybe.map (NEA.set 1 "X")
                    in
                    expectMaybe
                        (\nea ->
                            expectAll
                                [ Expect.equal (Just "a") (NEA.get 0 nea)
                                , Expect.equal (Just "X") (NEA.get 1 nea)
                                , Expect.equal (Just "c") (NEA.get 2 nea)
                                , Expect.equal Nothing (NEA.get 3 nea)
                                ]
                        )
                        mnea
            , test "set element at negative index" <|
                \_ ->
                    let
                        original =
                            NEA.fromList [ "a", "b", "c" ]

                        updated =
                            Maybe.map (NEA.set -42 "X") original
                    in
                    Expect.equal original updated
            , test "set element at too large index" <|
                \_ ->
                    let
                        original =
                            NEA.fromList [ "a", "b", "c" ]

                        updated =
                            Maybe.map (NEA.set 42 "X") original
                    in
                    Expect.equal original updated
            ]
        , describe "toList"
            [ fuzz nonEmptyIntList "toList produces a list" <|
                \randomList ->
                    let
                        maybeList =
                            NEA.fromList randomList
                                |> Maybe.map NEA.toList
                    in
                    expectMaybe (Expect.equal randomList) maybeList
            ]
        , describe "toIndexedList"
            [ test "toIndexedList for singleton" <|
                \_ ->
                    let
                        indexedSingletonList =
                            NEA.singleton ()
                                |> NEA.toIndexedList
                    in
                    Expect.equal (Just ( 0, () )) (List.head indexedSingletonList)
            , test "toIndexedList produces correct indexes" <|
                \_ ->
                    let
                        listOfNumbers =
                            List.range 0 30

                        maybeIndexedList =
                            NEA.fromList listOfNumbers
                                |> Maybe.map NEA.toIndexedList
                    in
                    expectMaybe
                        (\indexedList ->
                            indexedList
                                |> List.map
                                    (\( index, element ) ->
                                        Expect.equal index element
                                    )
                                |> expectAll
                        )
                        maybeIndexedList
            ]
        , describe "map"
            [ test "mapping over a single element" <|
                \_ ->
                    let
                        nea =
                            NEA.singleton 5
                                |> NEA.map (\x -> x * 2)
                    in
                    Expect.equal 10 (NEA.getFirst nea)
            , test "mapping over a multiple elements" <|
                \_ ->
                    let
                        mnea =
                            NEA.fromList [ 1, 2, 3, 4 ]
                                |> Maybe.map (NEA.map (\x -> x * 2))
                    in
                    expectMaybe
                        (\nea ->
                            expectAll
                                [ Expect.equal 4 (NEA.length nea)
                                , Expect.equal (Just 2) (NEA.get 0 nea)
                                , Expect.equal (Just 4) (NEA.get 1 nea)
                                , Expect.equal (Just 6) (NEA.get 2 nea)
                                , Expect.equal (Just 8) (NEA.get 3 nea)
                                , Expect.equal Nothing (NEA.get 4 nea)
                                ]
                        )
                        mnea
            ]
        , describe "filter"
            [ test "filter to zero elements returns Nothing" <|
                \_ ->
                    let
                        mnea =
                            NEA.fromList [ 1, 3, 5, 7 ]
                                |> Maybe.andThen (NEA.filter isEven)
                    in
                    Expect.equal Nothing mnea
            , fuzz intList "filter to > 0 elements returns Just" <|
                \randomList ->
                    let
                        mnea =
                            randomList
                                -- make sure there is at least one even number
                                |> (++) [ 2 ]
                                |> NEA.fromList
                                |> Maybe.andThen (NEA.filter isEven)
                    in
                    expectJust mnea
            , test "filter keeps the order" <|
                \_ ->
                    let
                        mnea =
                            NEA.fromList [ 0, 1, 2, 3, 4, 5, 6, 7 ]
                                |> Maybe.andThen (NEA.filter isEven)
                    in
                    expectMaybe
                        (\nea ->
                            expectAll
                                [ Expect.equal (Just 0) (NEA.get 0 nea)
                                , Expect.equal (Just 2) (NEA.get 1 nea)
                                , Expect.equal (Just 4) (NEA.get 2 nea)
                                , Expect.equal (Just 6) (NEA.get 3 nea)
                                , Expect.equal Nothing (NEA.get 4 nea)
                                ]
                        )
                        mnea
            ]
        , describe "foldl"
            [ test "foldl singleton" <|
                \_ ->
                    let
                        nea =
                            NEA.singleton "Hello"
                    in
                    Expect.equal "HelloWorld" (NEA.foldl (++) "World" nea)
            , fuzz nonEmptyStringList "foldl array with multiple elements" <|
                \randomList ->
                    let
                        foldedViaNea =
                            randomList
                                |> NEA.fromList
                                |> Maybe.map (NEA.foldl (++) "x")

                        foldedViaList =
                            randomList
                                |> List.foldl (++) "x"
                    in
                    expectMaybe
                        (\nea ->
                            Expect.equal foldedViaList nea
                        )
                        foldedViaNea
            ]
        , describe "foldr"
            [ test "foldr singleton" <|
                \_ ->
                    let
                        nea =
                            NEA.singleton "Hello"
                    in
                    Expect.equal "HelloWorld" (NEA.foldr (++) "World" nea)
            , fuzz nonEmptyStringList "foldr array with multiple elements" <|
                \randomList ->
                    let
                        foldedViaNea =
                            randomList
                                |> NEA.fromList
                                |> Maybe.map (NEA.foldr (++) "x")

                        foldedViaList =
                            randomList
                                |> List.foldr (++) "x"
                    in
                    expectMaybe
                        (\nea ->
                            Expect.equal foldedViaList nea
                        )
                        foldedViaNea
            ]
        ]


expectJust : Maybe (NonEmptyArray a) -> Expectation
expectJust maybe =
    case maybe of
        Just _ ->
            Expect.pass

        Nothing ->
            Expect.fail "Expected a Just _, got a Nothing"


expectMaybe :
    (a -> Expectation)
    -> Maybe a
    -> Expectation
expectMaybe expectation maybe =
    case maybe of
        Just something ->
            expectation something

        Nothing ->
            Expect.fail "Expected a Just, got a Nothing"


expectAll : List Expectation -> Expectation
expectAll expectations =
    let
        subject =
            ()

        functions : List (subject -> Expectation)
        functions =
            List.map (\exp -> \subj -> exp) expectations
    in
    Expect.all functions ()


intList : Fuzzer (List Int)
intList =
    Fuzz.list (Fuzz.intRange 0 1000)


nonEmptyIntList : Fuzzer (List Int)
nonEmptyIntList =
    intList
        |> Fuzz.conditional
            { retries = 10
            , fallback = \_ -> [ 42 ]
            , condition = List.isEmpty >> not
            }


nonEmptyStringList : Fuzzer (List String)
nonEmptyStringList =
    Fuzz.intRange 0 1000
        |> Fuzz.map toString
        |> Fuzz.list
        |> Fuzz.conditional
            { retries = 10
            , fallback = \_ -> [ "42" ]
            , condition = List.isEmpty >> not
            }


isEven : Int -> Bool
isEven x =
    x % 2 == 0



-- test toArray
-- slice
-- Use same order of functions as Array.Hamt
-- Use same docs as Array.Hamt
-- currentIndex feature
-- check AnimalActionSet which functions are provided there
