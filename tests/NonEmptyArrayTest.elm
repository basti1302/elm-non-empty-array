module NonEmptyArrayTest exposing (..)

import Array.Hamt as Array exposing (Array)
import Array.NonEmpty as NEA exposing (NonEmptyArray)
import Expect exposing (Expectation)
import Fuzz exposing (..)
import Random
import Test exposing (..)


suite : Test
suite =
    describe "The non empty array module"
        [ describe "array creation"
            [ test "created as a singleton, it has length 1" <|
                \_ ->
                    let
                        nea =
                            NEA.singleton "a"
                    in
                    Expect.equal 1 (NEA.length nea)
            , test "created from an empty array, it is Nothing" <|
                \_ ->
                    let
                        mnea =
                            NEA.fromArray Array.empty
                    in
                    Expect.equal Nothing mnea
            , test "created from an empty list, it is Nothing" <|
                \_ ->
                    let
                        mnea =
                            NEA.fromList []
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
            , test "created from a non empty list, it is Just" <|
                \_ ->
                    let
                        mnea =
                            [ 1, 2, 3 ]
                                |> NEA.fromList
                    in
                    expectJust mnea
            , fuzz (intRange 1 1000) "created from an array, it has the same length" <|
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
            , fuzz
                (list (intRange 0 1000))
                "filter to > 0 elements returns Just"
              <|
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
            ]
        ]



-- Test the resulting order of filter
-- Test append, especially the resulting order
-- Use same order of functions as Array.Hamt
-- Use same docs as Array.Hamt
-- push
-- append
-- get
-- set
-- toList
-- toIndexedList
-- check AnimalActionSet which functions are provided there
-- Foldl
-- Foldr


expectJust : Maybe (NonEmptyArray a) -> Expectation
expectJust maybe =
    case maybe of
        Just _ ->
            Expect.pass

        Nothing ->
            Expect.fail "Expected a Just _, got a Nothing"


expectMaybe :
    (NonEmptyArray a -> Expectation)
    -> Maybe (NonEmptyArray a)
    -> Expectation
expectMaybe expectation maybe =
    case maybe of
        Just nea ->
            expectation nea

        Nothing ->
            Expect.fail "Expected a Just _, got a Nothing"


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


isEven : Int -> Bool
isEven x =
    x % 2 == 0
