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
        [ describe "fromElement (array creation)"
            [ test "created as a singleton, it has length 1" <|
                \_ ->
                    let
                        nea =
                            NEA.fromElement "a"
                    in
                    Expect.equal 1 (NEA.length nea)
            , test "created as a singleton, it has selected index 0" <|
                \_ ->
                    let
                        nea =
                            NEA.fromElement "a"
                    in
                    Expect.equal 0 (NEA.selectedIndex nea)
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
                        , Expect.equal 0 (NEA.selectedIndex nea)
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
                        , Expect.equal 0 (NEA.selectedIndex nea)
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
                        , Expect.equal 0 (NEA.selectedIndex nea)
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
                        , Expect.equal 0 (NEA.selectedIndex nea)
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
            , test "created from a non empty array, it has selected index 0" <|
                \_ ->
                    let
                        mnea =
                            [ 1, 2, 3 ]
                                |> Array.fromList
                                |> NEA.fromArray
                    in
                    expectMaybe
                        (\nea ->
                            Expect.equal 0 (NEA.selectedIndex nea)
                        )
                        mnea
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
            , test "created from a non empty list, it has selected index 0" <|
                \_ ->
                    let
                        mnea =
                            [ 1, 2, 3 ]
                                |> NEA.fromList
                    in
                    expectMaybe
                        (\nea ->
                            Expect.equal 0 (NEA.selectedIndex nea)
                        )
                        mnea
            ]
        , describe "toString"
            [ test "render to string" <|
                \_ ->
                    let
                        nea =
                            NEA.initialize 3 identity
                    in
                    Expect.equal "NonEmptyArray [0,1,2] (0)" (NEA.toString nea)
            , test "render to string with different selected index" <|
                \_ ->
                    let
                        nea =
                            NEA.repeat 3 0
                                |> NEA.setSelectedIndex 1
                    in
                    Expect.equal "NonEmptyArray [0,0,0] (1)" (NEA.toString nea)
            ]
        , describe "push"
            [ test "push to singleton" <|
                \_ ->
                    let
                        nea =
                            NEA.fromElement "a"
                                |> NEA.push "b"
                    in
                    expectAll
                        [ Expect.equal (Just "a") (NEA.get 0 nea)
                        , Expect.equal (Just "b") (NEA.get 1 nea)
                        , Expect.equal Nothing (NEA.get 2 nea)
                        , Expect.equal 0 (NEA.selectedIndex nea)
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
                            , Expect.equal 0 (NEA.selectedIndex nea)
                            ]
                    )
                    mnea
        , describe "append"
            [ test "append two single element arrays" <|
                \_ ->
                    let
                        nea1 =
                            NEA.fromElement "a"

                        nea2 =
                            NEA.fromElement "b"

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
                            NEA.fromElement "a"

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
                            NEA.fromElement "d"

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
            , fuzz (Fuzz.intRange 0 9) "result of append uses selected index of first param" <|
                \idx ->
                    let
                        nea1 =
                            NEA.repeat 10 "a"
                                |> NEA.setSelectedIndex idx

                        nea2 =
                            NEA.repeat 10 "b"
                                |> NEA.setSelectedIndex (10 - idx - 1)

                        result =
                            NEA.append nea1 nea2
                    in
                    Expect.equal idx (NEA.selectedIndex result)
            ]
        , describe "getFirst"
            [ test "get the first element via getFirst" <|
                \_ ->
                    let
                        nea =
                            NEA.fromElement "element"
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
        , describe "getSelected"
            [ test "get the selected element" <|
                \_ ->
                    let
                        nea =
                            NEA.fromElement "element"
                    in
                    Expect.equal "element" (NEA.getSelected nea)
            , fuzz
                (Fuzz.map2 (,)
                    (Fuzz.intRange 1 100)
                    (Fuzz.intRange 1 100)
                )
                "get the selected element from array with multiple elements"
              <|
                \( int1, int2 ) ->
                    let
                        -- use the larger random int as the array size, the
                        -- smaller as the selected index
                        ( size, selectedIdx ) =
                            if int1 > int2 then
                                ( int1, int2 )
                            else if int2 > int1 then
                                ( int2, int1 )
                            else
                                ( int1, int1 - 1 )

                        nea =
                            NEA.initialize size identity
                                |> NEA.setSelectedIndex selectedIdx
                    in
                    Expect.equal selectedIdx (NEA.getSelected nea)
            ]
        , describe "setSelectedIndex"
            [ fuzz (Fuzz.intRange 0 9) "index in range is accepted" <|
                \selectedIdx ->
                    let
                        nea =
                            NEA.initialize 10 identity
                                |> NEA.setSelectedIndex selectedIdx
                    in
                    expectAll
                        [ Expect.equal selectedIdx (NEA.selectedIndex nea)
                        , Expect.equal selectedIdx (NEA.getSelected nea)
                        ]
            , fuzz
                (Fuzz.map2 (,)
                    (Fuzz.intRange 0 20)
                    (Fuzz.intRange 20 100)
                )
                "too large selected index is rejected"
              <|
                \( size, selectedIdx ) ->
                    let
                        nea =
                            NEA.initialize size identity
                                |> NEA.setSelectedIndex selectedIdx
                    in
                    Expect.equal 0 (NEA.selectedIndex nea)
            , fuzz
                (Fuzz.map2 (,)
                    (Fuzz.intRange 0 20)
                    (Fuzz.intRange -100 -1)
                )
                "too low selected index is rejected"
              <|
                \( size, selectedIdx ) ->
                    let
                        nea =
                            NEA.initialize size identity
                                |> NEA.setSelectedIndex selectedIdx
                    in
                    Expect.equal 0 (NEA.selectedIndex nea)
            ]
        , describe "setSelectedIndexAndReport"
            [ fuzz (Fuzz.intRange 1 9)
                "index in range is accepted and reported as True"
              <|
                \selectedIdx ->
                    let
                        ( nea, hasChanged ) =
                            NEA.initialize 10 identity
                                |> NEA.setSelectedIndexAndReport selectedIdx
                    in
                    expectAll
                        [ Expect.true "should report True" hasChanged
                        , Expect.equal selectedIdx (NEA.selectedIndex nea)
                        , Expect.equal selectedIdx (NEA.getSelected nea)
                        ]
            , fuzz (Fuzz.intRange 0 9)
                "report False if given index is already selected"
              <|
                \selectedIdx ->
                    let
                        ( nea, hasChanged ) =
                            NEA.initialize 10 identity
                                |> NEA.setSelectedIndex selectedIdx
                                |> NEA.setSelectedIndexAndReport selectedIdx
                    in
                    expectAll
                        [ Expect.false "should report False" hasChanged
                        , Expect.equal selectedIdx (NEA.selectedIndex nea)
                        , Expect.equal selectedIdx (NEA.getSelected nea)
                        ]
            , fuzz
                (Fuzz.map2 (,)
                    (Fuzz.intRange 0 20)
                    (Fuzz.intRange 20 100)
                )
                "too large selected index is rejected and reported as False"
              <|
                \( size, selectedIdx ) ->
                    let
                        ( nea, hasChanged ) =
                            NEA.initialize size identity
                                |> NEA.setSelectedIndexAndReport selectedIdx
                    in
                    expectAll
                        [ Expect.false "should report False" hasChanged
                        , Expect.equal 0 (NEA.selectedIndex nea)
                        ]
            , fuzz
                (Fuzz.map2 (,)
                    (Fuzz.intRange 0 20)
                    (Fuzz.intRange -100 -1)
                )
                "too low selected index is rejected and reported as False"
              <|
                \( size, selectedIdx ) ->
                    let
                        ( nea, hasChanged ) =
                            NEA.initialize size identity
                                |> NEA.setSelectedIndexAndReport selectedIdx
                    in
                    expectAll
                        [ Expect.false "should report False" hasChanged
                        , Expect.equal 0 (NEA.selectedIndex nea)
                        ]
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
        , describe "update"
            [ test "update in range" <|
                \_ ->
                    let
                        nea =
                            NEA.initialize 3 identity
                                |> NEA.setSelectedIndex 2
                                |> NEA.update 1 ((*) 10)
                    in
                    expectAll
                        [ Expect.equal (Just 0) (NEA.get 0 nea)
                        , Expect.equal (Just 10) (NEA.get 1 nea)
                        , Expect.equal (Just 2) (NEA.get 2 nea)
                        , Expect.equal 3 (NEA.length nea)
                        , Expect.equal 2 (NEA.selectedIndex nea)
                        ]
            , fuzz (Fuzz.intRange -10 -1) "update, index too small" <|
                \idx ->
                    let
                        nea =
                            NEA.initialize 3 identity
                                |> NEA.setSelectedIndex 2
                                |> NEA.update idx ((*) 3)
                    in
                    expectAll
                        [ Expect.equal (Just 0) (NEA.get 0 nea)
                        , Expect.equal (Just 1) (NEA.get 1 nea)
                        , Expect.equal (Just 2) (NEA.get 2 nea)
                        , Expect.equal 3 (NEA.length nea)
                        , Expect.equal 2 (NEA.selectedIndex nea)
                        ]
            , fuzz (Fuzz.intRange 3 20) "update, index too high" <|
                \idx ->
                    let
                        nea =
                            NEA.initialize 3 identity
                                |> NEA.setSelectedIndex 2
                                |> NEA.update idx ((*) 3)
                    in
                    expectAll
                        [ Expect.equal (Just 0) (NEA.get 0 nea)
                        , Expect.equal (Just 1) (NEA.get 1 nea)
                        , Expect.equal (Just 2) (NEA.get 2 nea)
                        , Expect.equal 3 (NEA.length nea)
                        , Expect.equal 2 (NEA.selectedIndex nea)
                        ]
            ]
        , describe "updateSelected"
            [ test "update the selected element" <|
                \_ ->
                    let
                        nea =
                            NEA.initialize 3 identity
                                |> NEA.setSelectedIndex 2
                                |> NEA.updateSelected ((*) 10)
                    in
                    expectAll
                        [ Expect.equal (Just 0) (NEA.get 0 nea)
                        , Expect.equal (Just 1) (NEA.get 1 nea)
                        , Expect.equal (Just 20) (NEA.get 2 nea)
                        , Expect.equal 3 (NEA.length nea)
                        , Expect.equal 2 (NEA.selectedIndex nea)
                        ]
            ]
        , describe "slice"
            [ fuzz sliceFuzzer1 "start == end -> Nothing" <|
                \( randomList, index ) ->
                    let
                        mnea =
                            NEA.fromList randomList
                                |> Maybe.andThen (NEA.slice index index)
                    in
                    Expect.equal Nothing mnea
            , fuzz (Fuzz.intRange 1 4) "start > end -> Nothing" <|
                \start ->
                    let
                        end =
                            start - 1

                        mnea =
                            NEA.fromList [ 0, 1, 2, 3 ]
                                |> Maybe.andThen (NEA.slice start end)
                    in
                    Expect.equal Nothing mnea
            , fuzz (Fuzz.intRange 0 3) "slice to one element" <|
                \start ->
                    let
                        mnea =
                            NEA.fromList [ 0, 1, 2, 3 ]
                                |> Maybe.andThen (NEA.slice start (start + 1))
                    in
                    expectMaybe
                        (\nea ->
                            expectAll
                                [ Expect.equal 1 (NEA.length nea)
                                , Expect.equal start (NEA.getFirst nea)
                                ]
                        )
                        mnea
            , fuzz (Fuzz.intRange -100 -4)
                "slice to first element with out of bounds start index"
              <|
                \start ->
                    let
                        mnea =
                            NEA.fromList [ 0, 1, 2, 3 ]
                                |> Maybe.andThen (NEA.slice start 1)
                    in
                    expectMaybe
                        (\nea ->
                            expectAll
                                [ Expect.equal 1 (NEA.length nea)
                                , Expect.equal 0 (NEA.getFirst nea)
                                ]
                        )
                        mnea
            , fuzz (Fuzz.intRange 4 100)
                "slice to last element with out of bounds end index"
              <|
                \end ->
                    let
                        mnea =
                            NEA.fromList [ 0, 1, 2, 3 ]
                                |> Maybe.andThen (NEA.slice 3 end)
                    in
                    expectMaybe
                        (\nea ->
                            expectAll
                                [ Expect.equal 1 (NEA.length nea)
                                , Expect.equal 3 (NEA.getFirst nea)
                                ]
                        )
                        mnea
            , fuzz (Fuzz.intRange -4 -1) "slice to one element from right" <|
                \start ->
                    let
                        end =
                            if start == -1 then
                                -- slice -1 0 is (correctly) the empty array,
                                -- for this case we need slice -1 4
                                4
                            else
                                start + 1

                        mnea =
                            NEA.fromList [ 0, 1, 2, 3 ]
                                |> Maybe.andThen (NEA.slice start end)

                        expectedElement =
                            start + 4
                    in
                    expectMaybe
                        (\nea ->
                            expectAll
                                [ Expect.equal 1 (NEA.length nea)
                                , Expect.equal expectedElement
                                    (NEA.getFirst nea)
                                ]
                        )
                        mnea
            , test "slice to more elements not including first" <|
                \_ ->
                    let
                        mnea =
                            NEA.fromList [ 0, 1, 2, 3, 4, 5, 6 ]
                                |> Maybe.andThen (NEA.slice 1 6)
                    in
                    expectMaybe
                        (\nea ->
                            expectAll
                                [ Expect.equal (Just 1) (NEA.get 0 nea)
                                , Expect.equal (Just 2) (NEA.get 1 nea)
                                , Expect.equal (Just 3) (NEA.get 2 nea)
                                , Expect.equal (Just 4) (NEA.get 3 nea)
                                , Expect.equal (Just 5) (NEA.get 4 nea)
                                , Expect.equal Nothing (NEA.get 5 nea)
                                , Expect.equal 5 (NEA.length nea)
                                ]
                        )
                        mnea
            , fuzz (Fuzz.intRange 7 100)
                "slice to more elements not including first with end out of bounds"
              <|
                \end ->
                    let
                        mnea =
                            NEA.fromList [ 0, 1, 2, 3, 4, 5, 6 ]
                                |> Maybe.andThen (NEA.slice 1 end)
                    in
                    expectMaybe
                        (\nea ->
                            expectAll
                                [ Expect.equal (Just 1) (NEA.get 0 nea)
                                , Expect.equal (Just 2) (NEA.get 1 nea)
                                , Expect.equal (Just 3) (NEA.get 2 nea)
                                , Expect.equal (Just 4) (NEA.get 3 nea)
                                , Expect.equal (Just 5) (NEA.get 4 nea)
                                , Expect.equal (Just 6) (NEA.get 5 nea)
                                , Expect.equal Nothing (NEA.get 6 nea)
                                , Expect.equal 6 (NEA.length nea)
                                ]
                        )
                        mnea
            , test "slice to more elements including first" <|
                \_ ->
                    let
                        mnea =
                            NEA.fromList [ 0, 1, 2, 3, 4, 5, 6 ]
                                |> Maybe.andThen (NEA.slice 0 6)
                    in
                    expectMaybe
                        (\nea ->
                            expectAll
                                [ Expect.equal (Just 0) (NEA.get 0 nea)
                                , Expect.equal (Just 1) (NEA.get 1 nea)
                                , Expect.equal (Just 2) (NEA.get 2 nea)
                                , Expect.equal (Just 3) (NEA.get 3 nea)
                                , Expect.equal (Just 4) (NEA.get 4 nea)
                                , Expect.equal (Just 5) (NEA.get 5 nea)
                                , Expect.equal Nothing (NEA.get 6 nea)
                                , Expect.equal 6 (NEA.length nea)
                                ]
                        )
                        mnea
            , fuzz (Fuzz.intRange -100 -7) "slice to more elements including first with start out of bounds" <|
                \start ->
                    let
                        mnea =
                            NEA.fromList [ 0, 1, 2, 3, 4, 5, 6 ]
                                |> Maybe.andThen (NEA.slice start 6)
                    in
                    expectMaybe
                        (\nea ->
                            expectAll
                                [ Expect.equal (Just 0) (NEA.get 0 nea)
                                , Expect.equal (Just 1) (NEA.get 1 nea)
                                , Expect.equal (Just 2) (NEA.get 2 nea)
                                , Expect.equal (Just 3) (NEA.get 3 nea)
                                , Expect.equal (Just 4) (NEA.get 4 nea)
                                , Expect.equal (Just 5) (NEA.get 5 nea)
                                , Expect.equal Nothing (NEA.get 6 nea)
                                , Expect.equal 6 (NEA.length nea)
                                ]
                        )
                        mnea
            , fuzz
                (Fuzz.map2 (,)
                    (Fuzz.intRange -7 -5)
                    (Fuzz.intRange -4 -1)
                )
                "slice to more elements from the right"
              <|
                \( start, end ) ->
                    let
                        mnea =
                            NEA.fromList [ 0, 1, 2, 3, 4, 5, 6 ]
                                |> Maybe.andThen (NEA.slice start end)

                        expectedLength =
                            end - start

                        expectedFirstElem =
                            start + 7
                    in
                    expectMaybe
                        (\nea ->
                            expectAll
                                [ Expect.equal expectedLength (NEA.length nea)
                                , Expect.equal expectedFirstElem
                                    (NEA.getFirst nea)
                                ]
                        )
                        mnea
            ]
        , describe "toArray"
            [ fuzz nonEmptyIntArray "toArray produces an array" <|
                \randomArray ->
                    let
                        maybeArray =
                            NEA.fromArray randomArray
                                |> Maybe.map NEA.toArray
                    in
                    expectMaybe (Expect.equal randomArray) maybeArray
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
                            NEA.fromElement ()
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
                            NEA.fromElement 5
                                |> NEA.map (\x -> x * 2)
                    in
                    Expect.equal 10 (NEA.getFirst nea)
            , test "mapping over multiple elements" <|
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
        , describe "indexedMap"
            [ test "indexedMap over multiple elements" <|
                \_ ->
                    let
                        nea =
                            NEA.initialize 3 identity
                                |> NEA.indexedMap (,)
                    in
                    expectAll
                        [ Expect.equal 3 (NEA.length nea)
                        , Expect.equal (Just ( 0, 0 )) (NEA.get 0 nea)
                        , Expect.equal (Just ( 1, 1 )) (NEA.get 1 nea)
                        , Expect.equal (Just ( 2, 2 )) (NEA.get 2 nea)
                        , Expect.equal Nothing (NEA.get 3 nea)
                        ]
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
        , describe "removeAt"
            [ fuzz nonEmptyIntList "removing at negative index does nothing" <|
                \randomList ->
                    let
                        result =
                            NEA.fromList randomList
                                |> Maybe.andThen (NEA.removeAt -1)
                                |> Maybe.map NEA.toList
                    in
                    Expect.equal result (Just randomList)
            , fuzz nonEmptyIntList "removing at an out of bound index does nothing" <|
                \randomList ->
                    let
                        removeIndex =
                            List.length randomList

                        result =
                            NEA.fromList randomList
                                |> Maybe.andThen (NEA.removeAt removeIndex)
                                |> Maybe.map NEA.toList
                    in
                    Expect.equal result (Just randomList)
            , test "removing the last element returns Nothing" <|
                \_ ->
                    let
                        mnea =
                            NEA.fromList [ "a" ]
                                |> Maybe.andThen (NEA.removeAt 0)
                    in
                    Expect.equal Nothing mnea
            , fuzz (Fuzz.intRange 0 1)
                "removing the second to last returns Just"
              <|
                \removeIndex ->
                    let
                        mnea =
                            NEA.fromList [ "a", "b" ]
                                |> Maybe.andThen (NEA.removeAt removeIndex)
                    in
                    expectJust mnea
            , test "removing the first when there are two" <|
                \_ ->
                    let
                        mnea =
                            NEA.fromList [ 0, 1 ]
                                |> Maybe.andThen (NEA.removeAt 0)
                    in
                    expectMaybe
                        (\nea ->
                            expectAll
                                [ Expect.equal (Just 1) (NEA.get 0 nea)
                                , Expect.equal Nothing (NEA.get 1 nea)
                                , Expect.equal 0 (NEA.selectedIndex nea)
                                ]
                        )
                        mnea
            , test "removing the second when there are two" <|
                \_ ->
                    let
                        mnea =
                            NEA.fromList [ 0, 1 ]
                                |> Maybe.andThen (NEA.removeAt 1)
                    in
                    expectMaybe
                        (\nea ->
                            expectAll
                                [ Expect.equal (Just 0) (NEA.get 0 nea)
                                , Expect.equal Nothing (NEA.get 1 nea)
                                , Expect.equal 0 (NEA.selectedIndex nea)
                                ]
                        )
                        mnea
            , test "removeAt keeps the order" <|
                \_ ->
                    let
                        mnea =
                            NEA.fromList [ 0, 1, 2, 3, 4, 5 ]
                                |> Maybe.andThen (NEA.removeAt 3)
                    in
                    expectMaybe
                        (\nea ->
                            expectAll
                                [ Expect.equal (Just 0) (NEA.get 0 nea)
                                , Expect.equal (Just 1) (NEA.get 1 nea)
                                , Expect.equal (Just 2) (NEA.get 2 nea)
                                , Expect.equal (Just 4) (NEA.get 3 nea)
                                , Expect.equal (Just 5) (NEA.get 4 nea)
                                , Expect.equal Nothing (NEA.get 5 nea)
                                ]
                        )
                        mnea
            , test "removing after the selected index leaves it unchanged" <|
                \_ ->
                    let
                        mnea =
                            NEA.fromList [ 0, 1, 2, 3, 4, 5 ]
                                |> Maybe.map (NEA.setSelectedIndex 3)
                                |> Maybe.andThen (NEA.removeAt 4)
                    in
                    expectMaybe
                        (\nea ->
                            Expect.equal 3 (NEA.selectedIndex nea)
                        )
                        mnea
            , test "removing the first element updates the selected index " <|
                \_ ->
                    let
                        mnea =
                            NEA.fromList [ 0, 1, 2, 3, 4, 5 ]
                                |> Maybe.map (NEA.setSelectedIndex 4)
                                |> Maybe.andThen (NEA.removeAt 0)
                    in
                    expectMaybe
                        (\nea ->
                            Expect.equal 3 (NEA.selectedIndex nea)
                        )
                        mnea
            , test "removing before the selected index updates it" <|
                \_ ->
                    let
                        mnea =
                            NEA.fromList [ 0, 1, 2, 3, 4, 5 ]
                                |> Maybe.map (NEA.setSelectedIndex 4)
                                |> Maybe.andThen (NEA.removeAt 3)
                    in
                    expectMaybe
                        (\nea ->
                            Expect.equal 3 (NEA.selectedIndex nea)
                        )
                        mnea
            , test "removing at the selected index updates it" <|
                \_ ->
                    let
                        mnea =
                            NEA.fromList [ 0, 1, 2, 3, 4, 5 ]
                                |> Maybe.map (NEA.setSelectedIndex 3)
                                |> Maybe.andThen (NEA.removeAt 3)
                    in
                    expectMaybe
                        (\nea ->
                            Expect.equal 2 (NEA.selectedIndex nea)
                        )
                        mnea
            ]
        , describe "removeAtSafe"
            [ fuzz nonEmptyIntList "removing at negative index does nothing" <|
                \randomList ->
                    let
                        result =
                            NEA.fromList randomList
                                |> Maybe.map (NEA.removeAtSafe -1)
                                |> Maybe.map NEA.toList
                    in
                    Expect.equal result (Just randomList)
            , fuzz nonEmptyIntList "removing at an out of bound index does nothing" <|
                \randomList ->
                    let
                        removeIndex =
                            List.length randomList

                        result =
                            NEA.fromList randomList
                                |> Maybe.map (NEA.removeAtSafe removeIndex)
                                |> Maybe.map NEA.toList
                    in
                    Expect.equal result (Just randomList)
            , test "removing the last element returns the array unchanged" <|
                \_ ->
                    let
                        result =
                            NEA.fromList [ "a" ]
                                |> Maybe.map (NEA.removeAtSafe 0)
                                |> Maybe.map NEA.toList
                    in
                    Expect.equal result (Just [ "a" ])
            , fuzz
                (Fuzz.intRange 0 1)
                "removing the second to last returns a changed array"
              <|
                \removeIndex ->
                    let
                        mnea =
                            NEA.fromList [ "a", "b" ]
                                |> Maybe.map (NEA.removeAtSafe removeIndex)
                    in
                    expectMaybe
                        (\nea -> Expect.equal 1 (NEA.length nea))
                        mnea
            , test "removing the first when there are two" <|
                \_ ->
                    let
                        mnea =
                            NEA.fromList [ 0, 1 ]
                                |> Maybe.map (NEA.removeAtSafe 0)
                    in
                    expectMaybe
                        (\nea ->
                            expectAll
                                [ Expect.equal (Just 1) (NEA.get 0 nea)
                                , Expect.equal Nothing (NEA.get 1 nea)
                                , Expect.equal 0 (NEA.selectedIndex nea)
                                ]
                        )
                        mnea
            , test "removing the second when there are two" <|
                \_ ->
                    let
                        mnea =
                            NEA.fromList [ 0, 1 ]
                                |> Maybe.map (NEA.removeAtSafe 1)
                    in
                    expectMaybe
                        (\nea ->
                            expectAll
                                [ Expect.equal (Just 0) (NEA.get 0 nea)
                                , Expect.equal Nothing (NEA.get 1 nea)
                                , Expect.equal 0 (NEA.selectedIndex nea)
                                ]
                        )
                        mnea
            , test "removeAtSafe keeps the order" <|
                \_ ->
                    let
                        mnea =
                            NEA.fromList [ 0, 1, 2, 3, 4, 5 ]
                                |> Maybe.map (NEA.removeAtSafe 3)
                    in
                    expectMaybe
                        (\nea ->
                            expectAll
                                [ Expect.equal (Just 0) (NEA.get 0 nea)
                                , Expect.equal (Just 1) (NEA.get 1 nea)
                                , Expect.equal (Just 2) (NEA.get 2 nea)
                                , Expect.equal (Just 4) (NEA.get 3 nea)
                                , Expect.equal (Just 5) (NEA.get 4 nea)
                                , Expect.equal Nothing (NEA.get 5 nea)
                                ]
                        )
                        mnea
            , test "removing after the selected index leaves it unchanged" <|
                \_ ->
                    let
                        mnea =
                            NEA.fromList [ 0, 1, 2, 3, 4, 5 ]
                                |> Maybe.map (NEA.setSelectedIndex 3)
                                |> Maybe.map (NEA.removeAtSafe 4)
                    in
                    expectMaybe
                        (\nea ->
                            Expect.equal 3 (NEA.selectedIndex nea)
                        )
                        mnea
            , test "removing the first element updates the selected index " <|
                \_ ->
                    let
                        mnea =
                            NEA.fromList [ 0, 1, 2, 3, 4, 5 ]
                                |> Maybe.map (NEA.setSelectedIndex 4)
                                |> Maybe.map (NEA.removeAtSafe 0)
                    in
                    expectMaybe
                        (\nea ->
                            Expect.equal 3 (NEA.selectedIndex nea)
                        )
                        mnea
            , test "removing before the selected index updates it" <|
                \_ ->
                    let
                        mnea =
                            NEA.fromList [ 0, 1, 2, 3, 4, 5 ]
                                |> Maybe.map (NEA.setSelectedIndex 4)
                                |> Maybe.map (NEA.removeAtSafe 3)
                    in
                    expectMaybe
                        (\nea ->
                            Expect.equal 3 (NEA.selectedIndex nea)
                        )
                        mnea
            , test "removing at the selected index updates it" <|
                \_ ->
                    let
                        mnea =
                            NEA.fromList [ 0, 1, 2, 3, 4, 5 ]
                                |> Maybe.map (NEA.setSelectedIndex 3)
                                |> Maybe.map (NEA.removeAtSafe 3)
                    in
                    expectMaybe
                        (\nea ->
                            Expect.equal 2 (NEA.selectedIndex nea)
                        )
                        mnea
            ]
        , describe "foldl"
            [ test "foldl singleton" <|
                \_ ->
                    let
                        nea =
                            NEA.fromElement "Hello"
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
                            NEA.fromElement "Hello"
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


isEven : Int -> Bool
isEven x =
    x % 2 == 0


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


nonEmptyIntArray : Fuzzer (Array Int)
nonEmptyIntArray =
    nonEmptyIntList
        |> Fuzz.map Array.fromList


{-| Generates a list of random length >= 1 and an index >= 0 and < length.
-}
sliceFuzzer1 : Fuzzer ( List Int, Int )
sliceFuzzer1 =
    let
        listFuzzer =
            nonEmptyIntList
    in
    listFuzzer
        |> Fuzz.andThen
            (\randomList ->
                let
                    maxIndex =
                        List.length randomList - 1

                    indexFuzzer =
                        Fuzz.intRange 0 maxIndex
                in
                Fuzz.map2 (,) listFuzzer indexFuzzer
            )
