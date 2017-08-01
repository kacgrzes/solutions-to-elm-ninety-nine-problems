module Main exposing (..)

import Extra1 exposing (dropWhile)
import Extra2 exposing (takeWhile)
import Html
import List
import Maybe


pack : List a -> List (List a)
pack xs =
    case xs of
        -- your implementation goes here
        [] ->
            []

        head :: tail ->
            let
                comparator =
                    \x -> x == head

                taken =
                    takeWhile comparator xs

                reminder =
                    dropWhile comparator tail
            in
            taken :: pack reminder


main : Html.Html a
main =
    Html.text <|
        case test of
            0 ->
                "Your implementation passed all tests."

            1 ->
                "Your implementation failed one test."

            x ->
                "Your implementation failed " ++ toString x ++ " tests."


test : Int
test =
    List.length <|
        List.filter ((==) False)
            [ pack [ 1, 1, 1, 1, 2, 5, 5, 2, 1 ] == [ [ 1, 1, 1, 1 ], [ 2 ], [ 5, 5 ], [ 2 ], [ 1 ] ]
            , pack [ 2, 1, 1, 1 ] == [ [ 2 ], [ 1, 1, 1 ] ]
            , pack [ 2, 2, 2, 1, 1, 1 ] == [ [ 2, 2, 2 ], [ 1, 1, 1 ] ]
            , pack [ 1 ] == [ [ 1 ] ]
            , pack [] == []
            , pack [ "aa", "aa", "aa" ] == [ [ "aa", "aa", "aa" ] ]
            , pack [ "aab", "b", "b", "aa" ] == [ [ "aab" ], [ "b", "b" ], [ "aa" ] ]
            ]
