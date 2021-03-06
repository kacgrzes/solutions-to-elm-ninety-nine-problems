module Main exposing (..)

import Html
import List
import Maybe


elementAt : List a -> Int -> Maybe a
elementAt xs n =
    -- your implementation here
    case List.drop (n - 1) xs of
        [] ->
            Nothing

        y :: ys ->
            Just y


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
            [ elementAt [ 1, 2, 3, 4 ] 2 == Just 2
            , elementAt [ 1 ] 2 == Nothing
            , elementAt [] 2 == Nothing
            , elementAt [] -1 == Nothing
            , elementAt [ 'a', 'b', 'c' ] 2 == Just 'b'
            ]
