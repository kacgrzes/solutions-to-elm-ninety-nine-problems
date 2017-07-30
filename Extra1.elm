module Extra1 exposing (..)

import Html
import List


dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
    -- your implementation here
    case list of
        [] ->
            []

        head :: tail ->
            if predicate head then
                dropWhile predicate tail
            else
                list


main =
    Html.text
        (if test then
            "Your implementation passed all tests."
         else
            "Your implementation failed at least one test."
        )


test : Bool
test =
    List.all ((==) True)
        [ dropWhile isOdd [ 1, 2, 1 ] == [ 2, 1 ]
        , dropWhile isEven [ 1, 2, 1 ] == [ 1, 2, 1 ]
        , dropWhile isEven [] == []
        , dropWhile isEven [ 2, 4, 100000, 1 ] == [ 1 ]
        , dropWhile (\x -> x < 5) (List.range 1 10) == [ 5, 6, 7, 8, 9, 10 ]
        ]


isEven x =
    x % 2 == 0


isOdd x =
    x % 2 /= 0
