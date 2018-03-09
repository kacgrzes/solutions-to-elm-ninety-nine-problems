import Html
import List


dropNth : List a -> Int -> List a
dropNth list n =
    -- your implementation goes here
    case max n 0 of
      0 ->
        list
      
      _ ->
        case list of
          [] ->
            []
          
          x :: xs -> 
            (List.take (n - 1) list) ++ (dropNth (List.drop n list) n)


main : Html.Html a
main =
    Html.text
        <| case test of
            0 ->
                "Your implementation passed all tests."

            1 ->
                "Your implementation failed one test."

            x ->
                "Your implementation failed " ++ (toString x) ++ " tests."


test : Int
test =
    List.length
        <| List.filter ((==) False)
            [ dropNth [ 1, 2, 5, 5, 2, 1 ] 2 == [ 1, 5, 2 ]
            , dropNth (1..20) 3 == [ 1, 2, 4, 5, 7, 8, 10, 11, 13, 14, 16, 17, 19, 20 ]
            , dropNth (1..5) 6 == [ 1, 2, 3, 4, 5 ]
            , dropNth (1..5) 0 == [ 1, 2, 3, 4, 5 ]
            , dropNth (1..5) -1 == [ 1, 2, 3, 4, 5 ]
            , dropNth (1..5) 1 == []
            , dropNth [ "1", "2", "3", "4", "5", "6" ] 2 == [ "1", "3", "5" ]
            ]


(..) : Int -> Int -> List Int
(..) start end =
    List.range start end