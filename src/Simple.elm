module Simple exposing (main)

import Array
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html
import List.Extra
import Masonry exposing (..)


main : Html.Html msg
main =
    let
        viewItem position height_ =
            el
                [ width fill
                , height <| px height_
                , Background.color <| rgb 0.2 0.7 0.5
                , Border.rounded 10
                , Font.color <| rgb 1 1 1
                , clip
                , inFront <|
                    el [ Font.size 50, padding 10 ] <|
                        text <|
                            "( "
                                ++ String.fromInt position
                                ++ ", "
                                ++ String.fromInt height_
                                ++ " )"
                ]
            <|
                none

        viewMasonry args =
            row [ width fill, spacing args.spacing, padding args.padding ] <|
                List.map
                    (\masonryColumn ->
                        column [ width fill, alignTop, spacing args.spacing ] <|
                            List.map
                                (\( position, height_ ) ->
                                    args.viewItem position height_
                                )
                                (List.reverse masonryColumn)
                    )
                    (List.reverse <| fromItems args.items args.columns)
    in
    layout [] <|
        viewMasonry
            { items = [ 250, 200, 300, 200, 450, 200, 300, 500, 300, 250, 200, 150, 200 ]
            , columns = 4
            , spacing = 10
            , padding = 10
            , viewItem = viewItem
            }
