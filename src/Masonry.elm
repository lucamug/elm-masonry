module Masonry exposing (fromItems)

import Array
import Element exposing (..)
import List.Extra


type alias Position =
    Int


type alias Height =
    Int


type alias Masonry =
    List (List ( Position, Height ))


columnHeight : List ( Position, Height ) -> Height
columnHeight column =
    List.foldl (\( _, height ) total -> height + total) 0 column


columnsHeights : Masonry -> List Height
columnsHeights masonry =
    List.map columnHeight masonry


positionOfShortestHeight : List Height -> Position
positionOfShortestHeight listOfHeights =
    let
        helper itemPosition itemHeight accPosition =
            if itemHeight == (Maybe.withDefault 0 <| List.minimum listOfHeights) then
                itemPosition

            else
                accPosition
    in
    List.Extra.indexedFoldl helper 0 listOfHeights


minimumHeightPosition : Masonry -> Position
minimumHeightPosition masonry =
    masonry |> columnsHeights |> positionOfShortestHeight


addItemToMasonry : Position -> Height -> Masonry -> Masonry
addItemToMasonry position height masonry =
    let
        minPosition =
            minimumHeightPosition masonry

        column =
            Maybe.withDefault [] <| Array.get minPosition (Array.fromList masonry)

        newColumn_ =
            ( position, height ) :: column
    in
    Array.toList <| Array.set minPosition newColumn_ (Array.fromList masonry)


fromItems : List Height -> Int -> Masonry
fromItems items columns =
    List.Extra.indexedFoldl addItemToMasonry (List.repeat columns []) items



--
-- EXTRA STUFF TO RUN STAND-ALONE
--
-- Uncomment this section if you would like to run this script stand-alone
--
{-
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
-}
