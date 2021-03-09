module Main exposing (main)

import Array
import Browser
import Browser.Events
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import List.Extra as List
import Masonry
import Svg
import Svg.Attributes as SA


type alias Model =
    { items : List Int
    , spacing : Int
    , padding : Int
    , masonryWidth : Float
    , masonryHeight : Float
    , border : Float
    , rounded : Float
    , itemMinWidth : Float
    , fullscreen : Bool
    , smooth : Bool
    , background : Bool
    , screenWidth : Int
    , screenHeight : Int
    , menuOpen : Bool
    }


initListOfItems : List Int
initListOfItems =
    [ 74, 96, 58, 120, 100, 70, 112, 140, 60, 80, 90, 50, 100 ]


type alias Flags =
    { width : Int
    , height : Int
    }


initModel : Flags -> Model
initModel flags =
    initModel1
        { screenHeight = flags.height
        , screenWidth = flags.width
        , smooth = True
        }


initModel1 :
    { a
        | screenHeight : Int
        , screenWidth : Int
        , smooth : Bool
    }
    -> Model
initModel1 model =
    { items = generateItems 50 initListOfItems
    , spacing = 8
    , padding = 8
    , masonryWidth =
        if isSmallScreen model.screenWidth then
            toFloat <| masonryWidth model.screenWidth

        else
            1000
    , masonryHeight = 740
    , border = 0
    , rounded = 7
    , smooth = model.smooth
    , background = True
    , itemMinWidth = 250
    , fullscreen = False
    , screenWidth = model.screenWidth
    , screenHeight = model.screenHeight
    , menuOpen = False
    }


initModel2 :
    { a
        | screenHeight : Int
        , screenWidth : Int
        , smooth : Bool
    }
    -> Model
initModel2 model =
    { items = generateItems 100 initListOfItems
    , spacing = 0
    , padding = 0
    , masonryWidth =
        if isSmallScreen model.screenWidth then
            toFloat <| masonryWidth model.screenWidth

        else
            1000
    , masonryHeight = 740
    , border = 1
    , rounded = 0
    , smooth = model.smooth
    , background = False
    , itemMinWidth = 120
    , fullscreen =
        if isSmallScreen model.screenWidth then
            True

        else
            False
    , screenWidth = model.screenWidth
    , screenHeight = model.screenHeight
    , menuOpen = False
    }


initModel3 :
    { a
        | screenHeight : Int
        , screenWidth : Int
        , smooth : Bool
    }
    -> Model
initModel3 model =
    { items = generateItems 20 initListOfItems
    , spacing = 2
    , padding = 2
    , masonryWidth =
        if isSmallScreen model.screenWidth then
            toFloat <| masonryWidth model.screenWidth

        else
            360
    , masonryHeight = toFloat <| model.screenHeight - 40
    , border = 2
    , rounded = 20
    , smooth = model.smooth
    , background = True
    , itemMinWidth = 180
    , fullscreen =
        if isSmallScreen model.screenWidth then
            True

        else
            False
    , screenWidth = model.screenWidth
    , screenHeight = model.screenHeight
    , menuOpen = False
    }


initModel4 :
    { a
        | screenHeight : Int
        , screenWidth : Int
        , smooth : Bool
    }
    -> Model
initModel4 model =
    { items = generateItems 100 [ 192, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78 ]
    , spacing = 0
    , padding = 0
    , masonryWidth = toFloat <| masonryWidth model.screenWidth
    , masonryHeight = toFloat <| model.screenHeight - 40
    , border = 0
    , rounded = 100
    , smooth = model.smooth
    , background = True
    , itemMinWidth = 120
    , fullscreen = True
    , screenWidth = model.screenWidth
    , screenHeight = model.screenHeight
    , menuOpen = False
    }


init : Flags -> ( Model, Cmd msg )
init flags =
    ( initModel flags
    , Cmd.none
    )


type Msg
    = DoNothing
    | OnChangeMasonryWidth Float
    | OnChangeMasonryHeight Float
    | OnChangeMinItemWidth Float
    | OnChangeItems Float
    | OnChangeItemQuantity Int Float
    | OnChangeSpacing Float
    | OnChangePadding Float
    | OnChangeBorder Float
    | OnChangeRounded Float
    | ToggleSmooth Bool
    | ToggleBackground Bool
    | ToggleFullscreen Bool
    | ToggleMenu
    | Preset Model
    | OnResize Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            ( model, Cmd.none )

        OnChangeMasonryWidth value ->
            ( { model | masonryWidth = value }, Cmd.none )

        OnChangeMasonryHeight value ->
            ( { model | masonryHeight = value }, Cmd.none )

        OnChangeBorder value ->
            ( { model | border = value }, Cmd.none )

        OnChangeRounded value ->
            ( { model | rounded = value }, Cmd.none )

        OnChangeItems value ->
            ( { model | items = generateItems value initListOfItems }, Cmd.none )

        OnChangeSpacing value ->
            ( { model | spacing = round value }, Cmd.none )

        OnChangePadding value ->
            ( { model | padding = round value }, Cmd.none )

        OnChangeMinItemWidth value ->
            ( { model | itemMinWidth = value }, Cmd.none )

        Preset value ->
            ( value, Cmd.none )

        ToggleMenu ->
            ( { model | menuOpen = not model.menuOpen }, Cmd.none )

        ToggleSmooth value ->
            ( { model | smooth = value }, Cmd.none )

        ToggleBackground value ->
            ( { model | background = value }, Cmd.none )

        ToggleFullscreen value ->
            ( { model
                | fullscreen = value
                , masonryWidth =
                    if value then
                        toFloat <| masonryWidth model.screenWidth

                    else
                        model.masonryWidth - 40
              }
            , Cmd.none
            )

        OnChangeItemQuantity index width ->
            ( { model
                | items =
                    model.items
                        |> Array.fromList
                        |> Array.set index (round width)
                        |> Array.toList
              }
            , Cmd.none
            )

        OnResize x y ->
            ( { model
                | screenWidth = x
                , screenHeight = y
                , masonryWidth =
                    if shouldRecalculateTheMasonryWidth model then
                        toFloat <| masonryWidth x

                    else
                        model.masonryWidth
              }
            , Cmd.none
            )


shouldRecalculateTheMasonryWidth : { a | fullscreen : Bool, menuOpen : Bool, screenWidth : Int } -> Bool
shouldRecalculateTheMasonryWidth model =
    model.fullscreen || isSmallScreen model.screenWidth


generateItems : Float -> List Int -> List Int
generateItems quantity seed =
    let
        larger =
            ceiling (quantity / toFloat (List.length seed))
    in
    List.take (round quantity) <| List.concat <| List.repeat larger seed


sliderStyle : List (Attribute msg)
sliderStyle =
    [ height <| fill
    , width fill
    , behindContent
        (el
            [ width fill
            , height (px 6)
            , centerY
            , Background.color <| rgb 0.5 0.5 0.5
            , Border.rounded 2
            ]
            none
        )
    ]


slider :
    { max : Float
    , min : Float
    , onChange : Float -> Msg
    , step : Float
    , textAfter : String
    , textBefore : String
    , value : Float
    , enabled : Bool
    }
    -> Element Msg
slider args =
    row
        [ spacing 20
        , width fill
        , alpha <|
            if args.enabled then
                1

            else
                0.3
        ]
        [ el [ width <| px 100, Font.alignRight ] <| text args.textBefore
        , Input.slider sliderStyle
            { onChange =
                if args.enabled then
                    args.onChange

                else
                    \_ -> DoNothing
            , label = Input.labelHidden ""
            , min = args.min
            , max = args.max
            , value = args.value
            , thumb = Input.defaultThumb
            , step = Just args.step
            }
        , el [ width <| px 60, Font.alignRight ] <| text <| String.fromFloat args.value ++ args.textAfter
        ]


defaultItem : Model -> Int -> Int -> Element msg
defaultItem model position height_ =
    let
        itemRatio_ =
            itemRatio
                { masonryWidth = model.masonryWidth
                , spacing = model.spacing
                , itemMinWidth = model.itemMinWidth
                }

        itemHeight =
            round <| toFloat height_ * (itemRatio_ / 100)

        picUrl =
            "images/pic" ++ String.fromInt (modBy 9 height_ + 1) ++ ".jpg"

        title =
            if model.background then
                wrappedRow [ Font.size 14, spacing 20, paddingXY 10 0 ]
                    [ el [ Font.color <| rgba 0 0 0 0.6, Font.bold ] <| text <| String.fromInt <| position + 1
                    , el [ Font.color <| rgba 0 0 0 0.4 ] <| text <| String.fromInt height_
                    ]

            else
                column [ Font.size <| 40, padding 20, spacing 5 ]
                    [ text <| String.fromInt <| position + 1
                    , el [ Font.size 14, Font.color <| rgba 1 1 1 0.7 ] <| text <| String.fromInt height_
                    ]

        content =
            el
                ([ width fill
                 , height <| px itemHeight
                 , Border.rounded <| round model.rounded
                 , Font.color <| rgb 1 1 1
                 , Border.width <| round model.border
                 , clip
                 ]
                    ++ (if model.smooth then
                            [ htmlAttribute <| Html.Attributes.style "transition" "all 0.4s" ]

                        else
                            []
                       )
                    ++ (if model.background then
                            [ Background.image picUrl
                            , Background.color <| rgb 0.1 0.1 0.1
                            , Border.color <| rgb 0.9 0.9 0.9
                            ]

                        else
                            [ Background.color <| rgb255 (255 - height_) 100 height_
                            , Border.color <| rgba 1 1 1 0.2
                            , inFront <| title
                            ]
                       )
                )
            <|
                none
    in
    if model.background then
        column
            ([ width fill
             , mouseOver [ Background.color <| rgb 0.9 0.9 0.9 ]
             , padding 10
             , spacing 10
             , Border.rounded <| round (model.rounded * 1.5)
             ]
                ++ (if model.smooth then
                        [ htmlAttribute <| Html.Attributes.style "transition" "all 0.4s" ]

                    else
                        []
                   )
            )
            [ content
            , title
            ]

    else
        column [ width fill ] [ content ]


leftColumnWidth : Int
leftColumnWidth =
    380


isSmallScreen : Int -> Bool
isSmallScreen screenWidth =
    screenWidth < 600


masonryWidth : Int -> Int
masonryWidth screenWidth =
    if isSmallScreen screenWidth then
        screenWidth

    else
        screenWidth - leftColumnWidth


itemRatio :
    { a
        | itemMinWidth : Float
        , masonryWidth : Float
        , spacing : Int
    }
    -> Float
itemRatio args =
    let
        columns =
            masonryColumns
                { masonryWidth = round args.masonryWidth
                , itemMinWidth = round args.itemMinWidth
                }
    in
    toFloat (round args.masonryWidth - (args.spacing * (columns + 1))) / toFloat columns


masonryColumns : { a | itemMinWidth : Int, masonryWidth : Int } -> Int
masonryColumns args =
    let
        columns =
            truncate <| toFloat args.masonryWidth / toFloat args.itemMinWidth
    in
    if columns < 1 then
        1

    else
        columns


separator : Element msg
separator =
    el [ paddingXY 0 15, width fill ] <|
        el
            [ Border.width 1
            , Border.color <| rgba 0 0 0 0.1
            , width fill
            ]
            none


header : Element Msg
header =
    row [ spacing 12 ]
        [ Input.button [] { label = el [ Font.size 30 ] <| text "☰", onPress = Just ToggleMenu }
        , el [ Font.size 28 ] <| text "elm-masonry"
        , newTabLink [] { url = "https://github.com/lucamug", label = html <| logo_lucamug 22 }
        , newTabLink [] { url = "https://github.com/lucamug/elm-masonry", label = html <| logo_github 22 }
        , newTabLink [] { url = "https://medium.com/@l.mugnaini/simple-masonry-layout-in-50-lines-of-elm-code-304ea9e9475c", label = html <| logo_medium 22 }
        , newTabLink [] { url = "https://twitter.com/luca_mug", label = html <| logo_twitter 22 }
        ]


logo_lucamug : Int -> Html.Html msg
logo_lucamug size =
    Svg.svg
        [ SA.xmlBase "http://www.w3.org/2000/svg"
        , SA.width <| String.fromInt size ++ "px"
        , SA.height <| String.fromInt size ++ "px"
        , SA.viewBox "0 0 100 100"
        ]
        [ Svg.path [ SA.fill "none", SA.d "M0 0h100v100H0z" ] []
        , Svg.circle [ SA.fill "tomato", SA.cx "50", SA.cy "50", SA.r "50" ] []

        -- , Svg.circle [ SA.fill "#bbb", SA.cx "50", SA.cy "50", SA.r "50" ] []
        , Svg.path [ SA.fill "#1e90ff", SA.d "M7.08 75.56c15.99 26.67 48.3 29.6 67.15 18.12-26.07-5.25-35.78-28.79-38.08-45.75-3.78.16-10.83-.05-15.76.13-3.08 17.08-7.86 21.09-13.3 27.5z" ] []

        -- , Svg.path [ SA.fill "#000", SA.d "M7.08 75.56c15.99 26.67 48.3 29.6 67.15 18.12-26.07-5.25-35.78-28.79-38.08-45.75-3.78.16-10.83-.05-15.76.13-3.08 17.08-7.86 21.09-13.3 27.5z" ] []
        , Svg.path [ SA.fill "#fff", SA.d "M3 43h15c4 0 4-5 0-5h-5c-1 0-1-1 0-1h22c4 0 4-5 0-5H7c-3 0-3 5 0 5h3c1 0 1 1 0 1l-8.55-.01C1.17 39.25.75 41.02.48 43zM93.84 60.95l-15-.05c-4-.01-4.01 4.99-.01 5l5 .02c1 0 1 1 0 1l-22-.07c-4 0-4.02 5-.02 5l28 .08c3 .01 3.02-4.99.02-5h-3c-1 0-1-1 0-1s5 0 10.6.07c.57-1.86.9-3 1.38-5zM20.21 47.62c-1.18 9.56-3.53 14.69-7.65 21.43 4.4 3.03 8.93-15.48 10.16-14.95 1.68.08-1.97 12.74-.52 12.88 1.58-.1 2.82-8.28 4.8-8.31 1.78.29 2.3 9.16 4.12 8.7 1.85-.31-.07-12.13 2.05-13.07 1.87-.2 5.07 16.32 9.59 14.85-3.63-7.02-5.7-14.78-6.7-21.47-4.54.05-11.69-.04-15.85-.06z" ] []
        ]


logo_github : Int -> Html.Html msg
logo_github size =
    Svg.svg
        [ SA.xmlBase "http://www.w3.org/2000/svg"
        , SA.width <| String.fromInt size ++ "px"
        , SA.height <| String.fromInt size ++ "px"
        , SA.viewBox "0 0 256 250"
        ]
        [ Svg.path [ SA.fill "#161614", SA.d "M128 0a128 128 0 00-40.5 249.5c6.4 1.1 8.8-2.8 8.8-6.2l-.2-23.8C60.5 227.2 53 204.4 53 204.4c-5.8-14.8-14.2-18.8-14.2-18.8-11.6-7.9.8-7.7.8-7.7 12.9.9 19.7 13.1 19.7 13.1 11.4 19.6 30 14 37.2 10.7 1.2-8.3 4.5-14 8.1-17.1-28.4-3.3-58.3-14.2-58.3-63.3 0-14 5-25.4 13.2-34.3a46 46 0 011.3-34S71.5 49.7 96 66.3a122.7 122.7 0 0164 0c24.5-16.6 35.2-13.1 35.2-13.1a46 46 0 011.3 33.9c8.2 9 13.2 20.3 13.2 34.3 0 49.2-30 60-58.5 63.2 4.6 4 8.7 11.7 8.7 23.7l-.2 35.1c0 3.4 2.4 7.4 8.8 6.1A128 128 0 00128 0zM48 182.3c-.3.7-1.3.9-2.3.4-.9-.4-1.4-1.3-1.1-1.9.3-.6 1.3-.8 2.2-.4 1 .4 1.5 1.3 1.1 2zm6.2 5.7c-.6.5-1.8.3-2.6-.6-.8-1-1-2.1-.4-2.7.7-.6 1.8-.3 2.7.6.8.9 1 2 .3 2.7zm4.4 7.1c-.8.6-2.1 0-2.9-1-.8-1.2-.8-2.6 0-3.1.8-.6 2 0 2.9 1 .8 1.2.8 2.6 0 3.1zm7.3 8.4c-.7.7-2.2.5-3.3-.5-1.1-1-1.5-2.5-.8-3.3.8-.8 2.3-.5 3.4.5 1 1 1.4 2.5.7 3.3zm9.4 2.8c-.3 1-1.7 1.4-3.2 1-1.4-.4-2.4-1.6-2.1-2.6.3-1 1.7-1.5 3.2-1 1.5.4 2.4 1.6 2.1 2.6zm10.7 1.2c0 1-1.1 1.9-2.7 2-1.5 0-2.7-.9-2.8-2 0-1 1.2-1.9 2.8-1.9 1.5 0 2.7.8 2.7 1.9zm10.6-.4c.2 1-.9 2-2.4 2.3-1.5.3-2.8-.3-3-1.3-.2-1.1.9-2.2 2.3-2.4 1.6-.3 3 .3 3.1 1.4z" ] []
        ]


logo_medium : Int -> Html.Html msg
logo_medium size =
    Svg.svg
        [ SA.xmlBase "http://www.w3.org/2000/svg"
        , SA.width <| String.fromInt size ++ "px"
        , SA.height <| String.fromInt size ++ "px"
        , SA.viewBox "0 0 256 256"
        ]
        [ Svg.path [ SA.fill "#12100E", SA.d "M0 0h256v256H0z" ] []
        , Svg.path [ SA.fill "#ffffff", SA.d "M61 86l-2-6-16-19v-3h50l38 84 34-84h48v3l-14 13-2 4v96l2 4 13 13v3h-67v-3l14-13 1-4V96l-38 98h-6L71 96v66c0 2 1 5 3 7l18 22v3H41v-3l18-22c2-2 3-5 2-7V86z" ] []
        ]


logo_twitter : Int -> Html.Html msg
logo_twitter size =
    Svg.svg
        [ SA.xmlBase "http://www.w3.org/2000/svg"
        , SA.width <| String.fromInt size ++ "px"
        , SA.height <| String.fromInt size ++ "px"
        , SA.viewBox "0 0 24 24"
        ]
        [ Svg.path [ SA.fill "#000000", SA.d "M24 5h-3l2-2-3 1a5 5 0 00-8 4C8 8 4 6 2 3c-2 2-1 5 1 7L1 9c0 2 2 5 4 5H3c0 2 2 3 4 3-2 2-4 3-7 3l8 2c9 0 14-8 14-15l2-2z" ] []
        ]


menuElement : Model -> Element Msg
menuElement model =
    column
        [ width <| px leftColumnWidth
        , alignTop
        , spacing 12
        , paddingXY 20 10
        , height <| px model.screenHeight
        , Background.color <| rgba 0.9 0.9 0.9 0.95
        , Font.size 16
        ]
        [ header
        , separator
        , text "Simple masonry layout in 50 lines of Elm code"
        , wrappedRow [ spacing 8 ]
            [ text "presets"
            , Input.button [ Border.width 1, paddingXY 8 5, Border.rounded 6 ] { label = text "1", onPress = Just <| Preset <| initModel1 model }
            , Input.button [ Border.width 1, paddingXY 8 5, Border.rounded 6 ] { label = text "2", onPress = Just <| Preset <| initModel2 model }
            , Input.button [ Border.width 1, paddingXY 8 5, Border.rounded 6 ] { label = text "3", onPress = Just <| Preset <| initModel3 model }
            , Input.button [ Border.width 1, paddingXY 8 5, Border.rounded 6 ] { label = text "4", onPress = Just <| Preset <| initModel4 model }
            ]
        , slider { textBefore = "width", value = model.masonryWidth, max = 1200, min = 320, step = 10, onChange = OnChangeMasonryWidth, enabled = not model.fullscreen, textAfter = " px" }
        , slider { textBefore = "height", value = model.masonryHeight, max = 740, min = 320, step = 10, onChange = OnChangeMasonryHeight, enabled = not model.fullscreen, textAfter = " px" }
        , Input.checkbox
            [ width shrink, alignRight ]
            { onChange = ToggleFullscreen
            , icon = Input.defaultCheckbox
            , checked = model.fullscreen
            , label = Input.labelLeft [] <| text "fullscreen"
            }
        , slider { textBefore = "item-min-width", value = model.itemMinWidth, max = 450, min = 120, step = 10, onChange = OnChangeMinItemWidth, enabled = True, textAfter = "" }
        , slider { textBefore = "spacing", value = toFloat model.spacing, max = 50, min = 0, step = 2, onChange = OnChangeSpacing, enabled = True, textAfter = " px" }
        , slider { textBefore = "padding", value = toFloat model.padding, max = 50, min = 0, step = 2, onChange = OnChangePadding, enabled = True, textAfter = " px" }
        , slider { textBefore = "items quantity", value = toFloat <| List.length model.items, max = 100, min = 1, step = 1, onChange = OnChangeItems, enabled = True, textAfter = "" }
        , slider { textBefore = "border", value = model.border, max = 30, min = 0, step = 1, onChange = OnChangeBorder, enabled = True, textAfter = " px" }
        , slider { textBefore = "rounded", value = model.rounded, max = 100, min = 0, step = 2, onChange = OnChangeRounded, enabled = True, textAfter = " px" }
        , Input.checkbox
            [ width shrink, alignRight ]
            { onChange = ToggleSmooth
            , icon = Input.defaultCheckbox
            , checked = model.smooth
            , label = Input.labelLeft [] <| text "smooth"
            }
        , Input.checkbox
            [ width shrink, alignRight ]
            { onChange = ToggleBackground
            , icon = Input.defaultCheckbox
            , checked = model.background
            , label = Input.labelLeft [] <| text "with background images"
            }
        , separator
        , column [ width fill, spacing 10, scrollbarY ] <|
            List.indexedMap
                (\index height_ ->
                    slider
                        { textBefore = "item " ++ String.fromInt (index + 1)
                        , value = toFloat height_
                        , max = 200
                        , min = 20
                        , step = 2
                        , onChange = OnChangeItemQuantity index
                        , enabled = True
                        , textAfter = " px"
                        }
                )
                model.items
        ]


viewMasonry :
    { columns : Int
    , items : List Int
    , padding : Int
    , spacing : Int
    , viewItem : Int -> Int -> Element msg
    }
    -> Element msg
viewMasonry args =
    row [ width fill, spacing args.spacing, padding args.padding ] <|
        List.map
            (\structureColumn ->
                column [ width fill, alignTop, spacing args.spacing ] <|
                    List.map
                        (\( position, height_ ) ->
                            args.viewItem position height_
                        )
                        (List.reverse structureColumn)
            )
            (List.reverse <| Masonry.fromItems args.items args.columns)


masonryElement : Model -> Element msg
masonryElement model =
    let
        masonry =
            viewMasonry
                { columns =
                    masonryColumns
                        { masonryWidth = round model.masonryWidth
                        , itemMinWidth = round model.itemMinWidth
                        }
                , items = model.items
                , padding = model.padding
                , spacing = model.spacing
                , viewItem = defaultItem model
                }
    in
    if shouldRecalculateTheMasonryWidth model then
        el
            ([ width <| px <| round model.masonryWidth
             , height <| px model.screenHeight
             , Background.color <| rgb 1 1 1
             , alignTop
             ]
                ++ (if isSmallScreen model.screenWidth then
                        []

                    else
                        [ scrollbarY ]
                   )
            )
        <|
            masonry

    else
        el
            [ Border.color <| rgb 0.2 0.2 0.2
            , Border.widthEach { bottom = 80, left = 5, right = 5, top = 30 }
            , Border.rounded 30
            , centerX
            , centerY
            , height <| px <| round model.masonryHeight
            , inFront <|
                el
                    [ Font.size 50
                    , centerX
                    , alignBottom
                    , width <| px 50
                    , height <| px 50
                    , moveDown 65
                    , Border.rounded 50
                    , Background.color <| rgb 0.1 0.1 0.1
                    ]
                <|
                    none
            ]
        <|
            el
                [ width <| px <| round model.masonryWidth
                , Background.color <| rgb 1 1 1
                , alignTop
                , scrollbarY
                ]
            <|
                masonry


view : Model -> Html.Html Msg
view model =
    layout
        [ Background.color <| rgb 0.9 0.9 0.9
        ]
    <|
        if isSmallScreen model.screenWidth then
            column
                [ spacing 10
                , width fill
                , inFront <|
                    el
                        ((htmlAttribute <| Html.Attributes.style "transition" "all 0.2s")
                            :: (if model.menuOpen then
                                    []

                                else
                                    [ moveLeft <| toFloat leftColumnWidth ]
                               )
                        )
                    <|
                        menuElement model
                ]
                [ el [ paddingXY 20 10, width fill ] header
                , masonryElement model
                ]

        else
            row [ width fill ]
                [ menuElement model
                , masonryElement model
                ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Browser.Events.onResize OnResize
        }
