module ActionMenu exposing (..)

import FontAwesome.Icon as Icon exposing (Icon)
import FontAwesome.Attributes as FontAwesomeAttributes
import Element exposing (Element, below, column, el, row, text, mouseOver, padding)
import Element exposing (Attribute)
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element exposing (spacingXY)
import Html.Attributes exposing (style)
import Element.Input as Input
import Element.Background exposing (color)
import Element exposing (alignRight)
import Element exposing (alignLeft)
import Element exposing (fill)
import Element exposing (width)
import Element exposing (spacing)
import Browser.Events
import Json.Decode as Json

type State = Opened | Closed | ListenClicks

type alias ActionMenu msg =
    { icon : Icon 
    , name : Maybe String
    , elements : List (ActionMenuItem msg)
    , state : State
    , toggleMsg : State -> msg 
    }

type ActionMenuItem msg = ActionMenuItem (Element msg)

actionMenu : ActionMenu msg -> Element msg
actionMenu { icon, name, elements, state, toggleMsg } = 
    let
        attributes = case state of 
            Opened -> [below 
                (column 
                    [ Border.rounded 5, Element.htmlAttribute (style "align-self" "end")
                    , Border.shadow { offset = (2, 2), size = 1, blur = 1, color = (Element.rgba255 0 0 0 0.1) }] (List.map (\(ActionMenuItem item) -> item) elements)) ]
            ListenClicks -> [below 
                (column 
                    [ Border.rounded 5, Element.htmlAttribute (style "align-self" "end")
                    , Border.shadow { offset = (2, 2), size = 1, blur = 1, color = (Element.rgba255 0 0 0 0.1) }] (List.map (\(ActionMenuItem item) -> item) elements)) ]
            _ -> []
        htmlElements = case name of
            Just n -> [Element.el [Font.size 14] (Element.text n), Element.html (Icon.viewStyled [ FontAwesomeAttributes.xs, style "margin-left" "10px"] icon) ]
            Nothing -> [ Element.html (Icon.viewStyled [ FontAwesomeAttributes.xs] icon) ]
    in
    Input.button [ alignRight ]
    { onPress = Nothing
    , label = 
        (el 
            (attributes ++ [ onClick (toggleMsg (nextStatus state)), mouseOver [ color (Element.rgba255 0 0 0 0.1) ], padding 10, Border.rounded 5 ])
            (el [] (row [] htmlElements))
        )
    }
    

actionMenuItem : String -> List (Attribute msg) -> ActionMenuItem msg
actionMenuItem name attributes =
    el (attributes ++ [ spacing 5, mouseOver [ color (Element.rgba255 0 0 0 0.05) ], width fill ]) (Element.el [ padding 10] (text name)) |> ActionMenuItem

subscriptions : State -> (State -> msg) -> Sub msg
subscriptions state toMsg =
    let
        a = Debug.log "test" (toMsg state)
    in
    case state of
        Opened ->
            Browser.Events.onAnimationFrame
                (\_ -> toMsg <| ListenClicks)

        ListenClicks ->
            Browser.Events.onClick
                (Json.succeed <| toMsg <| Closed)

        Closed ->
            Sub.none

nextStatus : State -> State
nextStatus status =
    case status of
        Opened ->
            Closed

        ListenClicks ->
            Closed

        Closed ->
            Opened
