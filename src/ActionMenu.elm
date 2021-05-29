module ActionMenu exposing (..)

import FontAwesome.Icon as Icon exposing (Icon)
import FontAwesome.Attributes as FontAwesomeAttributes
import Element exposing (Element, below, column, el, row, text)
import Element exposing (Attribute)
import Element.Events exposing (onClick)
import Browser.Events
import Json.Decode as Json

type State = Opened | Closed

type alias ActionMenu msg =
    { icon : Icon 
    , name : String
    , elements : List (ActionMenuItem msg)
    , state : State
    , toggleMsg : State -> msg 
    }

type ActionMenuItem msg = ActionMenuItem (Element msg)

actionMenu : ActionMenu msg -> Element msg
actionMenu { icon, name, elements, state, toggleMsg } = 
    let
        attributes = case state of 
            Opened -> [below (column [] (List.map (\(ActionMenuItem item) -> item) elements)) ]
            Closed -> []
    in
    el 
        (attributes ++ [ onClick (toggleMsg (nextStatus state)) ])
        (el [] (row [] [Element.text name, Element.html (Icon.viewStyled [ FontAwesomeAttributes.xs ] icon) ]))

actionMenuItem : String -> List (Attribute msg) -> ActionMenuItem msg
actionMenuItem name attributes =
    el attributes (text name) |> ActionMenuItem

-- subscriptions : State -> (State -> msg) -> Sub msg
-- subscriptions state toMsg =
--     let
--         a = Debug.log "test" 1    
--     in
--     case state of
--         Opened ->
--             Browser.Events.onAnimationFrame
--                 (\_ -> toMsg <| Waiting)

--         Waiting ->
--             Browser.Events.onClick
--                 (Json.succeed <| toMsg <| Closed)

--         Closed ->
--             Sub.none

nextStatus : State -> State
nextStatus status =
    case status of
        Opened ->
            Closed

        Closed ->
            Opened
