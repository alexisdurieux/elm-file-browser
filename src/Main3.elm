module Main3 exposing (main)

import Browser
import Html exposing (..)
import Element
import Element.Background exposing (color)
import Element exposing (fill, width)
import Element exposing (height)
import Element exposing (px)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { property : Int
    , property2 : String
    }


init : flags -> ( Model, Cmd Msg )
init flags =
    ( Model 0 "modelInitialValue", Cmd.none )


type Msg
    = Msg1
    | Msg2


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg1 ->
            ( model, Cmd.none )

        Msg2 ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Document Title"
    , body =
        [ div [ ]
            [ text "New Document" 
            , Element.layout [] (Element.el [ color (Element.rgb255 255 255 0), width (px 1000), height (px 500)  ] Element.none) ]
            
      ]
    }
