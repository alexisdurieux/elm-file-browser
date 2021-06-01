module Main2 exposing (..)

import Time
import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Element exposing (Color, Element, alignBottom, alignLeft, alignRight, alignTop, centerY, column, fill, fillPortion, height, mouseOver, padding, px, rgb255, row, spaceEvenly, spacing, spacingXY, table, width, wrappedRow)
import Element.Background exposing (color)
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import FontAwesome.Attributes as FontAwesomeAttributes
import FontAwesome.Icon as Icon exposing (Icon)
import FontAwesome.Solid exposing (file, folder, levelUpAlt)
import FontAwesome.Styles as FontAwesomeStyles
import Html exposing (Attribute)
import Html.Attributes exposing (style)
import Element.Events exposing (onDoubleClick)
import Element exposing (below)
import ActionMenu exposing (ActionMenu, State(..))
import ActionMenu exposing (actionMenuItem)
import ActionMenu exposing (actionMenu)
import FileBrowser exposing (FileOrFolder(..), createParents, fileOrFolderId)



{-
Sample folder structure
a
  c
    d
      data2
  data1
b
  data3

-}


folderA = Folder { id = "a", name = "Folder A", lastUpdated = Nothing, size = Nothing }
folderB = Folder { id = "b", name = "Folder B", lastUpdated = Nothing, size = Nothing }
folderC = Folder { id = "c", name = "Folder C", lastUpdated = Nothing, size = Nothing }
folderD = Folder { id = "d", name = "Folder D", lastUpdated = Nothing, size = Nothing }
file1 = File { id = "data1", name = "data1.csv", lastUpdated = Nothing, size = Nothing }
file2 = File { id = "data2", name = "data2.csv", lastUpdated = Nothing, size = Nothing }
file3 = File { id = "data3", name = "data3.csv", lastUpdated = Nothing, size = Nothing }
theFilesAndFolders : Dict String FileOrFolder
theFilesAndFolders = 
    Dict.fromList
    [ ("a", folderA )
    , ("b", folderB )
    , ("c", folderC )
    , ("d", folderD )
    , ("data1", file1 )
    , ("data2", file2 )
    , ("data3", file3 )
    ]

theChildren : Dict String (List FileOrFolder)
theChildren = Dict.fromList
    [ ( "a", [folderC, file1] )
    , ( "c", [folderD] )
    , ( "d", [file2] )
    , ( "b", [file3] )
    ]


-- Both filesAndFolders need to be served. Parents can be updated on the fly
type alias Model = 
    { filesAndFolders : Dict String FileOrFolder
    , currentFolder : Maybe FileOrFolder 
    , children : Dict String (List FileOrFolder) --> File should'nt have children but don't know how to best design the data structure yet
    , searchInput : String
    , actionMenus : Dict String (ActionMenu Msg)
    }

type Msg
    = Search String
    | ClickFolder FileOrFolder
    | ClickFile FileOrFolder
    | ClickFolderChainElement FileOrFolder
    | ClickMe
    | ToggleDropdown State
    | ToggleDropdown2 State
    | KissSophie


-- baseActions : ActionMenu
-- baseActions = 
--     { icon = Element.row [] [Element.text "Actions", Element.html (Icon.viewStyled [ lightGray, FontAwesomeAttributes.xs ] levelUpAlt)]
--     }

-- base2Actions : ActionMenu
-- base2Actions = 
--     { icon = Element.row [] [Element.text "Actions2", Element.html (Icon.viewStyled [ lightGray, FontAwesomeAttributes.xs ] levelUpAlt)]
--     }
model : Model 
model = 
    { filesAndFolders = theFilesAndFolders
    , currentFolder = Just folderA
    , children = theChildren
    , searchInput = ""
    , actionMenus = 
        Dict.fromList
        [
            ("sample-menu", 
            { icon = levelUpAlt 
            , name = Just "Sample Menu"
            , elements = [actionMenuItem "Click me" [ onClick ClickMe ]]
            , state = ActionMenu.Closed 
            , toggleMsg = ToggleDropdown }
            )
        ,   ("sample-menu2", 
            { icon = levelUpAlt 
            , name = Just"Sophie"
            , elements = [actionMenuItem "Click me 2" [ onClick ClickMe ], actionMenuItem "Kiss Sophie" [ onClick KissSophie ]]
            , state = ActionMenu.Closed 
            , toggleMsg = ToggleDropdown2 }
            )
        ]
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        Search v ->
            ( { m | searchInput = v }, Cmd.none )

        ClickFolder folder ->
            ( { m | currentFolder = Just folder }, Cmd.none )

        ClickFile file ->
            ( m, Cmd.none )

        ClickFolderChainElement folder ->
            ( { m | currentFolder = Just folder }, Cmd.none )

        ClickMe ->
            Debug.log "clickme" (m, Cmd.none)

        ToggleDropdown state -> 
            let
                newActions = Dict.update "sample-menu" (Maybe.map (\a -> { a | state = state })) m.actionMenus
            
            in
            ( {  m | actionMenus = newActions }, Cmd.none)
        ToggleDropdown2 state -> 
            let
                newActions = Dict.update "sample-menu2" (Maybe.map (\a -> { a | state = state })) m.actionMenus
            
            in
            ( {  m | actionMenus = newActions }, Cmd.none)

        KissSophie ->
            let
                newFiles  = Dict.insert "<3" (File { id = "<3", name = "I love you sophie", lastUpdated = Nothing, size = Nothing }) m.filesAndFolders
                newChildren = case (m.currentFolder, Dict.get "<3" newFiles) of 
                    (Just (Folder fo), Just (File fi)) ->  
                        (Dict.update 
                            fo.id
                            (\a -> case a of 
                                Just l ->  Just (File fi :: l)
                                Nothing -> Just [File fi]
                            )
                            m.children
                        )
                    _ -> m.children
            in
            ({ m | filesAndFolders = newFiles, children = newChildren }, Cmd.none)
            

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( model, Cmd.none )
        , subscriptions = 
            \m -> 
            Sub.batch (List.map (\am -> ActionMenu.subscriptions am.state am.toggleMsg) (Dict.values m.actionMenus))
        , update = update
        , view = view
        }

fileOrFolderName : FileOrFolder -> String
fileOrFolderName fileOrFolder = 
    case fileOrFolder of 
        (File file) -> file.name
        (Folder folder) -> folder.name

sortFilesOrFolders : List FileOrFolder -> String ->List FileOrFolder
sortFilesOrFolders filesOrFolders filter =
    let
        filteredList = List.filter (\fof -> String.contains filter (fileOrFolderName fof)) filesOrFolders
    in
    List.sortBy fileOrFolderName filteredList

lightGray : Attribute Msg
lightGray =
    style "color" "rgba(0, 0, 0, 0.26)"

displayMenu : Element Msg -> Element Msg
displayMenu base = Element.el [ below (column [] [Element.text "a", Element.text "b", Element.text "c"])] base

view : Model -> Html Msg
view { children, currentFolder, searchInput, filesAndFolders, actionMenus } =
    let
        (filesOrFolders, folderChain) =
            case currentFolder of
                Just (Folder f) -> case (Dict.get f.id children) of 
                    Just l -> 
                        (sortFilesOrFolders l searchInput, (createFolderChain (Folder f) filesAndFolders children []))
                    Nothing -> ([], [])
                Just (File _) -> ([], [])
                Nothing -> ([], [])
        
    in
    Html.div []
        [ FontAwesomeStyles.css
        , Element.layout [ padding 10 ]
            (column [ width fill, height (px 500), spacing 10, Border.width 1, Border.color (Element.rgba255 0 0 0 0.12), padding 10 ]
                [ row [ height (fillPortion 1) ]
                    [ Input.button [ mouseOver [ color (Element.rgba255 0 0 0 0.1) ], padding 10, alignLeft, Border.rounded 5 ]
                        { onPress = Nothing
                        , label = Element.html (Icon.viewStyled [ lightGray, FontAwesomeAttributes.xs ] levelUpAlt)
                        }
                    , row [ spacing 10, centerY, height fill ] (viewChain folderChain)
                    ]
                , row [ width fill, height (fillPortion 1), spacing 10 ]
                    [ Input.search [ width (fillPortion 2) ]
                        { onChange = \v -> Search v
                        , text = searchInput
                        , placeholder = Nothing
                        , label = Input.labelHidden "Search"
                        }
                    , Element.el [ width (fillPortion 1) ] (Element.text (String.fromInt (List.length filesOrFolders) ++ " items"))
                    , Element.row [ width (fillPortion 2) ] (Dict.foldl (\_ v acc -> actionMenu v :: acc) [] actionMenus)
                    ]
                , wrappedRow [ width fill, height (fillPortion 8), spacing 10, alignTop ] (List.map viewFileOrFolder filesOrFolders)
                ]
            )
        ]


viewChain : List FileOrFolder -> List (Element Msg)
viewChain folderChain =
    case folderChain of
        [ head ] ->
            [ folderChainRoot head ]

        head :: tail -> folderChainRoot head :: (List.map folderChainElement tail)

        [] ->
            []

folderChainRoot : FileOrFolder -> Element Msg
folderChainRoot fileOrFolder =
    case fileOrFolder of 
        (Folder f) ->
            Input.button [ mouseOver [ color (Element.rgba255 0 0 0 0.1) ], padding 10, Border.rounded 5 ]
            { onPress = Just (ClickFolderChainElement (Folder f))
            , label =
                row [ alignLeft, width fill, centerY, spacing 20, height fill ]
                    [ Element.html (Icon.viewStyled [ style "margin-right" "10px", FontAwesomeAttributes.xs ] folder)
                    , Element.el [ Font.size 14 ] (Element.text f.name)
                    ]
            }
        (File _) -> Element.none
    

folderChainElement : FileOrFolder -> Element Msg
folderChainElement fileOrFolder =
    case fileOrFolder of   
        (Folder f) ->
            Input.button [ mouseOver [ color (Element.rgba255 0 0 0 0.1) ], padding 10, Border.rounded 5 ]
                { onPress = Just (ClickFolderChainElement (Folder f))
                , label = row [ width fill, alignLeft, spacing 5, alignBottom, height fill, Font.size 14 ] [ Element.text f.name, Element.text "/" ]
                }
        (File _) -> Element.none


viewFileOrFolder : FileOrFolder -> Element Msg
viewFileOrFolder fileOrFolder =
    let
        (el, event) =
            case fileOrFolder of
                (Folder f) -> 
                    ( Element.html (Icon.viewStyled [ FontAwesomeAttributes.fa3x, lightGray ] folder)
                    , onDoubleClick (ClickFolder (Folder f)))
                (File f) -> 
                    ( Element.html (Icon.viewStyled [ FontAwesomeAttributes.fa3x, lightGray ] file)
                    , onDoubleClick (ClickFile (File f)))
    in
    column [ padding 5, spacing 5, event, alignTop ]
        [ el
        , Element.el [ Font.size 12, padding 2 ] (Element.text (fileOrFolderName fileOrFolder))
        ]

createFolderChain : FileOrFolder -> Dict String FileOrFolder -> Dict String (List FileOrFolder) -> List FileOrFolder -> List FileOrFolder
createFolderChain current fileOrFolderMap childrenMap acc =
    let
        parents : Dict String FileOrFolder
        parents = createParents fileOrFolderMap childrenMap
    in
    case Dict.get (fileOrFolderId current) parents of 
        Just f -> createFolderChain f fileOrFolderMap childrenMap (f :: acc)
        Nothing -> acc