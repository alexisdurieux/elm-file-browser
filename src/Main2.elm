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


type alias Structure = 
    { id : String
    , name : String
    , lastUpdated : Maybe Time.Posix
    , size : Maybe Int
    }

type FileOrFolder 
    = File Structure 
    | Folder Structure

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

mergeDicts : List (Dict String FileOrFolder) -> Dict String FileOrFolder
mergeDicts dicts = List.foldl Dict.union Dict.empty dicts

fileOrFolderId : FileOrFolder -> String
fileOrFolderId fileOrFolder =
    case fileOrFolder of 
        (File file) -> file.id
        (Folder folder) -> folder.id


createParents : Dict String FileOrFolder -> Dict String (List FileOrFolder) -> Dict String FileOrFolder
createParents allFiles childrenDict =
    let
        createParentsForAPair : String -> List FileOrFolder -> Dict String FileOrFolder
        createParentsForAPair parentKey childrenList =
            case (Dict.get parentKey allFiles) of
                Just fileOrFolder ->
                    List.foldl (\fof acc-> Dict.insert (fileOrFolderId fof) fileOrFolder acc) Dict.empty childrenList
                Nothing -> Dict.empty
            
    in
    mergeDicts (Dict.foldl (\k v acc -> List.append acc [(createParentsForAPair k v)]) [] childrenDict)


theParents : Dict String FileOrFolder
theParents = 
    createParents theFilesAndFolders theChildren

-- Both filesAndFolders need to be served. Parents can be updated on the fly
type alias Model = 
    { filesAndFolders : Dict String FileOrFolder
    , currentFolder : Maybe FileOrFolder 
    , children : Dict String (List FileOrFolder) --> File should'nt have children but don't know how to best design the data structure yet
    , parents : Dict String FileOrFolder 
    , searchInput : String
    }

type Msg
    = Search String
    | ClickFolder FileOrFolder
    | ClickFile FileOrFolder
    | ClickFolderChainElement FileOrFolder


model : Model 
model = 
    { filesAndFolders = theFilesAndFolders
    , currentFolder = Just folderA
    , children = theChildren
    , parents = theParents
    , searchInput = ""
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

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
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

view : Model -> Html Msg
view { children, currentFolder, searchInput, filesAndFolders } =
    let
        (filesOrFolders, folderChain) =
            case currentFolder of
                Just (Folder f) -> case (Dict.get f.id children) of 
                    Just l -> 
                        (sortFilesOrFolders l searchInput, (createFolderChain (Folder f) filesAndFolders children []))
                    Nothing -> ([], [])
                Just (File _) -> ([], [])
                Nothing -> ([], [])
        
        a = Debug.log "" folderChain
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
                    , Element.el [ width (fillPortion 1) ] (Element.el [ alignRight ] (Element.text "Action menu"))
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

        head :: tail ->
            List.append [ folderChainRoot head ] (List.map folderChainElement tail)

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
                    ( Element.html (Icon.viewStyled [ FontAwesomeAttributes.fa4x, lightGray ] folder)
                    , onClick (ClickFolder (Folder f)))
                (File f) -> 
                    ( Element.html (Icon.viewStyled [ FontAwesomeAttributes.fa4x, lightGray ] file)
                    , onClick (ClickFile (File f)))
    in
    column [ padding 5, spacing 5, event ]
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
        Just f -> createFolderChain f fileOrFolderMap childrenMap (List.append [f] acc)
        Nothing -> acc