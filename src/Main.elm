module Main exposing (..)

import Browser
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
import Html exposing (Attribute, Html)
import Html.Attributes exposing (style)
import List exposing (tail)
import Time
import Dict exposing (Dict)
import Array exposing (Array)
import FontAwesome.Solid exposing (child)


type alias File =
    { id : String
    , name : String
    , lastUpdated : Maybe Time.Posix
    , size : Maybe Int
    }


type Folder
    = Folder
        { id : String
        , name : String
        , size : Maybe Int
        , lastUpdated : Maybe Time.Posix
        , files : List File
        }


type alias FileOrFolder =
    { id : String
    , name : String
    , size : Maybe Int
    , lastUpdated : Maybe Time.Posix
    , files : List File
    , folders : List Folder
    , isFolder : Bool
    }

type alias FolderStructure = 
    { children : Dict String (List Folder)
    , parents : Dict String Folder
    }

type alias FileBrowserModel =
    { currentFolder : Maybe Folder -- This can be empty
    , data : Maybe Folder -- This can be useful to have a global folder state for static file structures
    , searchInput : String
    , folderChain : FolderChain
    , folderStructure : FolderStructure
    }

rootFolder = Folder
        { id = "root"
        , name = "root"
        , size = Just 10
        , lastUpdated = Nothing
        , files =
            [ { id = "data1.csv", name = "data1.csv", lastUpdated = Nothing, size = Nothing }
            , { id = "data3.csv", name = "data3.csv", lastUpdated = Nothing, size = Nothing }
            , { id = "data2.csv", name = "data2.csv", lastUpdated = Nothing, size = Nothing }
            , { id = "data3.csv", name = "data3.csv", lastUpdated = Nothing, size = Nothing }
            , { id = "data2.csv", name = "data2.csv", lastUpdated = Nothing, size = Nothing }
            , { id = "data3.csv", name = "data3.csv", lastUpdated = Nothing, size = Nothing }
            , { id = "data2.csv", name = "data2.csv", lastUpdated = Nothing, size = Nothing }
            , { id = "data3.csv", name = "data3.csv", lastUpdated = Nothing, size = Nothing }
            , { id = "data2.csv", name = "alexis.csv", lastUpdated = Nothing, size = Nothing }
            , { id = "data3.csv", name = "data3.csv", lastUpdated = Nothing, size = Nothing }
            , { id = "data2.csv", name = "data2.csv", lastUpdated = Nothing, size = Nothing }
            , { id = "data3.csv", name = "data3.csv", lastUpdated = Nothing, size = Nothing }
            , { id = "data2.csv", name = "data2.csv", lastUpdated = Nothing, size = Nothing }
            , { id = "data3.csv", name = "data3.csv", lastUpdated = Nothing, size = Nothing }
            , { id = "data2.csv", name = "b.csv", lastUpdated = Nothing, size = Nothing }
            , { id = "data3.csv", name = "data3.csv", lastUpdated = Nothing, size = Nothing }
            , { id = "data2.csv", name = "data2.csv", lastUpdated = Nothing, size = Nothing }
            , { id = "data3.csv", name = "data3.csv", lastUpdated = Nothing, size = Nothing }
            , { id = "data2.csv", name = "data2.csv", lastUpdated = Nothing, size = Nothing }
            ]
        }

childFolder = 
    Folder
    { id = "folder1"
    , name = "bsomefolder"
    , size = Nothing
    , lastUpdated = Nothing
    , files =
        [ { id = "data4.csv", name = "data4.csv", lastUpdated = Nothing, size = Nothing } ]
    }

theFolders : Array Folder 
theFolders = 
    Array.fromList 
    [ rootFolder
    , childFolder
    ]   

structure = 
    { parents =  Dict.fromList [(rootFolder.id, [childFolder])]
    , children = Dict.fromList [(childFolder.id, rootFolder)] }

model : FileBrowserModel
model =
    { currentFolder = Array.get(0) theFolders
    , searchInput = ""
    , data = Nothing
    , folderChain = [ ]
    , folderStructure = structure 
    }


main : Program () FileBrowserModel Msg
main =
    Browser.element
        { init = \_ -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


type Msg
    = Search String
    | ClickFolder Folder
    | ClickFile File
    | ClickFolderChainElement Folder


update : Msg -> FileBrowserModel -> ( FileBrowserModel, Cmd Msg )
update msg m =
    case msg of
        Search v ->
            ( { m | searchInput = v }, Cmd.none )

        ClickFolder folder ->
            ( { m | currentFolder = Just folder, folderChain = m.folderChain ++ [folder] }, Cmd.none )

        ClickFile file ->
            ( m, Cmd.none )

        ClickFolderChainElement folder ->
            ( { m | currentFolder = Just folder, folderChain = m.folderChain ++ [folder] }, Cmd.none )

monthToString : Time.Month -> String
monthToString month =
    case month of
        Time.Jan ->
            "Janvier"

        Time.Feb ->
            "Février"

        Time.Mar ->
            "Mars"

        Time.Apr ->
            "Avril"

        Time.May ->
            "Mai"

        Time.Jun ->
            "Juin"

        Time.Jul ->
            "Juillet"

        Time.Aug ->
            "Aout"

        Time.Sep ->
            "Septembre"

        Time.Oct ->
            "Octobre"

        Time.Nov ->
            "Novembre"

        Time.Dec ->
            "Décembre"


type alias FolderChain =
    List Folder


viewChain : FolderChain -> List (Element Msg)
viewChain folderChain =
    case folderChain of
        [ head ] ->
            [ folderChainRoot head ]

        head :: tail -> folderChainRoot head :: (List.map folderChainElement tail)

        [] ->
            []


folderChainElement : Folder -> Element Msg
folderChainElement (Folder folder) =
    Input.button [ mouseOver [ color (Element.rgba255 0 0 0 0.1) ], padding 10, Border.rounded 5 ]
        { onPress = Just (ClickFolderChainElement (Folder folder))
        , label = row [ width fill, alignLeft, spacing 5, alignBottom, height fill, Font.size 14 ] [ Element.text folder.name, Element.text "/" ]
        }


folderChainRoot : Folder -> Element Msg
folderChainRoot (Folder folder) =
    Input.button [ mouseOver [ color (Element.rgba255 0 0 0 0.1) ], padding 10, Border.rounded 5 ]
        { onPress = Just (ClickFolderChainElement (Folder folder))
        , label =
            row [ alignLeft, width fill, centerY, spacing 20, height fill ]
                [ Element.html (Icon.viewStyled [ style "margin-right" "10px", FontAwesomeAttributes.xs ] folder)
                , Element.el [ Font.size 14 ] (Element.text rootFolder.name)
                ]
        }


lightGray : Attribute Msg
lightGray =
    style "color" "rgba(0, 0, 0, 0.26)"


fileToFileOrFolder : File -> FileOrFolder
fileToFileOrFolder { id, name, lastUpdated, size } =
    { id = id
    , name = name
    , lastUpdated = lastUpdated
    , size = size
    , files = []
    , folders = []
    , isFolder = False
    }


folderToFileOrFolder : Folder -> FileOrFolder
folderToFileOrFolder (Folder { id, name, lastUpdated, size, folders, files }) =
    { id = id
    , name = name
    , lastUpdated = lastUpdated
    , size = size
    , files = files
    , folders = folders
    , isFolder = True
    }


sortFilesAndFolders : List File -> List Folder -> String -> List FileOrFolder
sortFilesAndFolders files folders filter =
    let
        filesFiltered : List File
        filesFiltered =
            List.filter (\f -> String.contains filter f.name) files

        foldersFiltered : List Folder
        foldersFiltered =
            List.filter (\(Folder f) -> String.contains filter f.name) folders
    in
    List.sortBy .name ((List.map fileToFileOrFolder filesFiltered) ++ (List.map folderToFileOrFolder foldersFiltered))


viewFileOrFolder : Folder -> Element Msg
viewFolder fileOrFolder =
    let
        (el, event) =
            if fileOrFolder.isFolder then
                ( Element.html (Icon.viewStyled [ FontAwesomeAttributes.fa4x, lightGray ] folder)
                , onClick (ClickFolder )
                )
            else
                ( Element.html (Icon.viewStyled [ FontAwesomeAttributes.fa4x, lightGray ] file)
                , onClick (ClickFile { id = fileOrFolder.id, name = fileOrFolder.name, lastUpdated = fileOrFolder.lastUpdated, size = fileOrFolder.size })
                )
    in
    column [ padding 5, spacing 5, event ]
        [ el
        , Element.el [ Font.size 12, padding 2 ] (Element.text fileOrFolder.name)
        ]


view : FileBrowserModel -> Html Msg
view { folderChain, currentFolder, searchInput } =
    let
        filesOrFolders =
            case currentFolder of
                Just (Folder folder) -> sortFilesAndFolders folder.files folder.folders searchInput

                Nothing -> []
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
