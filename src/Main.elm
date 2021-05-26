module Main exposing (..)

import Browser

import List
import Html exposing (Html)
import Element exposing (Element, row, width, fill, height)
import Time
import Element exposing (alignRight, alignLeft)
import Element.Border as Border
import Element exposing (px)
import Element exposing (fillPortion)
import Element exposing (column)
import Element exposing (spacing)
import Element exposing (spacingXY)
import Element exposing (padding)
import Element exposing (table)
import FontAwesome.Styles as FontAwesomeStyles
import FontAwesome.Solid exposing (folder, levelUpAlt)
import FontAwesome.Icon as Icon exposing (Icon)
import FontAwesome.Attributes as FontAwesomeAttributes
import Html.Attributes exposing (style)
import Element exposing (Color)
import Element.Input as Input
import Element.Font as Font
import Html exposing (Attribute)
import Element exposing (centerY)
import Element exposing (alignBottom)
import Element exposing (mouseOver)
import Element.Background exposing (color)
import List exposing (tail)
import FontAwesome.Solid exposing (file)
import Element exposing (rgb255)
import Element exposing (alignTop)
import Element exposing (spaceEvenly)
import Element exposing (wrappedRow)
type alias File = 
    { id : String
    , name : String
    , lastUpdated : Maybe Time.Posix
    , size: Maybe Int
    }

type Folder =
  Folder { id : String
  , name : String
  , size : Maybe Int
  , lastUpdated : Maybe Time.Posix
  , files : List File
  , folders : List Folder
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

type alias FileBrowserModel = 
    { currentFolder : Maybe Folder -- This can be empty
    , data : Maybe Folder -- This can be useful to have a global folder state for static file structures
    , searchInput : String
    , folderChain : FolderChain
    }


someFolder : Folder
someFolder = Folder 
  { id = "root"
  , name = "root"
  , size = Just 10
  , lastUpdated = Nothing
  , folders = 
    [ Folder
      { id = "folder1"
      , name = "bsomefolder"
      , size = Nothing
      , lastUpdated = Nothing
      , files = 
        [ { id = "data4.csv", name = "data4.csv", lastUpdated = Nothing, size = Nothing } ]
      , folders = []}
    ]
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

model: FileBrowserModel
model = 
  { currentFolder = Just someFolder
  , searchInput = ""
  , data = Nothing
  , folderChain = [someFolder]
  }

main : Program () FileBrowserModel Msg
main =
  Browser.element 
    { init = \_ -> (model, Cmd.none)
    , subscriptions = \_ -> (Sub.none)
    , update = update
    , view = view }

type Msg = Search String

update: Msg -> FileBrowserModel -> ( FileBrowserModel, Cmd Msg )
update msg m =
  case msg of
    Search v -> Debug.log v ({m | searchInput = v}, Cmd.none)
        
monthToString: Time.Month -> String
monthToString month = case month of
  Time.Jan -> "Janvier"
  Time.Feb -> "Février"
  Time.Mar -> "Mars"
  Time.Apr -> "Avril"
  Time.May -> "Mai"
  Time.Jun -> "Juin"
  Time.Jul -> "Juillet"
  Time.Aug -> "Aout"
  Time.Sep -> "Septembre"
  Time.Oct -> "Octobre"
  Time.Nov -> "Novembre"
  Time.Dec -> "Décembre"

type alias FolderChain = List Folder

viewChain : FolderChain -> List (Element Msg)
viewChain folderChain = 
  case folderChain of 
      [head] -> [folderChainRoot head]
      head :: tail -> List.append [folderChainRoot head] (List.map folderChainElement tail)
      [] -> []

folderChainElement : Folder -> Element Msg
folderChainElement (Folder folder) = 
  Input.button [ mouseOver [ color (Element.rgba255 0 0 0 0.10) ], padding 10, Border.rounded 5 ] 
    { onPress = Nothing
    , label = row [ width fill, alignLeft, spacing 5, alignBottom, height fill, Font.size 14] [Element.text (folder.name), Element.text "/"]
    }

folderChainRoot : Folder -> Element Msg
folderChainRoot (Folder rootFolder) = 
  Input.button [ mouseOver [ color (Element.rgba255 0 0 0 0.10) ], padding 10, Border.rounded 5 ]
    { onPress = Nothing
    , label = 
      row [ alignLeft, width fill, centerY, spacing 20, height fill ]
      [ Element.html (Icon.viewStyled [style "margin-right" "10px", FontAwesomeAttributes.xs ] folder)
      , Element.el [Font.size 14] (Element.text rootFolder.name)
      ]
    }

lightGray: Attribute Msg
lightGray = style "color" "rgba(0, 0, 0, 0.26)"

fileToFileOrFolder : File -> FileOrFolder
fileToFileOrFolder { id, name, lastUpdated, size } = 
  { id = id
  , name = name
  , lastUpdated = lastUpdated
  , size = size
  , files = []
  , folders = []
  , isFolder = False }

folderToFileOrFolder : Folder -> FileOrFolder
folderToFileOrFolder (Folder { id, name, lastUpdated, size, folders, files }) = 
  { id = id
  , name = name
  , lastUpdated = lastUpdated
  , size = size
  , files = files
  , folders = folders
  , isFolder = True }


sortFilesAndFolders : List File -> List Folder -> String -> List FileOrFolder
sortFilesAndFolders files folders filter = 
  let
    filesFiltered : List File
    filesFiltered = List.filter (\f -> String.contains filter f.name) files 

    foldersFiltered : List Folder
    foldersFiltered = List.filter (\(Folder f) -> String.contains filter f.name) folders 
  in
  List.sortBy .name (List.append (List.map fileToFileOrFolder filesFiltered) (List.map folderToFileOrFolder foldersFiltered))

viewFileOrFolder : FileOrFolder -> Element Msg
viewFileOrFolder fileOrFolder = 
  let
    el = 
      if (fileOrFolder.isFolder) then
        Element.html (Icon.viewStyled [ FontAwesomeAttributes.fa4x, lightGray ] folder)
      else
        Element.html (Icon.viewStyled [ FontAwesomeAttributes.fa4x, lightGray ] file)
  in
    column [ padding 5, spacing 5] 
      [ el
      , Element.el [ Font.size 12, padding 2 ] (Element.text fileOrFolder.name)]

view: FileBrowserModel -> Html Msg
view { folderChain, currentFolder, searchInput } =
  let
    (numberOfItems, filesOrFolders) = case currentFolder of
      Just (Folder folder) -> (List.length folder.files, sortFilesAndFolders folder.files folder.folders searchInput)
      Nothing -> (0, []) 
  in
  Html.div [] 
    [  FontAwesomeStyles.css, 
    Element.layout [ padding 10 ] 
    (
      column [ width fill, height (px 500), spacing 10, Border.width 1, Border.color (Element.rgba255 0 0 0 0.12), padding 10 ] 
        [ row [ height (fillPortion 1) ] 
          [ Input.button [ mouseOver [ color (Element.rgba255 0 0 0 0.10) ], padding 10, alignLeft, Border.rounded 5 ]
            { onPress = Nothing
            , label = Element.html (Icon.viewStyled [lightGray, FontAwesomeAttributes.xs ] levelUpAlt)
            }
          , row [ spacing 10, centerY, height fill ] (viewChain folderChain)
          ]
        , row [ width fill, height (fillPortion 1), spacing 10] 
          [ Input.search [ width (fillPortion 2) ]
              { onChange = \v -> Search v
              , text = searchInput
              , placeholder = Nothing
              , label = Input.labelHidden "Search"
              }
          , Element.el [ width (fillPortion 1) ] (Element.text ((String.fromInt numberOfItems) ++ " items"))
          , Element.el [ width (fillPortion 1) ] (Element.el [alignRight] (Element.text "Action menu"))
          ]
        , wrappedRow [ width fill, height (fillPortion 8), spacing 10, alignTop] (List.map viewFileOrFolder filesOrFolders)
        
          -- [ (table [] 
          --   { data = fileBrowser.files
          --   , columns = 
          --     [ { header = Element.text "Folder"
          --       , width = fill
          --       , view =
          --             \fileOrFolder ->
          --               case fileOrFolder of
          --                 File file ->
          --                   Element.text file.name
          --                 Folder folder ->
          --                   Element.text folder.name
          --       },
          --       { header = Element.text "Id"
          --       , width = fill
          --       , view =
          --             \fileOrFolder ->
          --               case fileOrFolder of
          --                 File file ->
          --                   Element.text file.id
          --                 Folder folder ->
          --                   Element.text folder.id
          --       },
          --       { header = Element.text "Last Updated At"
          --       , width = fill
          --       , view =
          --             \fileOrFolder ->
          --               case fileOrFolder of
          --                 File file ->
          --                   Element.text (monthToString (Time.toMonth Time.utc file.lastUpdated))
          --                 Folder folder ->
          --                   Element.text (monthToString (Time.toMonth Time.utc folder.lastUpdated))
          --       },
          --       { header = Element.text "Size"
          --       , width = fill
          --       , view =
          --             \fileOrFolder ->
          --               case fileOrFolder of
          --                 File file ->
          --                   Element.text (String.fromInt file.size)
          --                 Folder folder ->
          --                   Element.text (String.fromInt folder.size)
          --       }
          --     ]
          --   })
        ]
    )
    ]
  
  -- div []
  --   [ button [ onClick (Add (File { id = "new folder", name = "new file"})) ] [ text "New file" ]
  --   , button [ onClick (Add (Folder { id = "new folder", name = "new folder"})) ] [ text "New folder" ]
  --   , ul [] (List.map viewFile m.files)
  --   ]

