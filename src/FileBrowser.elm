module FileBrowser exposing (..)
import Element exposing (Element)
import Time
import Dict exposing (Dict)

fileBrowser : Element msg
fileBrowser = Element.none

type alias Structure = 
    { id : String
    , name : String
    , lastUpdated : Maybe Time.Posix
    , size : Maybe Int
    }

type FileOrFolder 
    = File Structure 
    | Folder Structure

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
    mergeDicts (Dict.foldl (\k v acc -> acc ++ [(createParentsForAPair k v)]) [] childrenDict)



