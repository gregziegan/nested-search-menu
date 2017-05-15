module Tree exposing (..)


type Tree a
    = Leaf a
    | Directory String (List (Tree a))


type alias Config data file =
    { toPath : data -> String
    , toCurPath : data -> String
    , toName : data -> String
    , nextDirectory : data -> data
    , toFile : data -> file
    }


empty : Tree a
empty =
    Directory "" []


fromList : Config data file -> List data -> Tree file
fromList config xs =
    List.foldl (insert config) empty xs


childrenHelper : (file -> String) -> Tree file -> String
childrenHelper toName tree =
    case tree of
        Directory path _ ->
            path

        Leaf file ->
            toName file


children : (file -> String) -> Tree file -> List String
children toName tree =
    case tree of
        Directory _ children ->
            List.map (childrenHelper toName) children

        Leaf file ->
            []



--


inCurrentDirectory config item tree =
    case tree of
        Directory childPath _ ->
            String.startsWith childPath (config.toPath item)

        Leaf file ->
            False


isInChildDirectories config item children =
    List.any (inCurrentDirectory config item) children



------


isSameDirectory path tree =
    case tree of
        Directory dirPath _ ->
            path == dirPath

        Leaf file ->
            False


noChildrenHavePath path children =
    List.all (not << isSameDirectory path) children


insert : Config data file -> data -> Tree file -> Tree file
insert config item tree =
    case tree of
        Directory path children ->
            if (config.toPath item) == "" then
                Directory path ((Leaf (config.toFile item)) :: children)
            else if isInChildDirectories config item children then
                Directory path (List.map (insert config (config.nextDirectory item)) children)
            else if noChildrenHavePath path children then
                insert config item (Directory path (Directory (config.toCurPath item) [] :: children))
            else
                tree

        Leaf _ ->
            tree
