module Tree exposing (..)


type Tree a
    = Leaf a
    | Directory (List String) (List (Tree a))


type alias Config a =
    { toName : a -> String }


empty : Tree a
empty =
    Directory [] []


fromList : Config a -> List ( List String, a ) -> Tree a
fromList config xs =
    List.foldl (\( path, item ) -> insert config path item) empty xs


childrenHelper : (a -> String) -> Tree a -> List String
childrenHelper toName tree =
    case tree of
        Directory path _ ->
            path

        Leaf a ->
            [ toName a ]


children : (a -> String) -> Tree a -> List (List String)
children toName tree =
    case tree of
        Directory _ children ->
            List.map (childrenHelper toName) children

        Leaf a ->
            []



-------


inCurrentDirectory config itemPath tree =
    case tree of
        Directory childPath _ ->
            List.any (flip String.startsWith itemPath) childPath

        Leaf a ->
            False


isInChildDirectories config itemPath children =
    List.any (inCurrentDirectory config itemPath) children



------


isSameDirectory path tree =
    case tree of
        Directory dirPath _ ->
            path == dirPath

        Leaf a ->
            False


noChildrenHavePath path children =
    List.all (not << isSameDirectory path) children


insert : Config a -> List String -> a -> Tree a -> Tree a
insert config itemPath item tree =
    case itemPath of
        [] ->
            case tree of
                Directory path children ->
                    Directory path (Leaf item :: children)

                Leaf _ ->
                    tree

        curPath :: restOfPath ->
            case tree of
                Directory path children ->
                    if List.isEmpty itemPath then
                        Directory path (Leaf item :: children)
                    else if isInChildDirectories config curPath children then
                        Directory path (List.map (insert config restOfPath item) children)
                    else if noChildrenHavePath path children then
                        insert config itemPath item (Directory path (Directory (curPath :: path) [] :: children))
                    else
                        tree

                Leaf _ ->
                    tree
