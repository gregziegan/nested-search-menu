module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import MultiwayTree as Tree exposing (Tree(Tree))
import MultiwayTreeZipper as Zipper exposing (Breadcrumbs, Context(Context), Zipper, goToChild, goToRoot, goUp)


type alias Model =
    { items : Maybe (Zipper SearchResult)
    }


type alias SearchJson =
    { id : String
    , path : String
    }


type alias FileInfo =
    { id : String
    , name : String
    }


type SearchResult
    = Directory String
    | File FileInfo


(&>) =
    flip Maybe.andThen


simpleTree =
    Tree (Directory "/")
        [ Tree (Directory "dev")
            [ Tree (Directory "cool_project")
                [ Tree (File { id = "2", name = "start.sh" }) [] ]
            , Tree (File { id = "3", name = "README.md" }) []
            ]
        , Tree (File { id = "1", name = ".bash_profile" }) []
        ]


init : Model
init =
    { items =
        Just ( simpleTree, [] )
            &> goToRoot
    }


type Msg
    = MoveToDirectory Int
    | GoUp


update : Msg -> Model -> Model
update msg model =
    case msg of
        MoveToDirectory index ->
            { model
                | items =
                    model.items
                        &> goToChild index
            }

        GoUp ->
            { model | items = model.items &> goUp }


view : Model -> Html Msg
view model =
    case model.items of
        Just ( tree, breadcrumbs ) ->
            div []
                [ viewBreadCrumbs breadcrumbs
                , viewSearchResults tree
                ]

        Nothing ->
            text ""


viewBreadCrumbs : Breadcrumbs SearchResult -> Html Msg
viewBreadCrumbs breadcrumbs =
    List.map viewContext breadcrumbs
        |> div []


viewContext : Context SearchResult -> Html Msg
viewContext (Context lastNode _ _) =
    case lastNode of
        Directory name ->
            p [ onClick GoUp ] [ text name ]

        _ ->
            text ""


viewSearchResults searchTree =
    Tree.children searchTree
        |> List.indexedMap viewItem
        |> ul [ class "menu" ]



-- viewItem : Int -> Tree SearchResult -> Html Msg


viewItem index tree =
    case Tree.datum tree of
        File { name } ->
            li [ class "menu-item" ] [ text name ]

        Directory name ->
            li [ class "menu-item menu-directory", onClick (MoveToDirectory index) ]
                [ text name, text ">" ]


items : List ( List String, SearchResult )
items =
    [ { id = "3", path = ".bash_profile" }
    , { id = "1", path = "dev/cool_project/start.sh" }
    , { id = "2", path = "dev/README.md" }
    ]
        |> List.map (\item -> ( String.split "/" item.path, toSearchResult item ))
        |> Debug.log "items"


name : SearchJson -> String
name searchJson =
    String.split "/" searchJson.path
        |> List.reverse
        |> List.head
        |> Maybe.withDefault "NOPE!"


toSearchResult : SearchJson -> SearchResult
toSearchResult searchJson =
    (File { id = searchJson.id, name = name searchJson })



-- treeConfig : FileTree.Config SearchResult
-- treeConfig =
-- { toName = .name }


main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }
