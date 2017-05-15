module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Tree exposing (Tree)


type alias Model =
    { items : Tree SearchResult
    , curDirectory : String
    }


type alias SearchJson =
    { id : String
    , path : String
    }


type alias SearchResult =
    { id : String
    , name : String
    }


init : Model
init =
    { items = Tree.fromList treeConfig items
    , curDirectory = ""
    }


type Msg
    = Search


update : Msg -> Model -> Model
update msg model =
    model


view : Model -> Html Msg
view model =
    div []
        [ viewSearchResults model.curDirectory model.items
        ]


viewSearchResults curDirectory searchTree =
    Tree.children .name searchTree
        |> List.map viewItem
        |> ul [ class "menu" ]


viewItem itemPath =
    case itemPath of
        [] ->
            text ""

        [ name ] ->
            li [ class "menu-item" ] [ text name ]

        curDir :: rest ->
            li [ class "menu-item menu-directory" ] [ text curDir ]


items : List ( List String, SearchResult )
items =
    [ { id = "3", path = "bash_profile" }
    , { id = "2", path = "dev/cool_project" }
    , { id = "1", path = "dev/cool_project/start.sh" }
    ]
        |> List.map (\item -> ( toPath item, toSearchResult item ))


name : SearchJson -> String
name searchJson =
    String.split "/" searchJson.path
        |> List.reverse
        |> List.head
        |> Maybe.withDefault "NOPE!"


toSearchResult : SearchJson -> SearchResult
toSearchResult searchJson =
    { id = searchJson.id, name = name searchJson }


toPath searchJson =
    if String.contains "/" searchJson.path then
        searchJson.path
            |> String.split "/"
    else
        []


treeConfig : Tree.Config SearchResult
treeConfig =
    { toName = .name }


main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }
