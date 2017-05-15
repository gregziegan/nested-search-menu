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
    { items = toSearchTree items
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


viewItem resultName =
    li [ class "menu-item" ] [ text resultName ]


items : List SearchJson
items =
    [ { id = "3", path = "bash_profile" }
    , { id = "2", path = "dev/cool_project" }
    , { id = "1", path = "dev/cool_project/start.sh" }
    ]


name searchJson =
    String.split "::" searchJson.path
        |> List.reverse
        |> List.head
        |> Maybe.withDefault "NOPE!"


toCurPath searchJson =
    searchJson.path
        |> String.split "/"
        |> List.head
        |> Maybe.withDefault "NOPE!"


nextDirectory searchJson =
    { searchJson
        | path =
            searchJson.path
                |> String.split "/"
                |> List.drop 1
                |> String.join "/"
    }


toSearchResult : SearchJson -> SearchResult
toSearchResult searchJson =
    { id = searchJson.id, name = name searchJson }


toPath searchJson =
    if String.contains "/" searchJson.path then
        searchJson.path
            |> String.split "/"
            |> List.head
            |> Maybe.withDefault "NOPE"
    else
        ""


treeConfig : Tree.Config SearchJson SearchResult
treeConfig =
    { toPath = toPath
    , toCurPath = toCurPath
    , toName = name
    , nextDirectory = nextDirectory
    , toFile = toSearchResult
    }


toSearchTree : List SearchJson -> Tree SearchResult
toSearchTree items =
    Tree.fromList treeConfig items


main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }
