module Main exposing (main)

import Browser
import Html exposing (Html, button, div, li, text, ul)
import Html.Events exposing (onClick)
import Http
import HttpBuilder
import Json.Decode
import Json.Decode.Pipeline
import RemoteData exposing (RemoteData(..), WebData)
import Task exposing (Task)


type alias Model =
    { lords : WebData (List Character) }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { lords = NotAsked }, Cmd.none )


type Msg
    = FetchClick
    | LordsFetched (WebData (List Character))


fetchHouses : Task Http.Error (List House)
fetchHouses =
    HttpBuilder.get "https://www.anapioficeandfire.com/api/houses"
        |> HttpBuilder.withQueryParam "pageSize" "2000"
        |> HttpBuilder.withQueryParam "region" "The Riverlands"
        |> HttpBuilder.withQueryParam "hasDiedOut" "false"
        |> HttpBuilder.withExpectJson (Json.Decode.list decodeHouse)
        |> HttpBuilder.toTask


fetchCurrentLord : House -> Task Http.Error Character
fetchCurrentLord { currentLord } =
    HttpBuilder.get currentLord
        |> HttpBuilder.withExpectJson decodeCharacter
        |> HttpBuilder.toTask


fetchLords : Cmd Msg
fetchLords =
    fetchHouses
        |> Task.andThen
            (\houses ->
                houses
                    |> List.filter (\house -> not <| String.isEmpty house.currentLord)
                    |> List.map fetchCurrentLord
                    |> Task.sequence
            )
        |> RemoteData.asCmd
        |> Cmd.map LordsFetched


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchClick ->
            ( { model | lords = Loading }
            , fetchLords
            )

        LordsFetched lords ->
            ( { model | lords = lords }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ case model.lords of
            NotAsked ->
                button [ onClick FetchClick ] [ text "Fetch lords" ]

            Loading ->
                text "Loading..."

            Success lords ->
                ul [] <|
                    List.map
                        (\char ->
                            li [] [ text char.name ]
                        )
                        lords

            Failure err ->
                text <| Debug.toString err
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Character =
    { url : String
    , name : String
    , culture : String
    , born : String
    , died : String
    , titles : List String
    , aliases : List String
    , father : String
    , mother : String
    , spouse : String
    , allegiances : List String
    , books : List String
    , povBooks : List String
    , tvSeries : List String
    , playedBy : List String
    }


decodeCharacter : Json.Decode.Decoder Character
decodeCharacter =
    Json.Decode.succeed Character
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "name" Json.Decode.string
        |> Json.Decode.Pipeline.required "culture" Json.Decode.string
        |> Json.Decode.Pipeline.required "born" Json.Decode.string
        |> Json.Decode.Pipeline.required "died" Json.Decode.string
        |> Json.Decode.Pipeline.required "titles" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "aliases" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "father" Json.Decode.string
        |> Json.Decode.Pipeline.required "mother" Json.Decode.string
        |> Json.Decode.Pipeline.required "spouse" Json.Decode.string
        |> Json.Decode.Pipeline.required "allegiances" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "books" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "povBooks" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "tvSeries" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "playedBy" (Json.Decode.list Json.Decode.string)


type alias House =
    { url : String
    , name : String
    , region : String
    , currentLord : String
    , swornMembers : List String
    }


decodeHouse : Json.Decode.Decoder House
decodeHouse =
    Json.Decode.succeed House
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "name" Json.Decode.string
        |> Json.Decode.Pipeline.required "region" Json.Decode.string
        |> Json.Decode.Pipeline.required "currentLord" Json.Decode.string
        |> Json.Decode.Pipeline.required "swornMembers" (Json.Decode.list Json.Decode.string)
