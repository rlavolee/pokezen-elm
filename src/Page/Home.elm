module Page.Home exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Pokemon.Pokemon as Pokemon exposing (..)
import Pokemon.PokemonName as PokemonName exposing (..)
import Http
import Url.Builder as Url
import Html.Events exposing (onInput)
import Route exposing (..)

-- Model

init : ( Model, Cmd Msg )
init =
    ( Model "" [] False, Cmd.none )

type alias Model
    =
    { partialName : String
    , pokemonNames : List PokemonName
    , blink : Bool
    }

type Msg
    = FuturePokemonList ( Result Http.Error ( List PokemonName ) )
    | OnPartialNameChange String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FuturePokemonList result ->
            case result of
                Ok pokemonNames ->
                  ( { model | pokemonNames = pokemonNames, blink = False }
                  , Cmd.none
                  )

                Err _ ->
                  ( { model | blink = False }
                  , Cmd.none
                  )

        OnPartialNameChange partialName ->
            ( { model | partialName = partialName, blink = True }
            , getPokemonNames partialName
            )


view : Model -> Html Msg
view model =
  div [ class "pokedex" ] [ div [ class "content" ] [ div [ class "screen" ] [ contentView model ] ] ]


contentView : Model -> Html Msg
contentView model =
    div [ class "pure-g home" ] [ div [ class "pure-u-1" ] [ input [ placeholder "Feel free to search Pokémons", value model.partialName, onInput OnPartialNameChange ] [] ]
                                , div [ class "pure-u-1" ] [ ul [] ( List.map (\value -> li [] [ a [ href (Route.toString <| PokemonDetail <| PokemonName.toString value ) ] [ text ( PokemonName.toString value ) ] ]) model.pokemonNames ) ]
                                , ledView model
                                ]

ledView : Model -> Html msg
ledView model =
     case model.blink of
         True ->
             div [ class "led blink" ] []

         False ->
             div [ class "led" ] []

-- HTTP

getPokemonNames : String -> Cmd Msg
getPokemonNames name =
  Http.send FuturePokemonList ( Http.get ( toPokemonUrlQuery name ) PokemonName.listPokemonNameDecoder )

toPokemonUrlQuery : String -> String
toPokemonUrlQuery name =
  Url.crossOrigin "http://localhost:9000" ["pokemon"] [ Url.string "name" name ]
