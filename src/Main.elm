module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Url.Parser exposing (Parser, (</>), int, map, oneOf, s, string)
import Route exposing (Route)
import Pokemon.PokemonName as PokemonName exposing (..)
import Page.Home exposing (..)
import Page.PokemonDetail exposing (..)



-- MAIN


main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }



-- MODEL

type alias Model
    =
    { navKey : Nav.Key
    , subModel : SubModel
    }

type SubModel
    = Redirect
    | Home Page.Home.Model
    | Detail Page.PokemonDetail.Model


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    Model key Redirect
        |> changeRouteTo ( Route.fromUrl url )



-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | GotHomeMsg Page.Home.Msg
  | GotPokemonDetailMsg Page.PokemonDetail.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case ( msg, model.subModel ) of
    ( LinkClicked urlRequest, _ ) ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.navKey (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    ( UrlChanged url, _ ) ->
      changeRouteTo (Route.fromUrl url) model

    ( GotHomeMsg subMsg, Home home ) ->
        Page.Home.update subMsg home
            |> updateSubModelWith Home GotHomeMsg model

    ( GotPokemonDetailMsg subMsg, Detail pokemon ) ->
        Page.PokemonDetail.update subMsg pokemon
            |> updateSubModelWith Detail GotPokemonDetailMsg model

    ( _, _ ) ->
        ( model, Cmd.none )



updateSubModelWith : (subModel -> SubModel) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateSubModelWith toModel toMsg model ( subModel, subCmd ) =
    ( { model | subModel = toModel subModel }
    , Cmd.map toMsg subCmd
    )


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo route model =
    case route of
        Nothing ->
            (model , Cmd.none)

        Just Route.Home ->
            Page.Home.init
                |> updateSubModelWith Home GotHomeMsg model

        Just ( Route.PokemonDetail name ) ->
            PokemonName.fromString name
                |> Page.PokemonDetail.init
                |> updateSubModelWith Detail GotPokemonDetailMsg model


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW

--view : Model -> Browser.Document Msg
--view model =
--  { title = "URL Interceptor"
--  , body =
--      [ text "The current URL is: "
--      , b [] [ text (Url.toString model.url) ]
--      , ul []
--          [ viewLink "/"
--          , viewLink "/profile"
--          , viewLink "/reviews/the-century-of-the-self"
--          , viewLink "/reviews/public-opinion"
--          , viewLink "/reviews/shah-of-shahs"
--          ]
--      ]
--  }

view : Model -> Browser.Document Msg
view model =
  case model.subModel of
      Redirect ->
          { title = "NotFound"
          , body =
              [ text "NotFound"
              , ul []
                  [ viewLink "/"
                  , viewLink "#/pokemon/pikachu"
                  ]
              ]
          }

      Home home ->
          Page.Home.view home
            |> Html.map GotHomeMsg
            |> toDocument ""

      Detail pokemonDetail ->
          Page.PokemonDetail.view pokemonDetail
            |> Html.map GotPokemonDetailMsg
            |> toDocument ( PokemonName.toString pokemonDetail.name )


toDocument : String -> Html msg -> Browser.Document msg
toDocument title content =
    { title = if ( String.isEmpty title ) then "Pokezen" else title ++ " | Pokezen"
    , body = [ content ]
    }

viewLink : String -> Html msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]