module Page.PokemonDetail exposing (view, init, update, getPokemonDetail, Msg, Model)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Url.Builder as Url
import Pokemon.PokemonName as PokemonName exposing (PokemonName, toString)
import Pokemon.Pokemon as Pokemon exposing (..)
import Http
import Svg exposing (Svg, rect, svg)
import Svg.Attributes exposing (width, viewBox, height, x, y, rx, ry, fill, fontFamily)
import Array
import Html.Events exposing (onClick)

-- MODEL

type alias Model
    =
    { name : PokemonName
    , pokemon : Maybe Pokemon
    , like : Maybe PokemonLike
    , stats : List PokemonStats
    }

type alias SvgStat
    =
    { stats : List Stat
    , color : String
    }

init : PokemonName -> ( Model, Cmd Msg )
init pokemonName =
    ( Model pokemonName Nothing Nothing []
    , Cmd.batch [ getPokemonDetail pokemonName
                , getPokemonLike pokemonName
                , getPokemonStats pokemonName
                ]
    )


-- UPDATE

type Msg
    = FuturePokemon ( Result Http.Error Pokemon )
    | FuturePokemonLike ( Result Http.Error PokemonLike )
    | FuturePokemonStats ( Result Http.Error ( List PokemonStats ) )
    | Like
    | Dislike
    | FutureLike ( Result Http.Error String )
    | FutureDislike ( Result Http.Error String )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FuturePokemon result ->
            case result of
                Ok newPokemon ->
                  ( { model | pokemon = Just newPokemon }
                  , Cmd.none
                  )

                Err _ ->
                  ( model , Cmd.none)

        FuturePokemonLike result ->
            case result of
                Ok newLike ->
                  ( { model | like = Just newLike }
                  , Cmd.none
                  )

                Err _ ->
                  ( model , Cmd.none)

        FuturePokemonStats result ->
            case result of
                Ok newStats ->
                  ( { model | stats = newStats }
                  , Cmd.none
                  )

                Err _ ->
                  ( model , Cmd.none)

        Like ->
            ( model , putPokemonLike model.name)

        Dislike ->
            ( model , putPokemonDislike model.name)

        FutureLike result ->
            case result of
                Ok _ ->
                  ( model
                  , getPokemonLike model.name
                  )

                Err _ ->
                  ( model , Cmd.none)

        FutureDislike result ->
            case result of
                Ok _ ->
                  ( model
                  , getPokemonLike model.name
                  )

                Err _ ->
                  ( model , Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
     div [ class "pokedex" ] [ div [ class "content" ] [ div [ class "screen" ] ( detailView model ), ledView model.pokemon, likeButtonsView ]
                             ]


detailView : Model -> List (Html msg)
detailView model =
    ( pokemonView model ) ++ ( statsView model.pokemon model.stats )

ledView : Maybe Pokemon -> Html msg
ledView model =
     case model of
         Nothing ->
             div [ class "led blink" ] []

         Just _ ->
             div [ class "led" ] []

likeButtonsView : Html Msg
likeButtonsView =
    div [class "like-area"] [ i [ class "em-svg em-heavy_minus_sign", onClick Dislike] [], i [ class "em-svg em-heavy_plus_sign", onClick Like] [] ]

pokemonView : Model -> List (Html msg)
pokemonView model =
    case model.pokemon of
        Nothing ->
            [ text "" ]

        Just pokemon ->
            [ div [ class "pure-g" ]
                [ div [ class "pure-u-1-2" ] [ h1 [] [ text <| String.toUpper pokemon.name ]
                         , ul [] [ li [] [ text ( "Weight " ++ String.fromInt pokemon.weight ) ]
                                 , li [] [ text ( "Height " ++ String.fromInt pokemon.height ) ]
                                 , li [] [ text <| likeView model.like ]
                                 ]
                         ]
                , div [ class "pure-u-1-2" ] [ img [ src <| Maybe.withDefault "plop" pokemon.sprites.frontDefault ] [] ]
                ]
            ]

likeView : Maybe PokemonLike -> String
likeView model =
    case model of
        Nothing ->
            "nobody likes him yet"

        Just like ->
            if like.like <= 0 then "nobody likes him yet"
            else ( String.fromInt like.like ) ++ " people like him"


statsView : Maybe Pokemon -> List PokemonStats -> List ( Html msg )
statsView pokemon stats =
    case ( pokemon, stats ) of
        ( Just p, h::_ ) ->
            let
                svgStats = toSvgStats stats p
            in
                [ svg
                      [ width "500"
                      , height "500"
                      , viewBox "0 0 500 500"
                      ]
                      ( List.indexedMap statView svgStats
                        |> List.concat )
                ]


        (_, _) ->
            [ text "" ]

toSvgStats : List PokemonStats -> Pokemon -> List SvgStat
toSvgStats stats pokemon =
    List.foldl (\x acc -> SvgStat x.average ( colorFromType x.pType ) :: acc ) [SvgStat pokemon.stats "cadetblue"] stats
        |> List.reverse

statView : Int -> SvgStat -> List ( Svg msg )
statView index svgStat  =
  List.indexedMap (\i stat ->
    let
        heightBar = stat.baseStat
        xBar = index * 110 + 150
        yBar = i * 25
        yText = yBar + 15
    in
        [ rect [x (String.fromInt xBar), y ( String.fromInt yBar) , width "100", height "20", fill "white"] []
        , rect [x (String.fromInt xBar), y ( String.fromInt yBar) , width ( String.fromInt heightBar ), height "20", fill svgStat.color] []
        , if index == 0 then Svg.text_ [ y ( String.fromInt yText), fontFamily "monospace", fill "white" ] [ Svg.text stat.name ]
          else Svg.text_ [] []
        ]
    ) svgStat.stats
    |> List.concat

-- HELPER

colorFromType : String -> String
colorFromType name =
    if name == "electric" then "yellow"
    else if name == "poison" then "green"
    else if name == "normal" then "grey"
    else if name == "fighting" then "red"
    else if name == "flying" then "blue"
    else if name == "ground" then "brown"
    else if name == "rock" then "darkgrey"
    else if name == "bug" then "black"
    else if name == "ghost" then "white"
    else if name == "steel" then "orange"
    else if name == "fire" then "red"
    else if name == "water" then "blue"
    else if name == "grass" then "green"
    else if name == "psychic" then "purple"
    else if name == "ice" then "lightblue"
    else if name == "dragon" then "darkgreen"
    else if name == "dark" then "black"
    else if name == "fairy" then "pink"
    else if name == "unknown" then "white"
    else if name == "shadow" then "lightblack"
    else "cadetblue"

-- HTTP

getPokemonDetail : PokemonName -> Cmd Msg
getPokemonDetail name =
    Http.send FuturePokemon ( Http.get ( toPokemonDetailUrl name ) Pokemon.pokemonDecoder )

toPokemonDetailUrl : PokemonName -> String
toPokemonDetailUrl name =
    Url.crossOrigin "http://localhost:9000" ["pokemon", PokemonName.toString name] []

getPokemonLike : PokemonName -> Cmd Msg
getPokemonLike name =
    Http.send FuturePokemonLike ( Http.get ( toPokemonLikeUrl name ) Pokemon.pokemonLikeDecoder )

putPokemonLike : PokemonName -> Cmd Msg
putPokemonLike name =
    Http.send FutureLike ( put ( toPokemonLikeUrl name ) Http.emptyBody )

putPokemonDislike : PokemonName -> Cmd Msg
putPokemonDislike name =
    Http.send FutureDislike ( put ( toPokemonDislikeUrl name ) Http.emptyBody )

toPokemonLikeUrl : PokemonName -> String
toPokemonLikeUrl name =
    Url.crossOrigin "http://localhost:9000" ["pokemon", PokemonName.toString name, "like"] []

toPokemonDislikeUrl : PokemonName -> String
toPokemonDislikeUrl name =
    Url.crossOrigin "http://localhost:9000" ["pokemon", PokemonName.toString name, "dislike"] []

getPokemonStats : PokemonName -> Cmd Msg
getPokemonStats name =
    Http.send FuturePokemonStats ( Http.get ( toPokemonStatsUrl name ) Pokemon.listPokemonStatsDecoder )

toPokemonStatsUrl : PokemonName -> String
toPokemonStatsUrl name =
    Url.crossOrigin "http://localhost:9000" ["pokemon", PokemonName.toString name, "stats"] []

put : String -> Http.Body -> Http.Request String
put url body =
  Http.request
    { method = "PUT"
    , headers = []
    , url = url
    , body = body
    , expect = Http.expectString
--    , expect = Http.expectStringResponse (\response -> if response.status.code == 202 then Ok () else Err () )
    , timeout = Nothing
    , withCredentials = False
    }