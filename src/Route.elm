module Route exposing (Route(..), fromUrl, toString)

import Url exposing (Url)
import Url.Parser exposing (Parser, (</>), int, map, oneOf, s, string, parse, top)


-- ROUTING

type Route
    = Home
    | PokemonDetail String

parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Home top
        , map PokemonDetail (s "pokemon" </> string)
        ]

-- PUBLIC HELPERS

fromUrl : Url -> Maybe Route
fromUrl url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> parse parser

toString : Route -> String
toString route =
    case route of
        Home ->
            "#/"
        PokemonDetail string ->
            "#/pokemon/" ++ string