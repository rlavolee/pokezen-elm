module Pokemon.PokemonName exposing (PokemonName, toString, fromString, listPokemonNameDecoder)

import Json.Decode exposing (..)

-- TYPES


type PokemonName
    = PokemonName String


fromString : String -> PokemonName
fromString name =
    (PokemonName name)

toString : PokemonName -> String
toString (PokemonName name) =
    name

-- MODEL DECODER

pokemonNameDecoder : Decoder PokemonName
pokemonNameDecoder =
    map PokemonName
        string


listPokemonNameDecoder : Decoder ( List PokemonName )
listPokemonNameDecoder =
    list pokemonNameDecoder