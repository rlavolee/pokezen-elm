module Pokemon.Pokemon exposing (Pokemon, PokemonLike, PokemonStats, Stat, pokemonDecoder, pokemonLikeDecoder, listPokemonStatsDecoder)

import Dict
import Json.Decode exposing (..)

-- MODEL

type alias Pokemon
    =
    { name : String
    , weight : Int
    , height : Int
    , sprites : Sprites
    , pTypes : List String
    , stats: List Stat
    }

type alias Sprites
    =
    { backFemale : Maybe String
    , backShinyFemale : Maybe String
    , backDefault : Maybe String
    , frontFemale : Maybe String
    , frontShinyFemale : Maybe String
    , backShiny : Maybe String
    , frontDefault : Maybe String
    , frontShiny : Maybe String
    }

type alias PokemonLike
    =
    { name : String
    , like : Int
    }

type alias PokemonStats
    =
    { name : String
    , pType : String
    , average : List Stat
    }

type alias Stat
    =
    { name : String
    , baseStat : Int
    }

-- DECODER

spritesDecoder : Decoder Sprites
spritesDecoder =
    field "sprites" pokemonSpritesDecoder

typeListDecoder : Decoder ( List String )
typeListDecoder =
    field "types" ( list string )

statsFieldDecoder : String -> Decoder ( List Stat )
statsFieldDecoder name =
    field name ( list statDecoder )

intFieldDecoder : String -> Decoder Int
intFieldDecoder name =
    field name int

stringFieldDecoder : String -> Decoder String
stringFieldDecoder name =
    field name string

-- DECODER SPRITES

maybeFieldDecoder : String -> Decoder ( Maybe String )
maybeFieldDecoder name =
    maybe ( field name string )

-- MODEL DECODER

pokemonDecoder : Decoder Pokemon
pokemonDecoder =
    map6 Pokemon
      ( stringFieldDecoder "name" )
      ( intFieldDecoder "weight" )
      ( intFieldDecoder "height" )
      spritesDecoder
      typeListDecoder
      ( statsFieldDecoder "stats" )

pokemonLikeDecoder : Decoder PokemonLike
pokemonLikeDecoder =
    map2 PokemonLike
        ( stringFieldDecoder "name" )
        ( intFieldDecoder "like" )

statDecoder : Decoder Stat
statDecoder =
    map2 Stat
        ( stringFieldDecoder "name" )
        ( intFieldDecoder "baseStat" )

pokemonStatsDecoder : Decoder PokemonStats
pokemonStatsDecoder =
    map3 PokemonStats
        ( stringFieldDecoder "name" )
        ( stringFieldDecoder "type" )
        ( statsFieldDecoder "average" )

listPokemonStatsDecoder : Decoder ( List PokemonStats )
listPokemonStatsDecoder =
    list pokemonStatsDecoder

pokemonSpritesDecoder : Decoder Sprites
pokemonSpritesDecoder =
    map8 Sprites
        ( maybeFieldDecoder "backFemale" )
        ( maybeFieldDecoder "backShinyFemale" )
        ( maybeFieldDecoder "backDefault" )
        ( maybeFieldDecoder "frontFemale" )
        ( maybeFieldDecoder "frontShinyFemale" )
        ( maybeFieldDecoder "backShiny" )
        ( maybeFieldDecoder "frontDefault" )
        ( maybeFieldDecoder "frontShiny" )