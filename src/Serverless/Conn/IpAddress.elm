module Serverless.Conn.IpAddress exposing
    ( IpAddress
    , ip4, loopback
    , decoder
    )

{-| Internet protocol addresses and related functions.

@docs IpAddress


## Constructors

@docs ip4, loopback


## Misc

These functions are typically not needed when building an application. They are
used internally by the framework.

@docs decoder

-}

import Json.Decode as Decode exposing (Decoder, andThen)


{-| IP address type.
-}
type alias IpAddress =
    { first : Int
    , second : Int
    , third : Int
    , fourth : Int
    }



-- CONSTRUCTORS


{-| Creates an IPv4 address.
-}
ip4 : Int -> Int -> Int -> Int -> IpAddress
ip4 a b c d =
    IpAddress a b c d


{-| The loopback address.

    loopback
    --> ip4 127 0 0 1

-}
loopback : IpAddress
loopback =
    IpAddress 127 0 0 1



-- JSON


{-| JSON decoder an IP address.
-}
decoder : Decoder IpAddress
decoder =
    Decode.string
        |> andThen
            (\w ->
                w
                    |> String.split "."
                    |> List.map toNonNegativeInt
                    |> maybeListFn
                    |> require4
                    |> Maybe.andThen take4Tuple
                    |> Maybe.map Decode.succeed
                    |> Maybe.withDefault (Decode.fail <| "Unsupported IP address: " ++ w)
            )


toNonNegativeInt : String -> Maybe Int
toNonNegativeInt val =
    case val |> String.toInt of
        Just i ->
            if i >= 0 then
                Just i

            else
                Nothing

        Nothing ->
            Nothing


require4 : Maybe (List a) -> Maybe (List a)
require4 maybeList =
    case maybeList of
        Just list ->
            if List.length list == 4 then
                Just list

            else
                Nothing

        Nothing ->
            Nothing



-- HELPERS
-- From https://package.elm-lang.org/packages/danielnarey/elm-toolkit/4.5.0/Toolkit-Helpers


take4Tuple : List Int -> Maybe IpAddress
take4Tuple list =
    case list of
        a :: b :: c :: d :: _ ->
            Just (IpAddress a b c d)

        _ ->
            Nothing


maybeListFn : List (Maybe a) -> Maybe (List a)
maybeListFn list =
    case list |> List.take 1 of
        [ Just value ] ->
            list
                |> List.drop 1
                |> maybeListFn
                |> Maybe.map ((::) value)

        [] ->
            Just []

        _ ->
            Nothing
