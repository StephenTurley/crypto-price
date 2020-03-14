module Main exposing (..)

import Browser exposing (Document)
import Html exposing (..)
import Http
import Json.Decode as D


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Model
    = Products (List Product)
    | Error String
    | Loading


type alias Product =
    { id : String
    , baseCurrency : String
    , quoteCurrency : String
    }


type Msg
    = GotProducts (Result Http.Error (List Product))


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getProducts )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotProducts result ->
            case result of
                Ok products ->
                    ( Products products, Cmd.none )

                Err _ ->
                    ( Error "Something went wrong!", Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Crypto Prices"
    , body =
        [ case model of
            Loading ->
                h2 [] [ text "Loading" ]

            Error error ->
                h2 [] [ text error ]

            Products products ->
                viewProducts products
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


viewProducts : List Product -> Html Msg
viewProducts products =
    let
        sortedProducts =
            List.sortBy .baseCurrency products
    in
    div []
        [ h1 [] [ text "Products" ]
        , ul [] (List.map (\p -> li [] [ text p.id ]) sortedProducts)
        ]


getProducts : Cmd Msg
getProducts =
    Http.get
        { url = "https://api.pro.coinbase.com/products"
        , expect = Http.expectJson GotProducts productsDecoder
        }


productsDecoder : D.Decoder (List Product)
productsDecoder =
    D.list productDecoder


productDecoder : D.Decoder Product
productDecoder =
    D.map3 Product
        (D.field "id" D.string)
        (D.field "base_currency" D.string)
        (D.field "quote_currency" D.string)
