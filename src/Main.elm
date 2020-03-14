module Main exposing (..)

import Browser exposing (Document)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Events exposing (onInput)
import Http
import Json.Decode as D


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- model


type Model
    = ProductsLoaded State
    | Error String
    | Loading


type alias State =
    { catalog : ProductCatalog
    , selected : Maybe Product
    }



-- product


type alias ProductCatalog =
    Dict Id Product


type alias Id =
    String


type alias Product =
    { id : Id
    , baseCurrency : String
    , quoteCurrency : String
    }


catalogFrom : List Product -> ProductCatalog
catalogFrom products =
    Dict.fromList (List.map (\p -> ( p.id, p )) products)


fetchProducts : Cmd Msg
fetchProducts =
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



-- message


type Msg
    = GotProducts (Result Http.Error (List Product))
    | ProductSelected Id



-- init


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, fetchProducts )



-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotProducts result ->
            case result of
                Ok productList ->
                    ( ProductsLoaded (State (catalogFrom productList) Nothing), Cmd.none )

                Err _ ->
                    ( Error "Something went wrong!", Cmd.none )

        ProductSelected id ->
            case model of
                ProductsLoaded state ->
                    ( ProductsLoaded { state | selected = Dict.get id state.catalog }, Cmd.none )

                Error _ ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )



-- view


view : Model -> Document Msg
view model =
    { title = "Crypto Prices"
    , body =
        [ case model of
            Loading ->
                h2 [] [ text "Loading" ]

            Error error ->
                h2 [] [ text error ]

            ProductsLoaded state ->
                div []
                    [ productSelect (Dict.values state.catalog)
                    , case state.selected of
                        Just product ->
                            productDetails product

                        Nothing ->
                            p [] [ text "select a product" ]
                    ]
        ]
    }


productDetails : Product -> Html Msg
productDetails product =
    div []
        [ h1 [] [ text product.id ]
        , h3 [] [ text ("Base Currency: " ++ product.baseCurrency) ]
        , h3 [] [ text ("Quote Currency: " ++ product.quoteCurrency) ]
        ]


productSelect : List Product -> Html Msg
productSelect products =
    let
        options =
            products
                |> List.sortBy .baseCurrency
                |> List.map (\p -> option [] [ text p.id ])
    in
    div []
        [ h1 [] [ text "Products" ]
        , select [ onInput ProductSelected ] options
        ]



--subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
