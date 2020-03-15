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
    , ticker : Maybe Ticker
    }



-- product


type alias ProductCatalog =
    Dict Id Product


type alias Id =
    String


type alias Ticker =
    { price : String
    }


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


fetchTicker : Id -> Cmd Msg
fetchTicker id =
    Http.get
        { url = "https://api.pro.coinbase.com/products/" ++ id ++ "/ticker"
        , expect = Http.expectJson GotTicker tickerDecoder
        }


tickerDecoder : D.Decoder Ticker
tickerDecoder =
    D.map Ticker (D.field "price" D.string)



-- message


type Msg
    = GotProducts (Result Http.Error (List Product))
    | GotTicker (Result Http.Error Ticker)
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
                    ( ProductsLoaded (State (catalogFrom productList) Nothing Nothing), Cmd.none )

                Err _ ->
                    ( Error "Something went wrong!", Cmd.none )

        ProductSelected id ->
            case model of
                ProductsLoaded state ->
                    ( ProductsLoaded { state | selected = Dict.get id state.catalog }, fetchTicker id )

                Error _ ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

        GotTicker result ->
            case result of
                Ok ticker ->
                    case model of
                        ProductsLoaded state ->
                            ( ProductsLoaded { state | ticker = Just ticker }, Cmd.none )

                        Error _ ->
                            ( model, Cmd.none )

                        Loading ->
                            ( model, Cmd.none )

                Err _ ->
                    ( Error "Something went wrong!", Cmd.none )



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
                    , case state.ticker of
                        Just ticker ->
                            h3 [] [ text ("Price: " ++ ticker.price) ]

                        Nothing ->
                            div [] []
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
                |> List.sortBy .id
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
