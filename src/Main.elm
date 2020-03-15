module Main exposing (init)

import Browser exposing (Document)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)
import Http
import Json.Decode as D
import Json.Decode.Pipeline exposing (hardcoded, required)
import Time


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



-- TODO make selected a Maybe ID again and update products directly in the catalog


type alias State =
    { catalog : ProductCatalog
    , selected : Maybe Product
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
    , ticker : Maybe Ticker
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
    D.succeed Product
        |> required "id" D.string
        |> required "base_currency" D.string
        |> required "quote_currency" D.string
        |> hardcoded Nothing


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
    | Tick Time.Posix



-- init


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, fetchProducts )



-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotProducts result ->
            handleHttp result updateProducts

        ProductSelected id ->
            updateState model (updateSelected id)

        GotTicker result ->
            handleHttp result (\ticker -> updateState model (updateTicker ticker))

        Tick time ->
            updateState model
                (\s ->
                    case s.selected of
                        Just product ->
                            ( model, fetchTicker product.id )

                        Nothing ->
                            ( model, Cmd.none )
                )


handleHttp : Result Http.Error data -> (data -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
handleHttp result happyPath =
    case result of
        Ok data ->
            happyPath data

        Err error ->
            case error of
                Http.BadStatus status ->
                    ( Error ("Server returned error " ++ String.fromInt status), Cmd.none )

                Http.BadUrl msg ->
                    ( Error msg, Cmd.none )

                Http.BadBody msg ->
                    ( Error msg, Cmd.none )

                Http.Timeout ->
                    ( Error "Request timed out", Cmd.none )

                Http.NetworkError ->
                    ( Error "Network Error", Cmd.none )


updateProducts : List Product -> ( Model, Cmd Msg )
updateProducts productList =
    ( ProductsLoaded (State (catalogFrom productList) Nothing), Cmd.none )


updateSelected : Id -> State -> ( Model, Cmd Msg )
updateSelected id state =
    ( ProductsLoaded { state | selected = Dict.get id state.catalog }, fetchTicker id )


updateTicker : Ticker -> State -> ( Model, Cmd Msg )
updateTicker ticker state =
    let
        selected =
            state.selected
    in
    case selected of
        Just product ->
            ( ProductsLoaded { state | selected = Just { product | ticker = Just ticker } }, Cmd.none )

        Nothing ->
            ( ProductsLoaded state, Cmd.none )


updateState : Model -> (State -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
updateState model updater =
    case model of
        ProductsLoaded state ->
            updater state

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
                    [ productSelect state
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
    let
        price =
            case product.ticker of
                Just ticker ->
                    ticker.price

                Nothing ->
                    ""
    in
    div []
        [ h1 [] [ text product.id ]
        , h3 [] [ text ("Base Currency: " ++ product.baseCurrency) ]
        , h3 [] [ text ("Quote Currency: " ++ product.quoteCurrency) ]
        , h3 [] [ text ("Price: " ++ price) ]
        ]


productSelect : State -> Html Msg
productSelect state =
    let
        products =
            state.catalog
                |> Dict.values
                |> List.sortBy .id

        options =
            List.map (\p -> option [] [ text p.id ]) products

        selected =
            case state.selected of
                Just product ->
                    product.id

                Nothing ->
                    ""
    in
    div []
        [ h1 [] [ text "Products" ]
        , select [ value selected, onInput ProductSelected ] options
        ]



--subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 2000 Tick
