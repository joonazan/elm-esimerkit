module Main exposing (main)

import Collage exposing (..)
import Collage.Layout as Layout
import Collage.Render
import Collage.Text as Text exposing (Text, color, fromString, size)
import Color exposing (..)
import Html exposing (Html, button, div, program, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onInput)
import Keyboard
import Random
import Time exposing (Time)


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


pelinKoko =
    20


type alias Piste =
    ( Int, Int )


type alias Mato =
    List Piste


type alias Omena =
    Piste


type alias Peli =
    { menosuunta : Suunta
    , mato : Mato
    , omena : Omena
    , gameOver : Bool
    }


alkutilanne =
    { menosuunta = Oikea
    , mato =
        [ ( 0, 0 )
        , ( 0, 1 )
        , ( 0, 2 )
        ]
    , omena = ( 0, 0 )
    , gameOver = False
    }


type Suunta
    = Ylos
    | Alas
    | Vasen
    | Oikea


init : ( Peli, Cmd Msg )
init =
    ( alkutilanne, Random.generate UusiOmena satunnainenPaikka )


{-| Generaattori, joka tuottaa satunnaisen sijainnin pelialueella.
-}
satunnainenPaikka : Random.Generator Piste
satunnainenPaikka =
    Random.pair (Random.int 0 (pelinKoko - 1)) (Random.int 0 (pelinKoko - 1))



-- UPDATE


type Msg
    = AikaaKului Time
    | MuutaSuuntaa Suunta
    | EiMuutosta
    | UusiOmena Piste
    | UusiPeli


update : Msg -> Peli -> ( Peli, Cmd Msg )
update msg peli =
    if msg == UusiPeli then
        init
    else if peli.gameOver then
        ( peli, Cmd.none )
    else
        case msg of
            AikaaKului time ->
                liikutaMatoa peli

            MuutaSuuntaa suunta ->
                ( kaanny suunta peli, Cmd.none )

            EiMuutosta ->
                ( peli, Cmd.none )

            UusiOmena sijainti ->
                ( { peli | omena = sijainti }, Cmd.none )

            UusiPeli ->
                init


{-| Siirtää pistettä yhden askeleen toivottuun suuntaan.
-}
seuraavaRuutu : Suunta -> Piste -> Piste
seuraavaRuutu suunta ( x, y ) =
    case suunta of
        Ylos ->
            ( x, y + 1 )

        Alas ->
            ( x, y - 1 )

        Oikea ->
            ( x + 1, y )

        Vasen ->
            ( x - 1, y )


liikutaMatoa : Peli -> ( Peli, Cmd Msg )
liikutaMatoa peli =
    let
        paa =
            List.head peli.mato
                |> Maybe.withDefault ( 0, 0 )

        uusiPaa =
            seuraavaRuutu peli.menosuunta paa

        uusiHanta =
            List.take (List.length peli.mato - 1) peli.mato
    in
    if not (onLaudalla uusiPaa) || List.member uusiPaa peli.mato then
        ( { peli | gameOver = True }, Cmd.none )
    else if uusiPaa == peli.omena then
        ( { peli | mato = uusiPaa :: peli.mato }
        , Random.generate UusiOmena satunnainenPaikka
        )
    else
        ( { peli | mato = uusiPaa :: uusiHanta }, Cmd.none )


onLaudalla : Piste -> Bool
onLaudalla ( x, y ) =
    x >= 0 && y >= 0 && x < pelinKoko && y < pelinKoko


kaanny : Suunta -> Peli -> Peli
kaanny uusiSuunta peli =
    if not (vastakkainen uusiSuunta peli.menosuunta) then
        { peli | menosuunta = uusiSuunta }
    else
        peli


vastakkainen : Suunta -> Suunta -> Bool
vastakkainen suunta1 suunta2 =
    (suunta1 == Alas && suunta2 == Ylos)
        || (suunta1 == Ylos && suunta2 == Alas)
        || (suunta1 == Oikea && suunta2 == Vasen)
        || (suunta1 == Vasen && suunta2 == Oikea)


subscriptions : Peli -> Sub Msg
subscriptions peli =
    Sub.batch
        [ Time.every (100 * Time.millisecond) AikaaKului
        , Keyboard.downs liikuNuolilla
        ]


{-| Ottaa näppäimen koodin ja palauttaa nuolinäppäimille
viestin MuutaSuuntaa oikean suunnan kera.
Muille näppäimille se palauttaa EiMuutosta.
-}
liikuNuolilla : Int -> Msg
liikuNuolilla nappainKoodi =
    case nappainKoodi of
        37 ->
            MuutaSuuntaa Vasen

        38 ->
            MuutaSuuntaa Ylos

        39 ->
            MuutaSuuntaa Oikea

        40 ->
            MuutaSuuntaa Alas

        _ ->
            EiMuutosta


{-| Pelin ruudun leveys kuvaruudulla pikseleinä.
-}
ruudunKoko : Float
ruudunKoko =
    10


{-| Antaa ruudun vasemman alakulman sijainnin ruudulla.
Ruudun (0, 0) kulma sijaitsee paikassa (0, 0),
ruudun (1, 0) paikassa (ruudunKoko, 0), jne.
-}
pikseleiksi : Piste -> ( Float, Float )
pikseleiksi ( x, y ) =
    ( toFloat (ruudunKoko * x), toFloat (ruudunKoko * y) )


{-| Pala matoa. Sen vasen alakulma sijaitsee paikassa (0, 0).
-}
matopalikka : Collage msg
matopalikka =
    filled (uniform yellow) (square 10)
        |> Layout.align Layout.bottomLeft


{-| Kuva omenasta ruudussa. Ruudun vasen alakulma on koordinaateissa (0, 0).
-}
omenapalikka =
    filled (uniform red) (circle 5)
        |> Layout.align Layout.bottomLeft


view : Peli -> Html.Html Msg
view peli =
    let
        pelimaailma =
            Layout.impose (group (omena :: mato)) tausta

        mato =
            List.map (\pos -> shift (pikseleiksi pos) matopalikka) peli.mato

        omena =
            shift (pikseleiksi peli.omena) omenapalikka

        tausta =
            filled (uniform green) (rectangle taustanKoko taustanKoko)
                |> Layout.align Layout.bottomLeft

        taustanKoko =
            ruudunKoko * pelinKoko

        kokoKuva =
            if peli.gameOver then
                Layout.at Layout.base lopputeksti pelimaailma
            else
                pelimaailma

        lopputeksti =
            Text.fromString "GAME OVER!"
                |> Text.size 30
                |> Text.color blue
                |> Collage.rendered
    in
    div []
        [ Collage.Render.svg kokoKuva
        , button [ onClick UusiPeli ] [ text "Otetaan uusiksi!" ]
        ]
