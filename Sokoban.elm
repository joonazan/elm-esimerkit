module Main exposing (main)

import Collage exposing (..)
import Collage.Layout as Layout
import Collage.Render
import Collage.Text as Text
import Color exposing (..)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Keyboard


type alias Piste =
    ( Int, Int )


type alias Pistejoukko =
    List Piste


lisaa : Piste -> Pistejoukko -> Pistejoukko
lisaa =
    (::)


poista : Piste -> Pistejoukko -> Pistejoukko
poista p =
    List.filter ((/=) p)


sisaltaa : Piste -> Pistejoukko -> Bool
sisaltaa =
    List.member


type alias Pelilauta =
    { pelaaja : Piste
    , kivet : Pistejoukko
    , kuopat : Pistejoukko
    , seinat : Pistejoukko
    }


alkutila : Pelilauta
alkutila =
    { pelaaja = ( 6, 5 )
    , kivet = [ ( 4, 7 ), ( 6, 3 ), ( 6, 4 ) ]
    , kuopat = [ ( 2, 7 ), ( 7, 6 ), ( 7, 2 ) ]
    , seinat =
        [ ( 1, 8 )
        , ( 2, 8 )
        , ( 3, 8 )
        , ( 4, 8 )
        , ( 5, 8 )
        , ( 6, 8 )
        , ( 7, 8 )
        , ( 8, 8 )
        , ( 1, 7 )
        , ( 8, 7 )
        , ( 1, 6 )
        , ( 2, 6 )
        , ( 3, 6 )
        , ( 4, 6 )
        , ( 8, 6 )
        , ( 4, 5 )
        , ( 8, 5 )
        , ( 4, 4 )
        , ( 8, 4 )
        , ( 4, 3 )
        , ( 8, 3 )
        , ( 4, 2 )
        , ( 8, 2 )
        , ( 4, 1 )
        , ( 5, 1 )
        , ( 6, 1 )
        , ( 7, 1 )
        , ( 8, 1 )
        ]
    }


ratkaistu : Pelilauta -> Bool
ratkaistu lauta =
    lauta.kuopat == []


main =
    Html.program
        { init = ( alkutila, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = Liiku Suunta
    | EiMuutosta
    | UusiPeli


update : Msg -> Pelilauta -> ( Pelilauta, Cmd Msg )
update msg pelilauta =
    if ratkaistu pelilauta then
        ( pelilauta, Cmd.none )
    else
        case msg of
            Liiku suunta ->
                ( siirto suunta pelilauta, Cmd.none )

            EiMuutosta ->
                ( pelilauta, Cmd.none )

            UusiPeli ->
                ( alkutila, Cmd.none )


type Suunta
    = Ylos
    | Alas
    | Oikea
    | Vasen


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


siirto : Suunta -> Pelilauta -> Pelilauta
siirto suunta ({ kuopat, seinat, kivet } as pelilauta) =
    let
        menossa =
            sisaltaa pelaajanSeuraavaRuutu

        pelaajanSeuraavaRuutu =
            seuraavaRuutu suunta pelilauta.pelaaja

        kivenSeuraavaRuutu =
            seuraavaRuutu suunta pelaajanSeuraavaRuutu
    in
    if menossa kuopat || menossa seinat then
        pelilauta
    else if menossa kivet then
        if sisaltaa kivenSeuraavaRuutu seinat || sisaltaa kivenSeuraavaRuutu kivet then
            pelilauta
        else
            { pelilauta | pelaaja = pelaajanSeuraavaRuutu }
                |> siirraKivea pelaajanSeuraavaRuutu kivenSeuraavaRuutu
    else
        { pelilauta | pelaaja = pelaajanSeuraavaRuutu }


siirraKivea : Piste -> Piste -> Pelilauta -> Pelilauta
siirraKivea vanha uusi lauta =
    let
        ilmanKivea =
            { lauta | kivet = poista vanha lauta.kivet }
    in
    if sisaltaa uusi lauta.kuopat then
        { ilmanKivea | kuopat = poista uusi lauta.kuopat }
    else
        { ilmanKivea | kivet = lisaa uusi ilmanKivea.kivet }


subscriptions : Pelilauta -> Sub Msg
subscriptions pelilauta =
    Keyboard.downs liikuNuolilla


{-| Ottaa näppäimen koodin ja palauttaa nuolinäppäimille
viestin Liiku oikean suunnan kera.
Muille näppäimille se palauttaa EiMuutosta.
-}
liikuNuolilla : Int -> Msg
liikuNuolilla nappainKoodi =
    case nappainKoodi of
        37 ->
            Liiku Vasen

        38 ->
            Liiku Ylos

        39 ->
            Liiku Oikea

        40 ->
            Liiku Alas

        _ ->
            EiMuutosta


view : Pelilauta -> Html Msg
view pelilauta =
    let
        kuvaPaikkaan kuva piste =
            shift (pikseleiksi piste) kuva

        pelimaailma =
            group
                (kuvaPaikkaan pelaajaHahmo pelilauta.pelaaja
                    :: List.map (kuvaPaikkaan kiviKuva) pelilauta.kivet
                    ++ List.map (kuvaPaikkaan kuoppa) pelilauta.kuopat
                    ++ List.map (kuvaPaikkaan seina) pelilauta.seinat
                )

        lopputeksti =
            Text.fromString "HIENO HOMMA!"
                |> Text.size 16
                |> Text.color orange
                |> Collage.rendered
                |> shift (pikseleiksi ( 4, 4 ))

        kokoKuva =
            if ratkaistu pelilauta then
                Layout.impose lopputeksti pelimaailma
            else
                pelimaailma
    in
    div []
        [ Collage.Render.svg kokoKuva
        , button [ onClick UusiPeli ] [ text "Aloita alusta" ]
        ]


ruudunLeveys : Float
ruudunLeveys =
    20


pikseleiksi : Piste -> ( Float, Float )
pikseleiksi ( x, y ) =
    ( ruudunLeveys * toFloat x, ruudunLeveys * toFloat y )


seina : Collage msg
seina =
    filled (uniform blue) (square ruudunLeveys)


kuoppa : Collage msg
kuoppa =
    filled (uniform black) (circle (ruudunLeveys / 3))


pelaajaHahmo : Collage msg
pelaajaHahmo =
    filled (uniform yellow) (rectangle (ruudunLeveys / 2) (ruudunLeveys - 2))


kiviKuva : Collage msg
kiviKuva =
    filled (uniform grey) (circle (ruudunLeveys / 2))
