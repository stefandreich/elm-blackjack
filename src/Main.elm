-----------------------
-- Stefan-Andrei Chichisan
-- 15.11.2020
-----------------------

module Main exposing (main, calculateScore)

import Browser
import Html exposing (..)
import Html.Attributes exposing (disabled, height, src, style, width)
import Html.Events exposing (..)
import Random
import Debug
import List
import Browser.Navigation as Navigation


import Card exposing (..)


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


type alias Model =
  { hand: List Card,
    deck: List Card,
    showDeck: Bool
  }

startingModel : Model
startingModel =
    Model [] Card.deck True

init : () -> (Model, Cmd Msg)
init _ =
  ( startingModel
  , Cmd.none
  )


type Msg
  = Draw
  | NewCard Card
  | ToggleDeck
  | ReloadButton


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Draw ->
      ( model
      , drawCard model
      )
    
    -- Add the new card to the player's hand (`hand`) and remove it from the `deck`
    NewCard newCard ->
      ( --Model (newCard::model.hand) (List.filter(\x -> x /= newCard) model.deck) (model.showDeck)
        { model | hand = (newCard::model.hand), deck = (List.filter(\x -> x /= newCard) model.deck), showDeck = model.showDeck }
      , Cmd.none
      )

    -- Toggle (if it's `True` set it `False`, if it's `False` set it to `True`) the `showDeck` member of the `Model`
    ToggleDeck ->
      (
        --Model (model.hand) (model.deck) (if model.showDeck == True then False else True)
        { model | hand = model.hand, deck = model.deck, showDeck = (if model.showDeck == True then False else True) }
      , Cmd.none
      )

    ReloadButton -> (model, Navigation.reload)

drawCard : Model -> Cmd Msg
drawCard model =
  case model.deck of
    (first::rest) -> Random.generate NewCard (Random.uniform first rest)
    _ -> Cmd.none

{-
  1. Get the value of each card (use `cardValue`)
  2. Generate a list of all possible scores
  3. Return the score closest to 21 (less than 21!), if one exists, else the smallest score over 21
  ```elm
  calculateScore [Card King Hearts] == 10
  calculateScore [Card Two Hearts] == 2
  calculateScore [Card Two Hearts, Card King Spades] == 12
  calculateScore [Card Ace Hearts, Card King Spades] == 21
  calculateScore [Card Ace Hearts, Card Five Hearts, Card Seven Spades] == 13
  calculateScore [Card King Hearts, Card Five Hearts, Card Seven Spades] == 22
  calculateScore [Card King Hearts, Card Ten Clubs, Card Ace Spades] == 21
  calculateScore [Card Ace Spades, Card Ace Clubs, Card Ten Clubs, Card King Clubs] == 22
  ```
-}
calculateScore : List Card -> Int
calculateScore cards =
    let
        getTheScore l acc =
            case l of
                [] -> acc
                x::xs -> getTheScore xs (((List.head (cardValue x)) |> Maybe.withDefault 0) + acc)

        checkForAce =
            List.filter (\(Card face suit) -> face == Ace) cards

        countAces =
            List.length (checkForAce)

        listOfScores currScore nbOfAces acc =
            case nbOfAces of
                0 -> acc ++ [getTheScore cards 0]
                _ -> listOfScores currScore (nbOfAces - 1) acc ++ [getTheScore cards 0 - 10 * (countAces)]

        f = listOfScores (getTheScore cards 0) countAces [getTheScore cards 0]

    in
        if List.isEmpty ((List.filter (\x -> x <= 21) f)) then
            (List.minimum (List.filter (\x -> x > 21) f)) |> Maybe.withDefault 0
        else
            (List.maximum (List.filter (\x -> x <= 21) f)) |> Maybe.withDefault 0

        --getTheScore cards 0

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

{-
  Use the `viewCard` function for showing the player's hand and the `cardToUnicode` function for the remaining deck.
-}
view : Model -> Html Msg
view model =
  let
    appName = "Blackjack"

    drawTheDeck =
        model.hand
        |> List.map viewCard

    toggleTheDeck =
        model.deck
        |> List.map cardToUnicode
        |> List.map text

  in
    div []
      [ div [] [ h1 [] [text appName] ]
      , button [onClick ReloadButton] [text "Start a new game"]
      , button [onClick Draw, disabled(calculateScore model.hand >= 21)] [text "Draw"]
      , button [onClick ToggleDeck] [text "Hide or show the deck"]
      , div [] drawTheDeck
      , div [] (if model.showDeck == False then toggleTheDeck else [text ""])
      , hr [] [text ("The score is: " ++ String.fromInt (calculateScore model.hand))]
      , div [] (if calculateScore model.hand == 21 then [text "You won!"] else if calculateScore model.hand > 21 then [text "You lost!"] else [text ""])
      , br [] []
      , br [] []
      ]