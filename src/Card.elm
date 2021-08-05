-----------------------
-- Stefan-Andrei Chichisan
-- 15.11.2020
-----------------------

module Card exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)

{-
  Replace with your definitions from assignment 1
-}
type Face
    = Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King

type Suit
    = Spades
    | Diamonds
    | Clubs
    | Hearts

type Card = Card Face Suit

faceToString : Face -> String
faceToString face =
    case face of
        Ace -> "Ace"
        Two -> "Two"
        Three -> "Three"
        Four -> "Four"
        Five -> "Five"
        Six -> "Six"
        Seven -> "Seven"
        Eight -> "Eight"
        Nine -> "Nine"
        Ten -> "Ten"
        Jack -> "Jack"
        King -> "King"
        Queen -> "Queen"

suitToString : Suit -> String
suitToString suit =
    case suit of
        Spades -> "Spades"
        Diamonds -> "Diamonds"
        Clubs -> "Clubs"
        Hearts -> "Hearts"

cardToString : Card -> String
cardToString (Card face suit) =
    faceToString face ++ " of " ++ suitToString suit


{-tailList: List a -> List a
tailList l =
    case l of
    [] -> []
    _::xs -> xs

indexList: Int -> List Face -> Face
indexList i l =
    let
        traverseList: List Face -> Int -> Face
        traverseList lx count =
            if count == i then
                case lx of
                    x::_ -> x
                    [] -> Ace
            else
                traverseList (tailList lx) (count + 1)
    in
        traverseList l 1-}

cardValue : Card -> List Int
cardValue card =
    let
        (Card face suit) = card
    in
        case face of
            Ace -> [11, 1]
            Two -> [2]
            Three -> [3]
            Four -> [4]
            Five -> [5]
            Six -> [6]
            Seven -> [7]
            Eight -> [8]
            Nine -> [9]
            Ten -> [10]
            Jack -> [10]
            Queen -> [10]
            King -> [10]


append : List Suit -> List Face -> List Card
append listSuit listFace =
    List.concatMap (\faces -> List.map(\suits -> Card faces suits) listSuit) listFace

deck: List Card
deck =
    let
        faces =
            [Ace
            ,Two
            , Three
            , Four
            , Five
            , Six
            , Seven
            , Eight
            , Nine
            , Ten
            , Jack
            , Queen
            , King]

        suits =
            [Spades,
             Diamonds,
             Clubs,
             Hearts]
    in
        append suits faces

{-
  Modify this function (if needed) to work with your `Card` definition
-}
cardToUnicode : Card -> String
cardToUnicode (Card face suit) =
   case face of
     Ace -> case suit of
       Spades ->"🂡"
       Hearts -> "🂱"
       Clubs ->  "🃑"
       Diamonds -> "🃁"
     Two -> case suit of
       Spades ->"🂢"
       Hearts -> "🂲"
       Clubs ->  "🃒"
       Diamonds -> "🃂"
     Three -> case suit of
       Spades ->"🂣"
       Hearts -> "🂳"
       Clubs ->  "🃓"
       Diamonds ->"🃃"
     Four -> case suit of
       Spades ->"🂤"
       Hearts -> "🂴"
       Clubs ->  "🃔"
       Diamonds -> "🃄"
     Five -> case suit of
       Spades ->"🂥"
       Hearts -> "🂵"
       Clubs ->  "🃕"
       Diamonds -> "🃅"
     Six -> case suit of
       Spades ->"🂦"
       Hearts -> "🂶"
       Clubs ->  "🃖"
       Diamonds -> "🃆"
     Seven -> case suit of
       Spades ->"🂧"
       Hearts -> "🂷"
       Clubs ->  "🃗"
       Diamonds -> "🃇"
     Eight -> case suit of
       Spades -> "🂨"
       Hearts ->  "🂸"
       Clubs ->   "🃘"
       Diamonds ->  "🃈"
     Nine -> case suit of
       Spades -> "🂩"
       Hearts ->  "🂹"
       Clubs ->   "🃙"
       Diamonds ->  "🃉"
     Ten -> case suit of
       Spades ->"🂪"
       Hearts -> "🂺"
       Clubs ->  "🃚"
       Diamonds -> "🃊"
     Jack -> case suit of
       Spades ->"🂫"
       Hearts -> "🂻"
       Clubs ->  "🃛"
       Diamonds -> "🃋"
     Queen -> case suit of
       Spades ->"🂭"
       Hearts -> "🂽"
       Clubs ->  "🃝"
       Diamonds -> "🃍"
     King -> case suit of
       Spades -> "🂮"
       Hearts -> "🂾"
       Clubs ->  "🃞"
       Diamonds -> "🃎"


{-
  Modify this function (if needed) to work with your `Card` definition
-}
viewCard : Card -> Html msg
viewCard (Card faceCard suitCard) =
   let
     suit = suitCard
     face = faceCard
     faceName = faceToString face
     suitName = suitToString suit
     suitColor s =
       case s of
         Diamonds -> "red"
         Spades -> "black"
         Hearts -> "red"
         Clubs -> "black"
     unicode = cardToUnicode (Card faceCard suitCard)
   in
     div [style "display" "inline-block"] [
       div [style "font-size" "12em", style "color" (suitColor suit)] [text unicode],
       div [style "font-size" "0.8em"]  [text (cardToString (Card faceCard suitCard))]
     ]