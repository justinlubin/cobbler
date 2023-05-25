module Syntax exposing (..)


type Color a
    = Red
    | Green
    | Blue
    | Custom a a a


int : Int
int =
    3


float : Float
float =
    3.5


unit : ()
unit =
    ()


string1 : String
string1 =
    "hello"


string2 : String
string2 =
    string1 ++ "!"


f : Int -> Int
f x =
    x + 1


g : a -> List a -> List a
g def =
    List.map (\_ -> def)


g2 : a -> List a -> List a
g2 def =
    List.map (\myArgument -> def)


g3 : a -> List a -> List a
g3 =
    \def ->
        List.map (\myArguuument -> def)


yellow : Color Int
yellow =
    Custom 255 255 0


red : Color a
red =
    Red


match : Int
match =
    case Custom 255 255 0 of
        Red ->
            0

        Green ->
            1

        Blue ->
            2

        Custom a b c ->
            a + b + c
