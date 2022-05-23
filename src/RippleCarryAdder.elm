module RippleCarryAdder exposing (andGate)

import Html
import Bitwise


andGate a b =
    Bitwise.and a b

orGate a b =
    Bitwise.or a b

notGate a =
    case a of 
        0 ->
            1
        1 -> 
            0
        _ ->
            -1

halfAdder a b =
    let
        d = 
            orGate a b
        
        e = 
            notGate ( andGate a b )

        digits =
            andGate d e

        carry = 
            andGate a b
    in
    { carry = carry
    , digits = digits
    }

fullAdder a b cin =
    let
        firstResult =
            halfAdder b cin
        
        secondResult =
            halfAdder a firstResult.digits
        
        fullCarry = 
            orGate firstResult.carry secondResult.carry
    in
    { carry = fullCarry
    , digits = secondResult.digits
    }

main = 
    let
        results =
            fullAdder 1 1 0
    in
    Html.text ( "Carry: " ++ String.fromInt( results.carry ) ++ " Digits: " ++ String.fromInt( results.digits ) )