module RippleCarryAdder exposing (andGate)

import Bitwise
import Html


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
            notGate (andGate a b)

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


type alias Binary =
    { d0 : Int
    , d1 : Int
    , d2 : Int
    , d3 : Int
    }


rippleCarryAdder a b cin =
    let
        firstResult =
            fullAdder a.d3 b.d3 cin

        secondResult =
            fullAdder a.d2 b.d2 firstResult.carry

        thirdResult =
            fullAdder a.d1 b.d1 secondResult.carry

        fourthResult =
            fullAdder a.d0 b.d0 thirdResult.carry
    in
    { carry = fourthResult.carry
    , sum1 = firstResult.digits
    , sum2 = secondResult.digits
    , sum3 = thirdResult.digits
    , sum4 = fourthResult.digits
    }


main =
    let
        a =
            Binary 1 1 0 1

        b =
            Binary 1 0 1 1

        results =
            rippleCarryAdder a b 0
    in
    Html.text ("Carry: " ++ String.fromInt results.carry ++ " Sum: " ++ String.fromInt results.sum4 ++ String.fromInt results.sum3 ++ String.fromInt results.sum2 ++ String.fromInt results.sum1)
