module scorecard2

type FrameR =
    | Unplayed
    | InProgress of int
    | Complete of int * int
    | Spare of int * int
    | Strike

type FrameF =
    | UnplayedF
    | InProgressF of int
    | InProgressBonusF1 of int
    | CompleteF of int * int
    | InProgressBonusF2 of int * int
    | CompleteBonusF of int * int * int

type Frame = FrameR of FrameR | FrameF of FrameF
type game = Frame list


let createNewGame = [Unplayed]


let isStrike pins = pins=10
let isSpare p1 p2 = (p1+p2)=10

let evaluateBowl pins game =
    match game with
    // Cases for the non-final frames
    | FrameR Unplayed::rest -> if isStrike pins then FrameR Strike::rest
                                                else FrameR (InProgress pins)::rest
    | FrameR (InProgress p1)::rest -> if isSpare p1 pins then FrameR (Spare (p1,pins))::rest
                                                         else FrameR (Complete (p1,pins))::rest
    // Cases for the final frame
    | FrameF UnplayedF::rest -> if isStrike pins then FrameF (InProgressBonusF1 pins)::rest
                                                 else FrameF (InProgressF pins)::rest
    | FrameF (InProgressF p1)::rest -> if isSpare p1 pins then FrameF (InProgressBonusF2 (p1,pins))::rest
                                                          else FrameF (CompleteF (p1,pins))::rest
    | FrameF (InProgressBonusF1 p1)::rest -> FrameF (InProgressBonusF2 (p1,pins))::rest
    | FrameF (InProgressBonusF2 (p1,p2))::rest -> FrameF (CompleteBonusF (p1,p2,pins))::rest
    | _ -> game // Really should error here?


let submitBowl pins game =
    let topFrameFinished game =
        match game with
        | FrameR (Complete (_,_))::_ -> true
        | FrameR (Spare (_,_))::_ -> true
        | FrameR Strike::_ -> true
        | FrameF (CompleteF (_,_))::_ -> true
        | FrameF (CompleteBonusF (_,_,_))::_ -> true
        | _ -> false
    let gameFinished game =
        match game with
        | FrameF (CompleteF (_,_))::_ -> true
        | FrameF (CompleteBonusF (_,_,_))::_ -> true
        | _ -> false
    
    let newGame = evaluateBowl pins game

    if topFrameFinished newGame then
        if gameFinished newGame then newGame
        else if (List.length game) < 9 then FrameR Unplayed::newGame else FrameF UnplayedF::newGame
    else newGame
