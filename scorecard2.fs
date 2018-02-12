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

type FrameScore =
    | Unscored
    | BuildingScore of int
    | Scored of int

type Frame = FrameR of FrameR * FrameScore | FrameF of FrameF * FrameScore
type game = Frame list


let createNewGame = [FrameR (Unplayed,Unscored)]


let isStrike pins = pins=10
let isSpare p1 p2 = (p1+p2)=10

let evaluateBowl pins game =
    match game with
    // Cases for the non-final frames
    | FrameR (Unplayed,_)::rest -> if isStrike pins then FrameR (Strike, BuildingScore 10)::rest
                                                    else FrameR (InProgress pins, BuildingScore pins)::rest
    | FrameR (InProgress p1, _)::rest -> if isSpare p1 pins then FrameR (Spare (p1,pins), BuildingScore 10)::rest
                                                            else FrameR (Complete (p1,pins), Scored <| p1+pins)::rest
    // Cases for the final frame
    | FrameF (UnplayedF, _)::rest -> if isStrike pins then FrameF (InProgressBonusF1 pins, BuildingScore pins)::rest
                                                      else FrameF (InProgressF pins, BuildingScore pins)::rest
    | FrameF (InProgressF p1, _)::rest -> if isSpare p1 pins then FrameF (InProgressBonusF2 (p1,pins), BuildingScore <| p1+pins)::rest
                                                             else FrameF (CompleteF (p1,pins), Scored <| p1+pins)::rest
    | FrameF (InProgressBonusF1 p1, _)::rest -> FrameF (InProgressBonusF2 (p1,pins), BuildingScore <| p1+pins)::rest
    | FrameF (InProgressBonusF2 (p1,p2), _)::rest -> FrameF (CompleteBonusF (p1,p2,pins), Scored <| p1+p2+pins)::rest
    | _ -> game // Really should error here?


let submitBowl pins game =
    let topFrameFinished game =
        match game with
        | FrameR (Complete (_,_), _)::_ -> true
        | FrameR (Spare (_,_), _)::_ -> true
        | FrameR (Strike, _)::_ -> true
        | FrameF (CompleteF (_,_), _)::_ -> true
        | FrameF (CompleteBonusF (_,_,_), _)::_ -> true
        | _ -> false
    let gameFinished game =
        match game with
        | FrameF (CompleteF (_,_), _)::_ -> true
        | FrameF (CompleteBonusF (_,_,_), _)::_ -> true
        | _ -> false
    
    let newGame = evaluateBowl pins game

    if topFrameFinished newGame then
        if gameFinished newGame then newGame
        else if (List.length game) < 9 then FrameR (Unplayed, Unscored)::newGame else FrameF (UnplayedF, Unscored)::newGame
    else newGame


// TESTS

if (createNewGame) <> [FrameR (Unplayed, Unscored)] then printfn "newGame test failed" else printfn "newGame test passed"

if List.fold (fun game pins -> submitBowl pins game) createNewGame [1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1] <>
    [FrameF <| (CompleteF (1,1), Scored 2); FrameR <| (Complete (1,1), Scored 2); FrameR <| (Complete (1,1), Scored 2); FrameR <| (Complete (1,1), Scored 2); FrameR <| (Complete (1,1), Scored 2);
     FrameR <| (Complete (1,1), Scored 2); FrameR <| (Complete (1,1), Scored 2); FrameR <| (Complete (1,1), Scored 2); FrameR <| (Complete (1,1), Scored 2); FrameR <| (Complete (1,1), Scored 2)]
then
    printfn "Simple game test failed"
else
    printfn "Simple game test passed"
