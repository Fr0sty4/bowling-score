// Bowling scorecard API
module Bowling

type FrameBalls =
    | Empty
    | Incomplete of int
    | Regular of int * int
    | Spare of int * int
    | Strike

type FinalFrameBalls =
    | Empty
    | Incomplete of int
    | NoBonus of int * int
    | SpareIncomplete
    | StrikeIncomplete
    | SpareWithBonus of int * int * int
    | StrikeWithSpare of int * int
    | DoubleStrikeIncomplete
    | DoubleStrikeWithBonus of int
    | Turkey


type FrameScore =
    | Unscored
    | BuildingScore of int
    | Scored of int

type RegularFrame = {balls:FrameBalls; score:FrameScore}
type FinalFrame = {balls:FinalFrameBalls; score:FrameScore}

type PlayFrame = Regular of RegularFrame | Final of FinalFrame

type Game = PlayFrame list

let newFrame = Regular {balls=FrameBalls.Empty; score=Unscored}
let newFinalFrame = Final {balls=FinalFrameBalls.Empty; score=Unscored}
let createNewGame = [ newFrame ]

let scoreCard = createNewGame

// Now for some convenience functions
let frameEmptyOrIncomplete frame = 
    match frame with
        | Regular f ->
            match f.balls with
                | FrameBalls.Empty -> true
                | FrameBalls.Incomplete _ -> true
                | _ -> false
        | Final f ->
            match f.balls with
                | FinalFrameBalls.Empty -> true
                | FinalFrameBalls.Incomplete _ -> true
                | FinalFrameBalls.SpareIncomplete _ -> true
                | FinalFrameBalls.StrikeIncomplete _ -> true
                | _ -> false

let getCurrentFrame game = if frameEmptyOrIncomplete (List.head game) then List.head game else newFrame


// Given a frame and a number of pins knocked down, return the new frame
let updateFrame frame pinsKnockedDown =
    match frame.bowl with
        | Empty -> {bowl=Incomplete pinsKnockedDown; score=BuildingScore pinsKnockedDown}
        | Incomplete x -> {bowl=Regular (x,pinsKnockedDown); score=Scored pinsKnockedDown}
        | _ -> frame


// Recalculate the scores for a game
let updateScores game =
    game
    
// Our main API function
let submitBowl game pinsKnockedDown =
    let currentFrame = getCurrentFrame game
    let newFrame = updateFrame currentFrame
    let newGame = if frameEmptyOrIncomplete (List.head game) then newFrame :: List.tail game  else newFrame :: game
    in
      updateScores newGame

