// Bowling scorecard API
module Bowling

type FrameBowl =
    | Empty
    | Incomplete of int
    | Regular of int * int
    | Spare of int * int
    | Strike
    | FinalIncomplete of int * int
    | FinalSpare of int * int * int
    | FinalStrike of int * int * int

type FrameScore =
    | Unscored
    | BuildingScore of int
    | Scored of int

type Frame = {bowl:FrameBowl; score:FrameScore}

type Game = Frame list

let newFrame = {bowl=Empty; score=Unscored}
let createNewGame = [ newFrame ]

let scoreCard = createNewGame

// Now for some convenience functions
let frameEmptyOrIncomplete frame = 
    match frame.bowl with
        | Empty -> true
        | Incomplete _ -> true
        | _ -> false

let getCurrentFrame game = if frameEmptyOrIncomplete (List.head game) then List.head game else newFrame


// Our main API function
let submitBowl game pinsKnockedDown =
    let currentFrame = getCurrentFrame game
    let newFrame = match currentFrame.bowl with
                    | Empty -> {bowl=Incomplete pinsKnockedDown; score=BuildingScore pinsKnockedDown}
                    | Incomplete x -> {bowl=Regular (x,pinsKnockedDown); score=Scored pinsKnockedDown}
                    | _ -> currentFrame
    in
      if frameEmptyOrIncomplete (List.head game) then newFrame :: List.tail game  else newFrame :: game
