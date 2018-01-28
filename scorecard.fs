// Bowling scorecard API
module Bowling

// Some values for game rules
let numberOfFramesInBowling = 10
let numberOfPinsInBowling = 10

// The ball type here should make displaying the scorecard easier if we
// wanted to do that.  
type Ball =
    | Invalid
    | Gutter
    | Hit of int
    | Strike

type RegularFrameBalls =
    | Empty
    | InProgress of Ball
    | Regular of Ball * Ball
    | Spare of Ball * Ball
    | Strike

type FinalFrameBalls =
    | Empty
    | InProgress of Ball
    | InProgressWithBonus of Ball * Ball
    | CompleteNoBonus of Ball * Ball
    | CompleteWithBonus of Ball * Ball * Ball

type FrameScore =
    | Unscored
    | BuildingScore of int
    | Scored of int


type RegularFrame = {balls:RegularFrameBalls; score:FrameScore}
type FinalFrame = {balls:FinalFrameBalls; score:FrameScore}

type Frame = RegularFrame of RegularFrame | FinalFrame of FinalFrame

type Game = Frame list

let newFrame = RegularFrame {balls=RegularFrameBalls.Empty; score=Unscored}
let newFinalFrame = FinalFrame {balls=FinalFrameBalls.Empty; score=Unscored}
let createNewGame = [ newFrame ]


// Now for some convenience functions
let scoreCard = createNewGame

let frameEmptyOrInProgress frame = 
    match frame with
        | RegularFrame f ->
            match f.balls with
                | RegularFrameBalls.Empty -> true
                | RegularFrameBalls.InProgress _ -> true
                | _ -> false
        | FinalFrame f ->
            match f.balls with
                | FinalFrameBalls.Empty -> true
                | FinalFrameBalls.InProgress _ -> true
                | FinalFrameBalls.InProgressWithBonus (_)-> true
                | _ -> false

let getCurrentFrame game = List.head game


let classifyBall pinsKnockedDown =
    match pinsKnockedDown with
    | 0 -> Gutter
    | i when i <= 9 -> Hit i
    | numberOfPinsInBowling -> Ball.Strike
    | _ -> Invalid


let ballScore ball =
    match ball with
    | Gutter -> 0
    | Hit i -> i
    | Ball.Strike -> numberOfPinsInBowling
    | Invalid -> 0

    
// Given a frame and a number of pins knocked down, return the new frame
let updateFrame (frame:RegularFrame) pinsKnockedDown =
    let
        ball = classifyBall pinsKnockedDown
    in
        match frame.balls with
            | RegularFrameBalls.Empty ->
                if pinsKnockedDown <> numberOfPinsInBowling then RegularFrame {balls=RegularFrameBalls.InProgress ball; score=BuildingScore pinsKnockedDown}
                else RegularFrame {balls=Strike; score=BuildingScore 10}
            | RegularFrameBalls.InProgress x -> 
                let
                    totalPins = ballScore x + pinsKnockedDown
                in
                    if totalPins <> 10 then RegularFrame {balls=RegularFrameBalls.Regular (x,ball); score=Scored totalPins}
                    else RegularFrame {balls=RegularFrameBalls.Spare (x,ball); score=BuildingScore 10}
            | _ ->
                RegularFrame frame

// Recalculate the scores for a game
let updateScores game =
    game

let framesInGame game =
    List.length game

    
// Our main API function
let submitBowl game pinsKnockedDown =
    let currentFrame = getCurrentFrame game
    let updatedFrame = 
        match currentFrame with
        | RegularFrame f -> updateFrame f pinsKnockedDown
        
    let newGame = match framesInGame game with
                  | i when i < numberOfFramesInBowling-1 -> if frameEmptyOrInProgress updatedFrame then updatedFrame :: List.tail game else newFrame :: updatedFrame :: List.tail game
                  | i when i = numberOfFramesInBowling-1 -> if frameEmptyOrInProgress updatedFrame then updatedFrame :: List.tail game else newFinalFrame :: updatedFrame :: List.tail game
    in
      updateScores newGame
