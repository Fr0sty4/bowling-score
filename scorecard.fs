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

type FrameProgression = Unplayed | InProgress | Complete
type FrameScore =
    | Unscored
    | BuildingScore of int
    | Scored of int

type Frame = {progress:FrameProgression;throws:Ball list; score:FrameScore}
type Game = Frame list

let newFrame = {progress=Unplayed;throws=[];score=Unscored}
let createNewGame = [ newFrame ]


// Now for some convenience functions
let scoreCard = createNewGame
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
        match frame.progress with
            | RegularFrameClass.Unplayed ->
                if pinsKnockedDown <> numberOfPinsInBowling then RegularFrame {progress=RegularFrameClass.InProgress; throws=[ball];score=BuildingScore pinsKnockedDown}
                else RegularFrame {progress=RegularFrameClass.Strike; throws=ball::frame.throws;score=BuildingScore 10}
            | RegularFrameClass.InProgress -> 
                let
                    totalPins = ballScore x + pinsKnockedDown
                in
                    if totalPins <> 10 then RegularFrame {progress=RegularFrameClass.Regular; throws=ball::frame.throws;score=Scored totalPins}
                    else RegularFrame {progress=RegularFrameClass.Spare; throws=ball::frame.throws;score=BuildingScore 10}
            | _ ->
                RegularFrame frame

// Recalculate the scores for a game
let updateScores game =
    let scoreBeingBuilt sc =
        match sc with
        | BuildingScore _ -> true
        | _ -> false
    let rec updateScoresR frames acc =
        match frames with
        | [RegularFrame ft1;RegularFrame ft]::rest when scoreBeingBuilt ft.score ->
            updateScoresR rest [ft1;ft]
        | [ft2;ft1;ft]::rest when frameNeedsScore ft ->
            updateScoresR rest [ft2;ft1;ft]
        | [] -> acc
    in
        updateScoresR game []


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
