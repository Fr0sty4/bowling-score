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
let updateFrame frame pinsKnockedDown =
    let
        ball = classifyBall pinsKnockedDown
    in
        match frame.progress with
            | Unplayed ->
                if pinsKnockedDown <> numberOfPinsInBowling then
                    {progress=InProgress; throws=[ball];score=BuildingScore pinsKnockedDown}
                else
                    {progress=Complete; throws=ball::frame.throws;score=BuildingScore 10}
            | InProgress -> 
                let totalPins = (frame.throws |> List.sumBy ballScore) + pinsKnockedDown
                let finalScore = if totalPins <> numberOfPinsInBowling then Scored totalPins else BuildingScore numberOfPinsInBowling
                let progress = if totalPins=10 or (List.length frame.throws)=1 then Complete else InProgress
                in {progress=Complete; throws=ball::frame.throws;score=finalScore}
            | Complete ->
                frame

// Recalculate the scores for a game
let updateScores game =
    let scoreBeingBuilt sc =
        match sc with
        | BuildingScore _ -> true
        | _ -> false
    in
        game

let framesInGame game =
    List.length game

    
// Our main API function
let submitBowl game pinsKnockedDown =
    let currentFrame = getCurrentFrame game
    let updatedFrame = updateFrame currentFrame pinsKnockedDown
    let gameFinished = (List.length game)=numberOfFramesInBowling
        
    let newGame =
        if not gameFinished then
            if updatedFrame.progress <> Complete then updatedFrame :: List.tail game else newFrame :: updatedFrame :: List.tail game
        else
            game
    in
      updateScores newGame
