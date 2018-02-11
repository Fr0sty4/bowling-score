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

type Frame =
    { progress : FrameProgression
      throws : int list
      score : FrameScore }

type Game = Frame list

let newFrame =
    { progress = Unplayed
      throws = []
      score = Unscored }

let createNewGame = [ newFrame ]


// Now for some convenience functions
let scoreCard = createNewGame
let getCurrentFrame game = List.head game


////////////////////////////////////////////////////////////////////////////
// Utility functions for frames
////////////////////////////////////////////////////////////////////////////

let frameIsStrike frame =
    match frame.throws with
    | [x] when x=numberOfPinsInBowling -> true
    | _ -> false

let frameIsSpare frame =
    match frame.throws with
    | [x;y] when x+y=numberOfPinsInBowling -> true
    | _ -> false
    
    
// Given a frame and a number of pins knocked down, return the new frame
let updateFrame frame pinsKnockedDown frameIsFinal =
    match frame.progress with
        | Unplayed ->
            if pinsKnockedDown <> numberOfPinsInBowling then
                { progress = InProgress; throws = [pinsKnockedDown]; score = BuildingScore pinsKnockedDown}
            else
                { progress = Complete; throws = pinsKnockedDown::frame.throws; score = BuildingScore numberOfPinsInBowling}
        | InProgress -> 
            let totalPins = (frame.throws |> List.sum) + pinsKnockedDown
            let finalScore = if totalPins <> numberOfPinsInBowling then Scored totalPins else BuildingScore numberOfPinsInBowling
            let progress = if totalPins=numberOfPinsInBowling or (List.length frame.throws)=1 then Complete else InProgress
            in {progress=Complete; throws=pinsKnockedDown::frame.throws;score=finalScore}
        | Complete ->
            frame

// Recalculate the scores for a game
let getNumericScoreValue score =
    match score with
    | Unscored -> 0
    | BuildingScore s -> s
    | Scored s -> s

let updateScores game frameIsFinal =
    let scoreBeingBuilt sc =
        match sc with
        | BuildingScore _ -> true
        | _ -> false
    in
        match game with
        // Strike scoring cases - in-game
        | fr1::fr::rest when (scoreBeingBuilt fr.score) && (frameIsStrike fr) && (not <| frameIsStrike fr1) && (fr1.progress=Complete) ->
            fr1::{fr with score=Scored
                       (10 + (getNumericScoreValue fr1.score))}::rest
        | fr2::fr1::fr::rest when (scoreBeingBuilt fr.score) && (frameIsStrike fr) && (frameIsStrike fr1) && (fr2.progress=Complete) ->
            fr2::fr1::{fr with score=Scored
                            (20 + (getNumericScoreValue fr2.score))}::rest
        // Spare scoring case - in-game
        | fr1::fr::rest when (scoreBeingBuilt fr.score) && (frameIsSpare fr) && not (List.isEmpty fr1.throws) ->
            fr1::{fr with score=Scored
                        (10 + (List.head fr1.throws))}::rest
        | _ -> game
        

let framesInGame game =
    List.length game

    
// Our main API function
let submitBowl pinsKnockedDown game =
    let currentFrame = getCurrentFrame game
    let frameIsFinal = (List.length game)=numberOfFramesInBowling && (List.head game).progress <> Complete
    let updatedFrame = updateFrame currentFrame pinsKnockedDown frameIsFinal
    let gameFinished = frameIsFinal && (List.head game).progress = Complete
        
    let newGame =
        if not gameFinished then
            if updatedFrame.progress <> Complete then
                updateScores <| (updatedFrame :: List.tail game) <| frameIsFinal
            else
                newFrame :: (updateScores <| (updatedFrame :: List.tail game) <| frameIsFinal)
        else
            game
    in
      newGame
