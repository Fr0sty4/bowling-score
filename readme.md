# Bowling Task

## References
https://en.wikipedia.org/wiki/Ten-pin_bowling#Scoring

## Data structs / entities
Scorecard
  Frame (r1 r2)
  - where (r1 r2) are the results of each thorw in the frame
  - Since r2 is optional, maybe something like this:
    Frame (Int, Maybe Int)

A frame is actually composed of one or two throws; I'm wondering if it's better to work on a
series of throws.  This stream of throws would then be divided into frames with these
classifications:
  Standard r1 r2 - a non-strike and non-spare frame where r1 and r2 are the results of each
                   throw in the frame; we preserve r1 and r2 separately because we likely
                   want to show the scorecard with the option of showing both throws.  
  Spare r1 r2    - a spare; again, we might preserve r1 and r2 for display purposes.  
  Strike         - a strike; no need for r1 or r2 here as a strike implies the frame score.  
  
The above classification is possible with the suggested API, though I think I'm free to change
this.  

## Scoring Rules
  - If r1 != 10 && (r1+r2 == 10) then throws are a Spare r1 r2
  - If r1 == 10 then throw is a Strike

  case (r1, r2) of:
    (10, _) -> Strike
    (x, y) -> if x+y == 10 then (Spare x y) else (Standard x y)

Once we have a stream of classified frames we can create some functions to calculate their score....
