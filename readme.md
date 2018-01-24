# Bowling Task

## References
https://en.wikipedia.org/wiki/Ten-pin_bowling#Scoring

## My approach
After thinking this through a bit, I'm going to take the approach of having the API
centered around individual throws rather than frames.  Having a simpler signature for the
main scoring function allows us to more easily handle the special cases of spares and
strikes.  

Each frame will be stored as a record like this:
  Event : InProgressThrow (s1) | Throw (s1 s2) | Spare (s1 s2) | Strike
  Score : PartlyDetermined int | FullyDetermined int

A strike or spare results in a frame with a partly determined score until the subsequent 2 or
1 frames have completed respectively.  


## Scoring Rules
  - If r1 != 10 && (r1+r2 == 10) then throws are a Spare r1 r2
  - If r1 == 10 then throw is a Strike

  case (r1, r2) of:
    (10, _) -> Strike
    (x, y) -> if x+y == 10 then (Spare x y) else (Standard x y)

Once we have a stream of classified frames we can create some functions to calculate their score....
