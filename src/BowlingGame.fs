module BowlingGame

  let score (sequenceOfRolls: int list) : int = 
    let rec scoreInner rolls acc =
      match rolls with
        | [] -> acc
        | x :: x' :: xs when x + x' < 10 -> scoreInner xs (acc + x + x') // Open Frame
        | x :: x' :: x'' :: [] -> (acc + x + x' + x'') // last frame
        | x :: x' :: x'' :: xs when x + x' = 10 -> scoreInner (x'' :: xs) (acc + x + x' + x'') // Spare
        | x :: x' :: x'' :: xs when x = 10 -> scoreInner (x' :: x'' :: xs) (acc + x + x' + x'') // Strike
    scoreInner sequenceOfRolls 0    
