module BowlingGameTests

open BowlingGame
open Xunit
open Expecto

[<Tests>]
let tests =

  testList "Bowling" [  

    testList "Score a valid sequence of rolls" [

      test "If in two tries, there are some pins still standing, the score for that frame is the total number of pins knocked down in the two tries" {
        let sequenceOfRolls = [3;4]
        let score = BowlingGame.score sequenceOfRolls
        Assert.Equal(7, score)
      }

      test "If in two tries, all the pins are knocked down, this is called a spare and the score is ten plus the number of pins knocked down on the next throw" {
        let sequenceOfRolls = [5;5;2;2]
        let score = BowlingGame.score sequenceOfRolls
        Assert.Equal(12 + 4, score)
      }

      test "If on the first try, all the pins are knocked down, this is called a strike. His turn is over and the score is ten plus the total of the pins knocked down in the next two rolls." {
        let sequenceOfRolls = [10;5;2;2;2]
        let score = BowlingGame.score sequenceOfRolls
        Assert.Equal(17 + 7 + 4, score)
      }

      test "If the player gets a spare in the last frame, the bowler gets to throw one more bonus ball." {
        let sequenceOfRolls = [5;5;2]
        let score = BowlingGame.score sequenceOfRolls
        Assert.Equal(12, score)
      }

      test "If the player gets a strike in the last frame, the bowler gets to throw two more bonus balls." {
        let sequenceOfRolls = [10;5;2]
        let score = BowlingGame.score sequenceOfRolls
        Assert.Equal(17, score)
      }

      test "12 rolls: 12 strikes" {
        let sequenceOfRolls = [10;10;10;10;10;10;10;10;10;10;10;10]
        let score = BowlingGame.score sequenceOfRolls
        Assert.Equal(300, score)
      }

      test "20 rolls: 10 pairs of 9 and miss" {
        let sequenceOfRolls = [9;0;9;0;9;0;9;0;9;0;9;0;9;0;9;0;9;0;9;0]
        let score = BowlingGame.score sequenceOfRolls
        Assert.Equal(90, score)
      }

      test "21 rolls: 10 pairs of 5 and spare, with a final 5" {
        let sequenceOfRolls = [5;5;5;5;5;5;5;5;5;5;5;5;5;5;5;5;5;5;5;5;5]
        let score = BowlingGame.score sequenceOfRolls
        Assert.Equal(150, score)
      }
  ]
]
