module ExtendedBowlingGameTests

open ExtendedBowlingGame
open Xunit
open Expecto
open ExtendedBowlingGame.BowlingGame

let assertThat actual assertFn = assertFn actual

let isEqualTo expected actual = Expect.equal expected actual "" 

let firstFrame game = game |> Result.map (fun g -> List.head g.Frames)

[<Tests>]
let tests =

  testList "Bowling" [  

    testList "Scoring" [

      test "If in two tries, there are some pins still standing, the score for that frame is the total number of pins knocked down in the two tries" {
        let currentGame = start |> knockDown 3 |> Result.bind (knockDown 3)
        assertThat (firstFrame currentGame) isEqualTo (Ok (OpenFrame(firstRoll = 3, secondRoll = Some 3), Some 6))
      }

      test "If in two tries, all the pins are knocked down, this is called a spare and the score is ten plus the number of pins knocked down on the next throw" {
        let currentGame = start |> knockDown 5 |> Result.bind (knockDown 5) |> Result.bind (knockDown 5)
        assertThat (firstFrame currentGame) isEqualTo (Ok (Spare(firstRoll = 5), Some 15))
      }

      let expectations = [ ([10; 5; 5], 20, "Next : Spare"); ([10; 10; 10], 30, "Next : Two strikes"); ([10; 1; 1], 12, "Next : Open frame") ]
      for (rolls, score, frame) in expectations do
        test $"If on the first try, all the pins are knocked down, this is called a strike. Turn is over and the score is ten plus next two rolls ({frame})." {
          let currentGame = play rolls
          assertThat (firstFrame currentGame) isEqualTo (Ok (Strike, Some score))
        }
      
      let expectations = [ ([11], (InvalidNumberOfPinsKnockedDown 11)); ([-1], (InvalidNumberOfPinsKnockedDown -1)); ([5; 6], (InvalidNumberOfPinsKnockedDown 6));]
      for (rolls, error) in expectations do
        test $"It should inform with an error when pins knocked down are not valid {rolls}" {
          let currentGame = play rolls
          assertThat currentGame isEqualTo (Error error)
        }
      
      test "12 rolls: 12 strikes" {
        let currentGame = play [10;10;10;10;10;10;10;10;10;10;10;10]
        let total = score currentGame
        assertThat total isEqualTo (Ok 300)
      }

      test "20 rolls: 10 pairs of 9 and miss" {
        let currentGame = play [9;0;9;0;9;0;9;0;9;0;9;0;9;0;9;0;9;0;9;0]
        let total = score currentGame
        assertThat total isEqualTo (Ok 90)
      }

      test "21 rolls: 10 pairs of 5 and spare, with a final 5" {
        let currentGame = play [5;5;5;5;5;5;5;5;5;5;5;5;5;5;5;5;5;5;5;5;5]
        let total = score currentGame
        assertThat total isEqualTo (Ok 150)
      }
  ] 
]
