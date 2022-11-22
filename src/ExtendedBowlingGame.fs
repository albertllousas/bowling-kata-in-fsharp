module ExtendedBowlingGame

open FSharpx.Result
open System

type Frame = 
  | Strike
  | Spare of firstRoll: int
  | OpenFrame of firstRoll: int * secondRoll: int option
  | TenthFrame of frame: Frame * firstBonusRoll: int option * secondBonusRoll: int option

type GameError = InvalidNumberOfPinsKnockedDown of pins : int

type Score = int

type BowlingGame = { Frames : (Frame * Score option) list }

module Frame = 

  let isCompleted frame =  
    match (frame) with
      | OpenFrame(x, None) -> false
      | TenthFrame(OpenFrame(_, None), _ , None) | TenthFrame(Strike, _ , None) | TenthFrame(Spare(_), None, _) -> false
      | _ -> true

  let rec nextBall frame =  
    match (frame) with
      | OpenFrame(x, _) -> Some x
      | Spare(x) -> Some x
      | Strike -> Some 10
      | TenthFrame(f, _, _) -> nextBall f

  let rec nextTwoBalls frame frame' =  
    match (frame, frame') with  
      | (OpenFrame(x, Some y), _) -> (Some x, Some y)
      | (OpenFrame(x, None), _) -> (Some x, None)
      | (Spare(x), _) -> (Some x, Some (10 - x))
      | (Strike, Some f) -> (Some 10, nextBall f)
      | (TenthFrame(Strike, Some x, _), _) -> (Some 10, Some x)
      | (TenthFrame(f, _, _), _) -> nextTwoBalls f None
      | _ -> (None, None)

  let createNew pins currentFramePos = 
    if (pins > 10 || pins < 0) then Error (InvalidNumberOfPinsKnockedDown pins)
    else
      let next = if pins < 10 then OpenFrame(pins, None) else Strike   
      if currentFramePos = 9 then TenthFrame(next, None, None) else next
      |> Ok

  let rec complete pins frame = 
    match frame with 
      | OpenFrame(x, None) when (pins + x) < 10 -> Ok <| OpenFrame(x, Some pins)
      | OpenFrame(x, None) when (pins + x) = 10 -> Ok <| Spare(x)
      | OpenFrame(x, None) when (pins + x) > 10 -> Error <| (InvalidNumberOfPinsKnockedDown pins)
      | TenthFrame(OpenFrame(x, None), None, None) -> complete pins (OpenFrame(x, None)) |> Result.map (fun f -> TenthFrame(f, None, None))
      | TenthFrame(f, None, None) -> Ok <| TenthFrame(f, Some pins, None)
      | TenthFrame(Strike, Some x, None) -> Ok <| TenthFrame(Strike, Some x, Some pins)
      | f -> Ok f

module BowlingGame =  

  let start : BowlingGame = { Frames = [] } 

  let rec private calculateScoreOf frame nextTwoBalls = 
    match (frame, fst nextTwoBalls, snd nextTwoBalls) with 
      | (OpenFrame(x, Some y), _, _) -> Some (x + y)
      | (Spare(_), Some x, _) -> Some (x + 10)
      | (Strike, Some x, Some y) -> Some (x + y + 10)
      | (TenthFrame(OpenFrame(x, Some y), _, _), _, _) -> Some (x + y)
      | (TenthFrame(f, x, y), _, _) -> calculateScoreOf f (x, y)
      | _ -> None 

  let rec private calculatePartialScores frames = 
    match frames with 
      | x :: y :: z :: tail -> (x, calculateScoreOf x (Frame.nextTwoBalls y (Some z))) :: calculatePartialScores (y :: z :: tail)
      | x :: y :: tail -> (x, calculateScoreOf x (Frame.nextTwoBalls y None)) :: calculatePartialScores (y :: tail)
      | x :: tail -> (x, calculateScoreOf x (None, None)) :: calculatePartialScores tail
      | [] -> []

  let private prependTo list result = result |> Result.map (fun ele -> ele :: list) 

  let knockDown (pins:int) (game:BowlingGame) : Result<BowlingGame, GameError> = 
    result {
      let pos = (List.length game.Frames)
      let! revFrames = match game.Frames |> List.map fst |> List.rev  with 
                          | [] -> Frame.createNew pins 0 |> prependTo [] 
                          | head :: tail when Frame.isCompleted head -> Frame.createNew pins pos |> prependTo (head :: tail)
                          | head :: tail -> Frame.complete pins head |> prependTo tail
      let frames = List.rev revFrames
      let scoredFrames = calculatePartialScores frames
      return { game with Frames = scoredFrames }
    }

  let play sequenceOfRolls = (Ok start, sequenceOfRolls) ||> List.fold (fun game roll -> Result.bind (knockDown roll) game)

  let score game = game |> Result.map (
    fun game -> game.Frames |> List.map (fun f ->  (0, snd f) ||> Option.defaultValue) |>  List.sum 
    )  
  
