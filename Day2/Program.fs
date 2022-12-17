open System.IO

// Part 1
printfn "What would your total score be if everything goes exactly according to your strategy guide?"

let lines = File.ReadLines("rockandpaper.txt")

type Move =
    | Rock = 1
    | Paper = 2
    | Scissors = 3

type Round = { OpponentMove: Move; MyMove: Move }

type Result =
    | OpponentWins = 0
    | MeWins = 6
    | Draw = 3

let play (round: Round) : int =
    LanguagePrimitives.EnumToValue round.MyMove
    + LanguagePrimitives.EnumToValue(
        match round.OpponentMove, round.MyMove with
        | Move.Rock, Move.Paper
        | Move.Paper, Move.Scissors
        | Move.Scissors, Move.Rock -> Result.MeWins

        | Move.Rock, Move.Scissors
        | Move.Paper, Move.Rock
        | Move.Scissors, Move.Paper -> Result.OpponentWins

        | _ -> Result.Draw
    )

let parseMove move =
    match move with
    | "A"
    | "X" -> Move.Rock
    | "B"
    | "Y" -> Move.Paper
    | "C"
    | "Z" -> Move.Scissors
    | _ -> failwith "Incorrect Data"

let calculateMove move result : Move =
    match result, move with
    | "X", "A" -> Move.Scissors //OpponentWin
    | "X", "B" -> Move.Rock //OpponentWin
    | "X", "C" -> Move.Paper //OpponentWin

    | "Y", "A" -> Move.Rock //Draw
    | "Y", "B" -> Move.Paper //Draw
    | "Y", "C" -> Move.Scissors //Draw

    | "Z", "A" -> Move.Paper //MeWin
    | "Z", "B" -> Move.Scissors //MeWin
    | "Z", "C" -> Move.Rock //MeWin
    | _ -> failwith "Incorrect Data"

let parseMoveMove (moves: List<string>) : Round =
    { OpponentMove = parseMove moves[0]
      MyMove = parseMove moves[1] }

let parseMoveResult (moves: List<string>) : Round =
    { OpponentMove = parseMove moves[0]
      MyMove = calculateMove moves[0] moves[1] }

let parseRounds parseFunc (lines: seq<string>) : seq<Round> =
    lines |> Seq.map (fun line -> line.Split " " |> Seq.toList |> parseFunc)

let totalScore = lines |> parseRounds parseMoveMove |> Seq.map play |> Seq.sum
printfn $"The total score for the strategy is {totalScore}"

// Part 2
let totalScore2 = lines |> parseRounds parseMoveResult |> Seq.map play |> Seq.sum
printfn $"The total score for the strategy is {totalScore2}"
