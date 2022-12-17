open System
open System.IO

// Part 1
printfn "Which Elf is carrying the most Calories?"
let text = File.ReadAllText("calories.txt")
let elves = text.Split $"{Environment.NewLine}{Environment.NewLine}"
let elfCalories = elves |> Seq.map (fun foodItems -> foodItems.Split Environment.NewLine |> Seq.sumBy int)
let maxCalories = elfCalories |> Seq.max
printfn $"Most Calories = {maxCalories}"

// Part 2
let sortedElfCalories = elfCalories |> Seq.sortDescending |> Seq.toList
printfn $"Sum of top three Elves carrying the most Calories = {sortedElfCalories[..2] |> List.sum}"