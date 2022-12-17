open System
open System.IO

module RucksackReorganization =
    // Part 1
    printfn "What is the sum of the priorities of item types that appear in both compartments of each rucksack?"
    let lines = File.ReadLines("rucksacks.txt")

    type Rucksack =
        { LeftCompartment: int seq
          RightCompartment: int seq }

    type Rucksack with

        member x.FullCompartment = Seq.concat [ x.LeftCompartment; x.RightCompartment ]

    let findEqualItemsInCompartments (rucksack: Rucksack) : int =
        (Set.intersect (Set.ofSeq rucksack.LeftCompartment) (Set.ofSeq rucksack.RightCompartment))
            .MaximumElement

    let parseCompartment (input: char seq) : int seq =
        input
        |> Seq.map (fun itemChar ->
            match (Char.IsLower itemChar) with
            | true -> int itemChar - int 'a' + 1
            | false -> int itemChar - int 'A' + 27)

    let parseRucksacks (lines: string seq) : Rucksack seq =
        lines
        |> Seq.map (fun line ->
            { LeftCompartment = (parseCompartment line[.. (line.Length / 2) - 1])
              RightCompartment = (parseCompartment line[(line.Length / 2) ..]) })

    let sumSharedItemPriorities =
        lines |> parseRucksacks |> Seq.map findEqualItemsInCompartments |> Seq.sum

    printfn $"Sum of the priorities of items that appear in both rucksack compartments = {sumSharedItemPriorities}"

    // Part 2

    printfn "What is the sum of the priorities of the badges of each three-Elf group?"


    type ElvesGroup = { Rucksacks: Rucksack seq }

    let getGroups (lines: string list) =
        let rec getNextGroup (lines: string list) (groupsAcc: string list list) =
            match lines with
            | [] -> groupsAcc
            | x :: xs -> getNextGroup (xs |> List.skip 2) [ x :: (xs |> List.take 2) ] @ groupsAcc

        getNextGroup lines []


    let parseGroups (lineGroups: string[] seq) : ElvesGroup seq =
        lineGroups
        |> Seq.map (fun lineGroup -> { Rucksacks = parseRucksacks lineGroup })

    let findUnique (rucksacks: int Set list) : int =
        let first = (Set.intersect (Set.ofSeq rucksacks[0]) (Set.ofSeq rucksacks[1]))
        (Set.intersect first (Set.ofSeq rucksacks[2])).MaximumElement

    let findGroupBadges (rucksacks: Rucksack seq) : int =
        rucksacks
        |> Seq.map (fun rucksack -> Set.ofSeq rucksack.FullCompartment)
        |> Seq.toList
        |> findUnique
    // rucksacks |> Seq.map (fun rucksack -> Set.ofSeq rucksack.FullCompartment) |> Seq.reduce Set.intersect |> Set.maxElement

    let findBadges (groups: ElvesGroup seq) : int seq =
        groups |> Seq.map (fun group -> findGroupBadges group.Rucksacks)

    let groups = lines |> Seq.chunkBySize 3 |> parseGroups |> findBadges |> Seq.sum

    printfn $"{groups}"
