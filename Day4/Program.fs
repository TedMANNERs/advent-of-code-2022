open System
open System.IO

module CampCleanup =
    // Part 1
    printfn "In how many assignment pairs does one range fully contain the other?"
    let lines = File.ReadLines("section_assignments.txt")
    
    type SectionAssignmentPair =
        {Left: int seq
         Right: int seq}
        
    let parseSection (inputNumbers: string list) : int list =
        [ int inputNumbers[0].. int inputNumbers[1] ]
    
    let parseSectionAssignment (input: string list) : SectionAssignmentPair =
        { Left = parseSection ((input[0].Split '-') |> Seq.toList)
          Right = parseSection ((input[1].Split '-') |> Seq.toList) }        
    
    let parsePairs (input: string seq) : SectionAssignmentPair seq =
        input |> Seq.map (fun line -> line.Split ',' |> Seq.toList |> parseSectionAssignment)
        
    let isSubset (pair: SectionAssignmentPair) : bool =
        Set.isSubset (Set.ofSeq pair.Left) (Set.ofSeq pair.Right)
        || Set.isSubset (Set.ofSeq pair.Right) (Set.ofSeq pair.Left)
    let intersect (pair: SectionAssignmentPair) : int seq =
        Set.intersect (Set.ofSeq pair.Left) (Set.ofSeq pair.Right)
        
    let calculateFullOverlaps = Seq.map isSubset >> Seq.where id >> Seq.length
    let calculatePartialOverlaps = Seq.map intersect >> Seq.where (fun (x: int seq) -> Seq.length x > 0) >> Seq.length
        
    let fullyOverlappingPairsCount = lines |> parsePairs |> calculateFullOverlaps
    printfn $"Number of fully overlapping assignment pairs = {fullyOverlappingPairsCount}"
    
    let partiallyOverlappingPairsCount = lines |> parsePairs |> calculatePartialOverlaps
    printfn $"Number of partially overlapping assignment pairs = {partiallyOverlappingPairsCount}"