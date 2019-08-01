#load "Csv.fs"

open System
open Csv

let rec unorderedPairs xs =
  match xs with
  | x::xs -> seq { 
      yield! List.map (fun y -> x,y) xs
      yield! unorderedPairs xs
    }
  | [] -> Seq.empty  

let numberOfPairs n = n * (n-1) / 2

let (|Concordant|Discordant|Tied|) (i1 : Item, i2 : Item) =
  if i1.Rank1 = i2.Rank1 || i1.Rank2 = i2.Rank2 then Tied
  else if(i1.Rank1 - i2.Rank1) * (i1.Rank2 - i2.Rank2) < 0 then Discordant
  else Concordant 

let areConcordant = function Concordant -> true | _ -> false
let areDiscordant = function Discordant -> true | _ -> false

let tauDistance (ls : Item list) =
  let pairsThatAreDiscordant = unorderedPairs ls |> Seq.filter areDiscordant
  pairsThatAreDiscordant |> Seq.length

//Calculates Kendall's Tau-a coefficient
let tauCoefficientA (ls : Item list) =
  let pairs = unorderedPairs ls
  let numConcordant = pairs |> Seq.filter areConcordant |> Seq.length
  let numDiscordant = pairs |> Seq.filter areDiscordant |> Seq.length
  float (numConcordant - numDiscordant) / float (numConcordant + numDiscordant)

//Calculates Kendall's Tau-b coefficient.
//https://en.wikipedia.org/wiki/Kendall_rank_correlation_coefficient#Tau-b
let tauCoefficientB (ls : Item list) =
  let pairs = unorderedPairs ls
  let numConcordant = pairs |> Seq.filter areConcordant |> Seq.length
  let numDiscordant = pairs |> Seq.filter areDiscordant |> Seq.length
  let ties rank ls = ls |> Seq.filter (fun (x,y) -> rank x = rank y) |> Seq.map (fst >> rank) |> Seq.groupBy id |> Seq.map snd |> Seq.map Seq.length
  let tiedInFirstRanking  = pairs |> ties rank1
  let tiedInSecondRanking = pairs |> ties rank2
  let n0 = numberOfPairs (ls |> Seq.length)
  let n1 = tiedInFirstRanking |> Seq.map numberOfPairs |> Seq.sum
  let n2 = tiedInSecondRanking |> Seq.map numberOfPairs |> Seq.sum
  let x = (int64 (n0 - n1)) * (int64 (n0 - n2))
  float (numConcordant - numDiscordant) / sqrt (float (int64 (n0 - n1) * (int64 (n0 - n2))))

//Returns percentage (range 0 to 1) of item pairs that have changed order
let normalisedTauDistance xs =
  let count = List.length xs
  let distance = tauDistance xs
  (float distance) / (float (numberOfPairs count))

let example : Item list = [
    { Title = "A"; Rank1 = 1;  Rank2 = 1  }
    { Title = "B"; Rank1 = 2;  Rank2 = 2  }
    { Title = "C"; Rank1 = 3;  Rank2 = 3  }
    { Title = "D"; Rank1 = 4;  Rank2 = 4  }
    { Title = "E"; Rank1 = 5;  Rank2 = 5  }
    { Title = "F"; Rank1 = 6;  Rank2 = 6  }
    { Title = "G"; Rank1 = 7;  Rank2 = 7  }
    { Title = "H"; Rank1 = 8;  Rank2 = 8  }
    { Title = "I"; Rank1 = 9;  Rank2 = 9  }
    { Title = "J"; Rank1 = 10; Rank2 = 10 }
]  

let filepath = @"C:\Users\RuneIbsen\Downloads\rankings.csv"
let printNonScientific (f : float) = f.ToString("F20")

filepath |> readCsv |> tauCoefficientB |> printNonScientific
example |> tauCoefficientB |> printNonScientific