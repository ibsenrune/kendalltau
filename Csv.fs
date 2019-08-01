module Csv

open System

type Item = { Title : string; Value1 : int; Value2 : int }

let readLines path = System.IO.File.ReadAllLines path |> List.ofArray
let rec splitCsv (line : string) =
  let split (separator : string) (s : string) = s.Split([|separator|], System.StringSplitOptions.None)
  let trimOne (chr : char) (str : string) = 
    let trimOneStart (chr : char) (str : string) =
      if str.StartsWith(string chr) then str.Substring(1) else str 
    let trimOneEnd (chr : char) (str : string) =
      if str.EndsWith(string chr) then str.Substring(0, str.Length - 1) else str
    str |> trimOneStart '\"' |> trimOneEnd '\"'
  line |> trimOne '\"' |> split "\",\"" |> List.ofArray

let (|Int|_|) str =
  match Int32.TryParse(str) with
  | (true, value) -> Some value
  | (false, _) -> None

let parseItem line =
  match splitCsv line with
  | title::_::_::id::(Int rank1)::(Int rank2)::[] -> { Title = id; Value1 = rank1; Value2 = rank2 }
  | _ -> failwith (sprintf "Cannot parse line:\n\t%s" line)

let readCsv path =
    path 
    |> readLines 
    |> List.skip 1
    |> List.map parseItem

