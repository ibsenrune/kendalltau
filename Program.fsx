open System

type Item = { Title : string; Value1 : int; Value2 : int }

let rec unorderedPairs xs =
  match xs with
  | x::xs -> seq { 
      yield! List.map (fun y -> x,y) xs
      yield! unorderedPairs xs
    }
  | [] -> Seq.empty  

let kendallTau (ls : Item list) =
  let areDiscordant (i1 : Item, i2: Item) =
    (i1.Value1 - i2.Value1) * (i1.Value2 - i2.Value2) < 0
  let pairsThatAreDiscordant = unorderedPairs ls |> Seq.filter areDiscordant
  pairsThatAreDiscordant |> Seq.length

//Returns percentage (range 0 to 1) of item pairs that have changed order
let normalisedKendallTau xs =
  let count = List.length xs
  let numPairs = count * (count - 1) / 2
  let distance = kendallTau xs
  (float distance) / (float numPairs)

let example : Item list = [
    { Title = "A"; Value1 = 1;  Value2 = 6  }
    { Title = "B"; Value1 = 2;  Value2 = 2  }
    { Title = "C"; Value1 = 3;  Value2 = 3  }
    { Title = "D"; Value1 = 4;  Value2 = 4  }
    { Title = "E"; Value1 = 5;  Value2 = 5  }
    { Title = "F"; Value1 = 6;  Value2 = 1  }
    { Title = "G"; Value1 = 7;  Value2 = 7  }
    { Title = "H"; Value1 = 8;  Value2 = 8  }
    { Title = "I"; Value1 = 9;  Value2 = 9  }
    { Title = "J"; Value1 = 10; Value2 = 10 }
]  
