namespace sodokusolver
module parser =
  open System
  let convertToValue chr =
    if chr = "." then 0
    else Int32.Parse chr
  let makeIntoLists numList =
    let emptyBoard = [for x in 0..8 do
                      for y in 0..8 do
                      yield [x;y;]]
    Seq.map2 (fun ([x;y;]) (value) -> [x;y;value;]) emptyBoard numList
    
  let parse (input:string) =
    input |> Seq.map (fun (x) -> convertToValue (x.ToString())) |> makeIntoLists


