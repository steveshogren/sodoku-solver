// .94...13..............76..2.8..1.....32.........2...6.....5.4.......8..7..63.4..8
namespace sodokusolver
module parser =
  open System
  let convertToValue char =
    if char = "." then 0
    else Int32.Parse char
  let parse input =
    input |> Seq.map convertToValue


