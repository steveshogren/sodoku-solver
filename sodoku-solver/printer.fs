namespace sodokusolver
module printer =
  let print board =
    for cell in board do
      match cell with
        | [x;y;value] -> if y = 8 then printfn "%A" value else printf "%A_" value
        | _ -> printf "Nadda"
