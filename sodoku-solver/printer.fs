namespace sodokusolver
module printer =
  let print board =
    for x in 0..8 do 
      printfn "%A\n"  (Seq.take 9 board)
