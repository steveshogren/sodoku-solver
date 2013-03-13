open System

module sudoku =
    let square x = x * x
    let squares = List.map square [1 .. 10]
    let checklist alist =
      match alist with
        | [] -> 0
        | [a] -> 1
        | [a; b] -> 2
        | [a; b; c] -> 3
        | _ -> failwith "List too big"
    let isSet cell =
      match cell with
        | [x; y; 0] -> false
        | _ -> true

open System
let v = [
  [0; 0; 1];[0; 1; 7];[0; 2; 0];[0; 3; 5];[0; 4; 0];[0; 5; 0];[0; 6; 6];[0; 7; 8];[0; 8; 0];
  [1; 0; 3];[1; 1; 5];[1; 2; 2];[1; 3; 0];[1; 4; 0];[1; 5; 8];[1; 6; 1];[1; 7; 0];[1; 8; 7];
  [2; 0; 0];[2; 1; 4];[2; 2; 0];[2; 3; 1];[2; 4; 0];[2; 5; 7];[2; 6; 0];[2; 7; 0];[2; 8; 9];
  [3; 0; 0];[3; 1; 0];[3; 2; 0];[3; 3; 4];[3; 4; 0];[3; 5; 0];[3; 6; 2];[3; 7; 9];[3; 8; 0];
  [4; 0; 0];[4; 1; 0];[4; 2; 6];[4; 3; 0];[4; 4; 5];[4; 5; 0];[4; 6; 8];[4; 7; 0];[4; 8; 0];
  [5; 0; 0];[5; 1; 9];[5; 2; 1];[5; 3; 0];[5; 4; 0];[5; 5; 6];[5; 6; 0];[5; 7; 0];[5; 8; 0];
  [6; 0; 5];[6; 1; 0];[6; 2; 0];[6; 3; 9];[6; 4; 0];[6; 5; 4];[6; 6; 0];[6; 7; 6];[6; 8; 0];
  [7; 0; 8];[7; 1; 0];[7; 2; 4];[7; 3; 7];[7; 4; 0];[7; 5; 0];[7; 6; 9];[7; 7; 3];[7; 8; 5];
  [8; 0; 0];[8; 1; 2];[8; 2; 7];[8; 3; 0];[8; 4; 0];[8; 5; 5];[8; 6; 0];[8; 7; 1];[8; 8; 8];]

 (* 1 7 0 5 0 0 6 8 0 
  3 5 2 0 0 8 1 0 7 
  0 4 0 1 0 7 0 0 9 
  0 0 0 4 0 0 2 9 0 
  0 0 6 0 5 0 8 0 0 
  0 9 1 0 0 6 0 0 0 
  5 0 0 9 0 4 0 6 0 
  8 0 4 7 0 0 9 3 5 
  0 2 7 0 0 5 0 1 8 *)
//Console.Write("OUTPUT: " + sudoku.squares.ToString())
let filterd =
  List.filter (fun (va) -> List.nth va 2 = 0) v
let extract x y =
  List.filter (fun (cell) ->
               let cellX = List.nth cell 0
               let cellY = List.nth cell 1
               cellX = x && cellY = y) v
let makeQuad =
  [for x in 0..2 do
   for y in 0..2 do
   yield [x;y;]]

let makeRow rowNum =
  [for y in 0..8 do
   yield [rowNum;y;]]

let filterNum num theList = List.filter (fun x -> x <> num) theList

let extractCellValues cells =
  List.map (fun (cell) ->
            match cell with
            | [x;y] -> List.nth (extract x y |> List.head) 2
            | _ -> 0)
            cells
            
let findRow = extractCellValues (makeRow 0)  
let findQuad = extractCellValues makeQuad  

//for x::y::rest in [[1;2;];[3;4]] do
//  Console.Write("x: " + x.ToString() + " y: " + y.ToString())
printfn "%A" findQuad
printfn "%A" findRow
