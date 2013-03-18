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

 (*
  1 7 0 5 0 0 6 8 0 
  3 5 2 0 0 8 1 0 7 
  0 4 0 1 0 7 0 0 9 
  0 0 0 4 0 0 2 9 0 
  0 0 6 0 5 0 8 0 0 
  0 9 1 0 0 6 0 0 0 
  5 0 0 9 0 4 0 6 0 
  8 0 4 7 0 0 9 3 5 
  0 2 7 0 0 5 0 1 8 *)
//Console.Write("OUTPUT: " + sudoku.squares.ToString())
let missingVals board =
  List.filter (fun (va) -> List.nth va 2 = 0) board

let extract x y board =
  List.filter (fun (cell) ->
               let cellX = List.nth cell 0
               let cellY = List.nth cell 1
               cellX = x && cellY = y) board 

let nestedLoop xFrom xTo yFrom yTo =
  [for x in xFrom..xTo do for y in yFrom..yTo do yield [x;y;]]
  
let makeQuad section =
  match section with
    | 1 -> nestedLoop 0 2 0 2
    | 2 -> nestedLoop 0 2 3 5
    | 3 -> nestedLoop 0 2 6 8 
    | 4 -> nestedLoop 3 5 0 2
    | 5 -> nestedLoop 3 5 3 5
    | 6 -> nestedLoop 3 5 6 8 
    | 7 -> nestedLoop 6 8 0 2
    | 8 -> nestedLoop 6 8 3 5
    | 9 -> nestedLoop 6 8 6 8
    | _ -> [] 

let makeRow rowNum =
  [for y in 0..8 do
   yield [rowNum;y;]]
let makeCol colNum =
  [for x in 0..8 do
   yield [x;colNum;]]

let filterNum num theList = List.filter (fun x -> x <> num) theList

let extractCellValues cells board =
  List.filter (fun x -> x <> 0) (List.map (fun (cell) -> match cell with | [x;y] -> List.nth ((extract x y board) |> List.head) 2 | _ -> 0) cells)
 
let inList num list = List.exists (fun x -> x = num) list
let firstRow x = x < 3
let secondRow x = x > 2 && x < 6
let thirdRow x = x > 5 
let firstCol y = firstRow y
let secondCol y = secondRow y
let thirdCol y = thirdRow y
let mapToQuad cell =
  match cell with
    | [x;y;value] when firstRow x && firstCol y  -> 1
    | [x;y;value] when firstRow x && secondCol y -> 2
    | [x;y;value] when firstRow x && thirdCol y  -> 3
    | [x;y;value] when secondRow x && firstCol y -> 4
    | [x;y;value] when secondRow x && secondCol y-> 5
    | [x;y;value] when secondRow x && thirdCol y -> 6
    | [x;y;value] when thirdRow x && firstCol y  -> 7
    | [x;y;value] when thirdRow x && secondCol y -> 8
    | [x;y;value] when thirdRow x && thirdCol y  -> 9
    
let findRow cell board = extractCellValues (makeRow (List.head cell)) board
let findQuad cell board = extractCellValues (cell |> mapToQuad |> makeQuad) board
let findCol cell board = extractCellValues (makeCol (cell |> List.tail |> List.head)) board

let missingFrom list =
  List.filter (fun x -> (not (inList x list))) [1..9]

let certainNewValue cell board =
  let colVals = (missingFrom (findCol cell board))
  let quadVals = (missingFrom (findQuad cell board))
  let rowVals = (missingFrom (findRow cell board))
  let possibleCols =
    List.filter (fun x -> ((inList x rowVals) && (inList x quadVals)))
     colVals
  let possibleRows =
    List.filter (fun x -> ((inList x colVals) && (inList x quadVals)))
     rowVals
  let possibleQuads =
    List.filter (fun x -> ((inList x colVals) && (inList x rowVals)))
     quadVals
  if (List.length possibleCols = 1) && (List.length possibleCols = 1) && (List.length possibleCols = 1) then
    List.head possibleCols
  else 0 // couldn't find a conclusive number

let matches cell1 cell2 =
  let [x1;y1;val1] = cell1 
  let [x2;y2;val2] = cell2 
  x1 = x2 && y1 = y2

let updateBoard board cell =
  List.map (fun oldCell -> if matches oldCell cell then cell else oldCell) board
  
//let printCellAnswer cell = printfn "Can set cell:%A to: %A" cell (certainNewValue cell)
//List.map printCellAnswer missingVals
let findAnswers board =
  let rec inner iboard =
    let emptyCells = missingVals iboard
    let optionallyReplaceCell cell =
      let [x;y;value] = cell
      if value = 0 then [x;y; (certainNewValue cell iboard)] else cell
    if List.length emptyCells = 0 then
      iboard
    else
      inner (List.map optionallyReplaceCell iboard)
  inner board 

printfn "Finding..."
printfn "%A" (findAnswers v)




