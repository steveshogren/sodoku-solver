namespace sodokusolver
module solve =
    let missingVals board =
      List.filter (fun (va) -> List.nth va 2 = 0) board

    let extract x y board =
      List.filter (fun (cell) ->
                   let cellX = List.nth cell 0
                   let cellY = List.nth cell 1
                   cellX = x && cellY = y) board 

    let nestedLoop xFrom xTo yFrom yTo =
      [for x in xFrom..xTo do for y in yFrom..yTo do yield [x;y;]]

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
    let makeQuad cell =
      match cell with
        | [x;y;value] when firstRow x && firstCol y  -> nestedLoop 0 2 0 2
        | [x;y;value] when firstRow x && secondCol y -> nestedLoop 0 2 3 5
        | [x;y;value] when firstRow x && thirdCol y  -> nestedLoop 0 2 6 8
        | [x;y;value] when secondRow x && firstCol y -> nestedLoop 3 5 0 2
        | [x;y;value] when secondRow x && secondCol y-> nestedLoop 3 5 3 5
        | [x;y;value] when secondRow x && thirdCol y -> nestedLoop 3 5 6 8
        | [x;y;value] when thirdRow x && firstCol y  -> nestedLoop 6 8 0 2
        | [x;y;value] when thirdRow x && secondCol y -> nestedLoop 6 8 3 5
        | [x;y;value] when thirdRow x && thirdCol y  -> nestedLoop 6 8 6 8

    let findRow cell board = extractCellValues (makeRow (List.head cell)) board
    let findQuad cell board = extractCellValues (cell |> makeQuad) board
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
