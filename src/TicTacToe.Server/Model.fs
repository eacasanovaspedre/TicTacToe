namespace TicTacToe

// type CellValue =
//     | X
//     | O
//     | Blank
//
// type Row = Row of CellValue * CellValue * CellValue
//
// type Board = Board of Row * Row * Row

type CellValue =
    | X
    | O

type ColumnPosition =
    | Left
    | CenterCol
    | Right
    
type RowPosition =
    | Top
    | CenterRow
    | Bottom
    
type CellPosition = CellPos of RowPosition * ColumnPosition

type Board = Board of Map<CellPosition, CellValue>        

type BoardState =
    | CanContinue
    | XWins
    | OWins

module Board =
        
    let allColumns = [ Left; CenterCol; Right ]
    
    let empty = Board Map.empty
    
    let move position cellValue (Board map) =
        Board (Map.add position cellValue map)
        
    let toConsole (Board map) =
        let printRow row =
            allColumns
            |> Seq.map (fun col -> CellPos (row, col))
            |> Seq.map (fun pos -> Map.tryFind pos map)
            |> Seq.map (fun cellValueOpt ->
                match cellValueOpt with
                | Some value -> $"{value}"
                | None -> " ")
            |> Seq.iteri (fun index str -> printf (if index = 0 then "%s" else "|%s") str)
        printRow Top
        printfn ""
        printfn "_____"
        printRow CenterRow
        printfn ""
        printfn "_____"
        printRow Bottom