module Program

open System
open Debug

type SodokuSlice = ROW | COLUMN | SUBGRID

let getDuplicates numbers =
    numbers |> Array.groupBy id
            |> Array.choose (fun( key, set ) ->
                if set.Length > 1
                    then Some key
                    else None)

let sliceValidator (sliceType: SodokuSlice) (matrix: inref<int[,]>) (index: int): bool =
    let slice = match sliceType with
                    | ROW ->  matrix[index, *]
                    | COLUMN ->  matrix[*, index]
                    | SUBGRID ->
                        let arr: int[] = Array.init 9 (fun x -> (0))
                        let mutable i = 0
                        let xStart = (index % 3)  * 3
                        for x in xStart .. xStart + 2 do
                            let yStart =  int(Math.Ceiling(float(index / 3))) * 3
                            for y in yStart .. yStart + 2 do
                                arr[i] <- matrix[x,y]
                                i <- i + 1
                        arr                

    let isSumCorrect = Array.sum slice = 45
    let isNotDuplicate = (getDuplicates  slice).Length = 0

    isSumCorrect && isNotDuplicate

[<EntryPoint>]
let main args =
    let grid: int[,] = Array2D.init 9 9 (fun x y -> (0))

    for i in 0 .. 9 - 1 do
        let words = basicGrid[i].Split [|' '|]
        for j in 0 .. 9 - 1 do
            let n = int(words.[j])
            grid[i, j] <- n
        ()

    ()


    let mutable result = true

    for i in 0 .. 8 do
        let isRowOk = sliceValidator SodokuSlice.ROW &grid  i
        let isColumnOk = sliceValidator SodokuSlice.COLUMN &grid i
        let isSubgridOK = sliceValidator SodokuSlice.SUBGRID &grid i

        result <- (isRowOk && isColumnOk && isSubgridOK && result)

    printfn "RESULT: %b" result

    0
