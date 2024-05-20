module internal MakeWord

    open MultiSet
    open ScrabbleUtil
    open ScrabbleUtil.Dictionary
    open State
        
    let isGoodStartPos (x, y) (st: state) dir =
        let checkTile dx dy = Map.tryFind (x + dx, y + dy) st.lettersPlaced |> Option.isNone
        match dir with
        | Right -> checkTile -1 0
        | Down  -> checkTile 0 -1

    let areSurroundingTilesEmpty ((x, y): coord) (st: state) (dir: direction) (hasStarted: bool) =
        let checkTile dx dy = Map.tryFind (x + dx, y + dy) st.lettersPlaced |> Option.isNone
        match dir with
        | Right -> checkTile 0 1 && checkTile 0 -1 && (not hasStarted || checkTile 1 0)
        | Down  -> checkTile 1 0 && checkTile -1 0 && (not hasStarted || checkTile 0 1)

        
    let getLongestWord (words: List<move>) =
        match words with
        | [] -> []
        | _ -> List.maxBy List.length words
    
    let tilesLeftToSwap (st: state) (maxTiles: int): int =
        let amountOfPlayedLetters = Map.count st.lettersPlaced |> int
        let handSize = size st.hand |> int
        
        if amountOfPlayedLetters <= 97 then
            7
        else if amountOfPlayedLetters + handSize > 97 && amountOfPlayedLetters < 104 then
            maxTiles - amountOfPlayedLetters 
        else 0    
    
    let canSwap (st: state) =
        let tilesLeft = tilesLeftToSwap st 104
        let amountOfPlayedLetters = Map.count st.lettersPlaced |> int 
        amountOfPlayedLetters + tilesLeft < 104
        
    let handToList (st: state) =
        let tilesLeft = tilesLeftToSwap st 104
        let handList = toList st.hand
        List.take tilesLeft handList
        
    let getNextMove (usedCoords: List<coord>) (st: state) (tiles: Map<uint32, tile>) =
        let next (x, y) = function
            | Right -> (x + 1, y)
            | Down -> (x, y + 1)
        
        let rec move pos direction dict hand currentMove possibleMoves hasStarted startPos =
            let nextPos = next pos direction
            let nextHasStarted = if hasStarted then true else pos = startPos

            match Map.tryFind pos st.lettersPlaced with
            | Some char ->
                match step char dict with
                | Some (isTerminal, nextDict) ->
                    let nextPossibleMoves =
                        if isTerminal && List.length currentMove > 0 && nextHasStarted && areSurroundingTilesEmpty pos st direction nextHasStarted then
                            currentMove :: possibleMoves
                        else
                            possibleMoves
                    
                    move nextPos direction nextDict hand currentMove nextPossibleMoves nextHasStarted startPos
                | None -> possibleMoves
            | None -> 
                checkNextPos pos direction dict hand currentMove possibleMoves hasStarted startPos

        and checkNextPos pos direction dict hand currentMove possibleMoves hasStarted startPos =
            let nextPos = next pos direction
            
            if areSurroundingTilesEmpty pos st direction hasStarted then
                fold (fun acc id _val ->
                    let nextHand = removeSingle id hand
                    let nextPossibleMoves =
                        Set.fold (fun accu (char, value) ->
                            let nextMove = currentMove @ [pos, (id, (char, value))]

                            match step char dict with
                            | Some (isTerminal, nextDict) ->
                                let newMoves =
                                    if isTerminal && hasStarted then
                                        nextMove :: possibleMoves
                                    else
                                        possibleMoves
                                
                                move nextPos direction nextDict nextHand nextMove newMoves hasStarted startPos @ accu
                            | None -> accu
                        ) [] (Map.find id tiles)
                    nextPossibleMoves @ acc
                ) possibleMoves hand
            else
                possibleMoves

        let goDown pos =
            match isGoodStartPos pos st Down with // false = down, true = right
            | true -> move pos Down st.dict st.hand [] [] true pos
            | false -> []

        let goRight pos =
            match isGoodStartPos pos st Right with
            | true -> move pos Right st.dict st.hand [] [] true pos
            | false -> []
            
        getLongestWord (List.fold (fun acc pos ->
            let down = goDown pos
            let right = goRight pos
            acc @ down @ right 
        ) List.Empty usedCoords)

                
    let getMove (st: state) (tiles: Map<uint32, tile>) =
        if st.lettersPlaced.IsEmpty then
            getNextMove [st.board.center] st tiles
        else
            let listOfUsedCoords = st.lettersPlaced |> Map.toList |> List.map fst
            getNextMove listOfUsedCoords st tiles