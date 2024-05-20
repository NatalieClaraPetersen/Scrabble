module internal MakeWord

    open MultiSet
    open ScrabbleUtil
    open ScrabbleUtil.Dictionary
    open State
    let isTileEmpty x y (st: state) =
        st.lettersPlaced |> Map.tryFind (x, y) |> Option.isNone
    
    let isValidStartPos ((x, y): coord) (dir: direction) (st: state) =
        match dir with
        | Right -> isTileEmpty (x-1) y st
        | Down  -> isTileEmpty x (y-1) st

    let areSurroundingTilesEmpty ((x, y): coord) (dir: direction) (st: state) =
        match dir with
        | Right -> isTileEmpty x (y+1) st && isTileEmpty x (y-1) st && isTileEmpty (x+1) y st
        | Down  -> isTileEmpty (x+1) y st && isTileEmpty (x-1) y st && isTileEmpty x (y+1) st
       
    let tilesForSwappies (st: state) =
        let handList = st.hand |> toList |> List.rev
        let tilesToChangeCount = min st.tilesLeft 7
        List.take tilesToChangeCount handList
        
    let getNextMove (usedCoords: List<coord>) (tiles: Map<uint32, tile>) (st: state) =
        let next (x, y) = function
            | Right -> (x + 1, y)
            | Down -> (x, y + 1)
        
        let rec move pos direction dict hand currentMove possibleMoves startPos =
            let nextPos = next pos direction
            let isFirstLetter = pos = startPos

            match Map.tryFind pos st.lettersPlaced with
            | Some char ->
                match step char dict with
                | Some (isTerminal, nextDict) ->
                    let nextPossibleMoves =
                        if isTerminal &&
                           List.length currentMove > 0 &&
                           not isFirstLetter &&
                           areSurroundingTilesEmpty pos direction st
                        then
                            currentMove :: possibleMoves
                        else
                            possibleMoves
                    
                    move nextPos direction nextDict hand currentMove nextPossibleMoves startPos
                | None -> possibleMoves
            | None -> 
                checkNextPos pos direction dict hand currentMove possibleMoves startPos


        and checkNextPos pos direction dict hand currentMove possibleMoves startPos =
            let nextPos = next pos direction
            
            if areSurroundingTilesEmpty pos direction st then
                fold (fun acc id _val ->
                    let nextHand = removeSingle id hand

                    let nextPossibleMoves = 
                        Set.fold (fun acc (char, value) ->
                            let nextMove = currentMove @ [pos, (id, (char, value))]

                            match step char dict with
                            | Some (isTerminal, nextDict) ->
                                let newMoves =
                                    if isTerminal then
                                        nextMove :: possibleMoves
                                    else
                                        possibleMoves
                                
                                move nextPos direction nextDict nextHand nextMove newMoves startPos @ acc
                            | None -> acc
                        ) [] (Map.find id tiles)

                    nextPossibleMoves @ acc
                ) possibleMoves hand
            else
                possibleMoves

        let buildWordsInDirection pos dir =
            if isValidStartPos pos dir st then
                move pos dir st.dict st.hand [] [] pos
            else
                []
                
        let possibleWordsFromPos pos =
           let right = buildWordsInDirection pos Right
           let down = buildWordsInDirection pos Down
           right @ down
        
        let allPossibleWords = List.fold (fun acc pos -> acc @ possibleWordsFromPos pos) List.Empty usedCoords
        
        if allPossibleWords.IsEmpty then
            []
        else
            allPossibleWords |> List.maxBy List.length
                
    let getMove (tiles: Map<uint32, tile>) (st: state) =
        if st.lettersPlaced.IsEmpty then
            getNextMove [st.center] tiles st
        else
            let listOfUsedCoords = st.lettersPlaced |> Map.toList |> List.map fst
            getNextMove listOfUsedCoords tiles st