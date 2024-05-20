module internal MakeWord

    open MultiSet
    open ScrabbleUtil
    open ScrabbleUtil.Dictionary
    open State
    let isTileEmpty x y (st: state) =
        st.lettersPlaced |> Map.tryFind (x, y) |> Option.isNone
    
    let isValidStartPos ((x, y): coord) (dir: direction) (st: state) =
        match dir with
        | Right -> isTileEmpty (x-1) y st && isTileEmpty (x+1) y st
        | Down  -> isTileEmpty x (y-1) st && isTileEmpty x (y+1) st

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
        
        let rec buildMoves currentPos direction dict hand currentMove possibleMoves =
            let nextPos = next currentPos direction
            
            if areSurroundingTilesEmpty currentPos direction st then
                fold (fun acc id _val ->
                    let nextHand = removeSingle id hand

                    let PossibleMoves = 
                        Set.fold (fun acc (char, value) ->
                            match step char dict with
                            | Some (isTerminal, nextDict) ->
                                let nextMove = currentMove @ [currentPos, (id, (char, value))]
                                let nextPossibleMoves =
                                    if isTerminal then
                                        nextMove :: possibleMoves
                                    else
                                        possibleMoves
                                
                                buildMoves nextPos direction nextDict nextHand nextMove nextPossibleMoves @ acc
                            | None -> acc
                        ) [] (Map.find id tiles)

                    PossibleMoves @ acc
                ) possibleMoves hand
            else
                possibleMoves

        let buildWordsInDirection startPos dir =
            let nextPos = next startPos dir
            
            if isValidStartPos startPos dir st then
                match Map.tryFind startPos st.lettersPlaced with
                | Some startChar -> 
                    let startDict =
                        match step startChar st.dict with
                        | Some (_, nextDict) -> nextDict
                        | None -> st.dict
                    buildMoves nextPos dir startDict st.hand [] []
                | None ->
                    buildMoves startPos dir st.dict st.hand [] []
            else
                []
                
        let possibleWordsFromPos startPos =
           let right = buildWordsInDirection startPos Right
           let down = buildWordsInDirection startPos Down
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