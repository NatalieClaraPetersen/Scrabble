module internal MakeWord

    open System.Threading
    open MultiSet
    open Parser
    open ScrabbleUtil
    open ScrabbleUtil.Dictionary
    
    let getNodeAfterPrefix (trieRoot: Dict) (prefix: char) =
        match ScrabbleUtil.Dictionary.step prefix trieRoot with
        | Some (_, childNode) -> childNode
        | None -> trieRoot  // Prefix not found in trie
        
    let areSurroundingTilesEmpty (x, y) lettersPlaced dir =
        let checkTile dx dy = Map.tryFind (x + dx, y + dy) (Map.ofList lettersPlaced) |> Option.isNone
        match dir with
        | true  -> checkTile 0 1 && checkTile 0 (-1) && checkTile 1 0 // vandret skriveretning
        | false -> checkTile 1 0 && checkTile (-1) 0 && checkTile 0 1
        
    let rec findPossibleSuffixes (prefixNode :Dict) (hand :MultiSet<(uint32*char)>) (x, y) lettersPlaced (dir :bool) start =
        let rec loop (currentNode :Dict) (currentPieces :MultiSet<(uint32*char)>) (charTup :uint32*char) (x1, y1) (currentSuffix :(uint32*char) List) acc (start: bool) =
            match step (snd charTup) currentNode with
            | None -> acc                     // Dead end
            | Some (true, _) ->
                let coord = if dir then (x1+1,y1) else (x1,y1+1)
                let empty = areSurroundingTilesEmpty coord lettersPlaced dir
                if empty then
                    Set.add currentSuffix acc // Current path forms a valid terminal word (suffix)
                else
                    acc  
            | Some (false, children) ->       // Current path does not form a valid word or is not a terminal node
                let coord = if dir then (x1+1,y1) else (x1,y1+1)
                let empty = areSurroundingTilesEmpty coord lettersPlaced dir
                if empty || start then
                    MultiSet.fold
                        (fun state charTup _ ->
                            let unusedPieces :MultiSet<(uint32*char)> = MultiSet.removeSingle charTup currentPieces
                            let suffixList :(uint32*char) List= currentSuffix @ [charTup]
                            loop children unusedPieces charTup coord suffixList state false
                        ) acc currentPieces
                else
                    acc
        // Initialize the loop with an empty suffix and start on node after prefix
        MultiSet.fold
            (fun acc charTup _ ->
                let unusedPieces = MultiSet.removeSingle charTup hand
                loop prefixNode unusedPieces charTup (x, y) [charTup] acc start
            ) Set.empty hand
