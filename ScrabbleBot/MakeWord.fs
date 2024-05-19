module internal MakeWord

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
        | false -> checkTile 0 1 && checkTile 0 (-1)
        | true -> checkTile 1 0 && checkTile (-1) 0
        
    let rec findPossibleSuffixes (prefixNode :Dict) (hand :MultiSet<(uint32*char)>) (x, y) lettersPlaced (dir :bool) =
        let rec loop (currentNode :Dict) (currentPieces :MultiSet<(uint32*char)>) (charTup :uint32*char) (currentSuffix :(uint32*char) List) acc =
            match step (snd charTup) currentNode with
            | None -> acc               // Dead end
            | Some (true, _) -> Set.add currentSuffix acc  // Current path forms a valid terminal word (suffix)
            | Some (false, children) -> // Current path does not form a valid word or is not a terminal node
                if areSurroundingTilesEmpty (x, y) lettersPlaced dir then
                    MultiSet.fold
                        (fun state charTup _ ->
                            let unusedPieces :MultiSet<(uint32*char)> = MultiSet.removeSingle charTup currentPieces
                            let suffixList :(uint32*char) List= currentSuffix @ [charTup]
                            loop children unusedPieces charTup suffixList state
                        ) acc currentPieces
                else
                    acc
        // Initialize the loop with an empty suffix and start on node after prefix
        MultiSet.fold
            (fun acc charTup _ ->
                let unusedPieces = MultiSet.removeSingle charTup hand
                loop prefixNode unusedPieces charTup [charTup] acc
            ) Set.empty hand
