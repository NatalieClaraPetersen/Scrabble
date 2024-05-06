module internal MakeWord

    open MultiSet
    open Parser
    open ScrabbleUtil
    open ScrabbleUtil.Dictionary
    
    let getNodeAfterPrefix (trieRoot: Dict) (prefix: string) =
        let rec loop node chars =
            match chars with
            | [] -> node  // Entire prefix is valid if we've processed all characters
            | char :: remainingChars ->
                match ScrabbleUtil.Dictionary.step char node with
                | Some (_, childNode) -> loop childNode remainingChars
                | None -> trieRoot  // Prefix not found in trie
        loop trieRoot (List.ofSeq prefix)
        
    let rec findPossibleSuffixes (dict :Dict) (hand :MultiSet<(uint32*char)>) (prefix :string) =
        let rec loop (currentNode :Dict) (currentPieces :MultiSet<(uint32*char)>) (charTup :uint32*char) (currentSuffix :(uint32*char) List) acc =
            match step (snd charTup) currentNode with
            | None -> acc               // Dead end
            | Some (true, _) -> Set.add currentSuffix acc  // Current path forms a valid terminal word (suffix)
            | Some (false, children) -> // Current path does not form a valid word or is not a terminal node
                MultiSet.fold
                    (fun state charTup _ ->
                        let unusedPieces :MultiSet<(uint32*char)> = MultiSet.removeSingle charTup currentPieces
                        let suffixList :(uint32*char) List= currentSuffix @ [charTup]
                        loop children unusedPieces charTup suffixList state
                    ) acc currentPieces
        // Initialize the loop with an empty suffix and start on node after prefix
        let prefixNode = getNodeAfterPrefix dict prefix
        MultiSet.fold
            (fun acc charTup _ ->
                let unusedPieces = MultiSet.removeSingle charTup hand
                loop prefixNode unusedPieces charTup [charTup] acc
            ) Set.empty hand
