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
        
    let rec findPossibleSuffixes (dict: Dict) (hand: char list) (prefix :string) : (char List) Set =
        let rec loop currentNode currentPieces char currentSuffix (acc : (char List) Set) =
            match step char currentNode with
            | None -> acc               // Dead end
            | Some (true, _) -> Set.add currentSuffix acc  // Current path forms a valid terminal word (suffix)
            | Some (false, children) -> // Current path does not form a valid word or is not a terminal node
                List.fold
                  (fun state char ->
                    let unusedPieces = List.filter (fun c -> c <> char) currentPieces
                    loop children unusedPieces char (currentSuffix @ [char]) state
                  ) acc currentPieces
        // Initialize the loop with an empty suffix and start on node after prefix
        List.fold
            (fun acc char ->
                let unusedPieces = List.filter (fun c -> c <> char) hand
                loop (getNodeAfterPrefix dict prefix) unusedPieces char [char] acc
            ) Set.empty hand
