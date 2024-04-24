module TreeDict
    
    type TreeDict =
        | Leaf of bool
        | Dict of bool * Map<char, TreeDict>

    let empty () = Leaf false

    let rec lookup (word: string) =
        function
        // we reach a leaf and the word is empty
        | Leaf _ when word.Length = 0 -> true
        // we reach a leaf and the word is not empty
        | Leaf _ -> false

        // When word end is reached we print the bool value, true: is a word, false: not a word
        | Dict (isTerminal, _) when word.Length = 0 -> isTerminal

        | Dict (isTerminal, children) ->
            match Map.tryFind word.[0] children with
            | Some child -> lookup (word.Substring(1)) child
            // word is not empty but there is no more chars to lookup
            | None -> false
    
    let rec insert (word: string) = 
        function
        | Leaf _ when word.Length = 0 -> Leaf true
        | Dict (_, children) when word.Length = 0 -> Dict (true, children)
        | Leaf isTerminal ->
            let newDict = Dict(isTerminal, Map.empty)
            insert word newDict

        | Dict (isTerminal, children) ->
            let char = word.[0]
            match Map.tryFind char children with
            | Some child ->
                let newMap = Map.add char (insert (word.Substring(1)) child) children
                Dict(isTerminal, newMap)
            | None ->
                let newMap = Map.add char (insert (word.Substring(1)) (empty())) children
                Dict(isTerminal, newMap)

    let step (char: char) =
        function
        | Leaf _ -> None
        | Dict (_, children) ->
            match Map.tryFind char children with
            | Some child -> 
                match child with
                | Leaf isTerminal -> 
                    Some (isTerminal, child)
                | Dict (isTerminal, _) ->
                    Some (isTerminal, child)
            | None -> None