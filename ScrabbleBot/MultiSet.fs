// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet
    type MultiSet<'a when 'a : comparison> = R of Map<'a, uint32>

    let empty = R Map.empty

    let isEmpty (R ms: MultiSet<'a>) = ms.IsEmpty
    
    let size (R ms : MultiSet<'a>) = Map.fold (fun acc _ value -> acc + value) 0u ms
    
    let contains (elem : 'a) (R ms : MultiSet<'a>) = ms.ContainsKey(elem)

    let numItems (elem : 'a) (R ms : MultiSet<'a>) =
        match Map.tryFind elem ms with
        | Some count -> count
        | None -> 0u

    let add (elem: 'a) (amount : uint32) (R ms : MultiSet<'a>) =
        let currentVal = numItems elem (R ms)
        R (Map.add elem (currentVal + amount) ms)
        
    let addSingle (elem: 'a) (ms : MultiSet<'a>) = add elem 1u ms
        
    let remove (elem: 'a) (amount: uint32) (R ms: MultiSet<'a>) =
        match numItems elem (R ms) with
        | 0u -> R ms
        | x when amount >= x -> R (Map.remove elem ms)
        | x -> R (Map.add elem (x - amount) ms)

    let removeSingle (elem: 'a) (ms: MultiSet<'a>) = remove elem 1u ms

    let fold (folder: 'b -> 'a -> uint32 -> 'b) (acc: 'b) (R ms: MultiSet<'a>) = Map.fold folder acc ms
    let foldBack (folder: 'a -> uint32 -> 'b -> 'b) (R ms: MultiSet<'a>) (acc: 'b) = Map.foldBack folder ms acc
    
    let ofList (_ : 'a list) : MultiSet<'a> = failwith "not implemented"
    let toList (R ms : MultiSet<'a>) : 'a list = Map.fold (fun acc elem count -> acc @ List.init (int count) (fun _ -> elem)) [] ms


    let map (_ : 'a -> 'b) (_ : MultiSet<'a>) : MultiSet<'b> = failwith "not implemented"

    let union (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = failwith "not implemented"
    let sum (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = failwith "not implemented"
    
    let subtract (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = failwith "not implemented"
    
    let intersection (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = failwith "not implemented"
