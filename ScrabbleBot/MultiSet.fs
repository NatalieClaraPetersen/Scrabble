// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet
    
    // ORIGINAL FILE CONTENT:
    //type MultiSet<'a> = Temp of unit // Not implemented
    //let empty : MultiSet<'a> = Temp () // Not implemented
    //let add   : 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a> = fun _ _ _ -> failwith "Not implemented"
    //let fold  : ('b -> 'a -> uint32 -> 'b) -> 'b -> MultiSet<'a> -> 'b = fun _ _ _ -> failwith "Not implemented"

    type MultiSet<'a when 'a : comparison> = R of Map<'a, uint32>

    let empty = R Map.empty

    let isEmpty (R set: MultiSet<'a>) = set.IsEmpty
    
    let size (R set : MultiSet<'a>) = Map.fold(fun acc key value -> acc + value) 0u set
    
    let contains (elem : 'a) (R set : MultiSet<'a>) = set.ContainsKey(elem)

    let numItems (elem : 'a) (R set : MultiSet<'a>) =
        match Map.tryFind elem set with
        | Some count -> count
        | None -> 0u

    let add (elem: 'a) (num : uint32) (R set : MultiSet<'a>) : MultiSet<'a> =
        match Map.tryFind elem set with
        | Some currentVal -> R (Map.add elem (currentVal + num) set)
        | None -> R (Map.add elem (num) set)

    let addSingle (elem: 'a) (R set: MultiSet<'a>) : MultiSet<'a> =
        match Map.tryFind elem set with
        | Some currentVal -> R (Map.add elem (currentVal + 1u) set)
        | None -> R (Map.add elem 1u set)
    
    let remove (elem: 'a) (num: uint32) (R set: MultiSet<'a>) : MultiSet<'a> =
        match Map.tryFind elem set with
        | Some currentVal when currentVal > num -> R (Map.add elem (currentVal - num) set)
        | Some _ -> R (Map.remove elem set)
        | None -> R(set)

    let removeSingle (elem: 'a) (set: MultiSet<'a>) : MultiSet<'a> = remove elem 1u set

    let fold (f: 'b -> 'a -> uint32 -> 'b) (acc: 'b) (R set: MultiSet<'a>) = Map.fold f acc set
    let foldBack (f: 'a -> uint32 -> 'b -> 'b) (R set: MultiSet<'a>) (acc: 'b) = Map.foldBack f set acc
    
    let ofList (_ : 'a list) : MultiSet<'a> = failwith "not implemented"
    let toList (_ : MultiSet<'a>) : 'a list = []


    let map (_ : 'a -> 'b) (_ : MultiSet<'a>) : MultiSet<'b> = failwith "not implemented"

    let union (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = failwith "not implemented"
    let sum (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = failwith "not implemented"
    
    let subtract (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = failwith "not implemented"
    
    let intersection (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = failwith "not implemented"
