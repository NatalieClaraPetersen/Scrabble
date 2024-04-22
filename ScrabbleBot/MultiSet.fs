// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet
    
    // ORIGINAL FILE CONTENT:
    //type MultiSet<'a> = Temp of unit // Not implemented
    //let empty : MultiSet<'a> = Temp () // Not implemented
    //let add   : 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a> = fun _ _ _ -> failwith "Not implemented"
    //let fold  : ('b -> 'a -> uint32 -> 'b) -> 'b -> MultiSet<'a> -> 'b = fun _ _ _ -> failwith "Not implemented"

    type MultiSet<'a when 'a : comparison> = { items: Map<'a, uint32> }
    
    let empty : MultiSet<'a> = { items = Map.empty }
    
    let fold (f : 'b -> 'a -> uint32 -> 'b) (acc : 'b) (m : MultiSet<'a>) = 
        Map.fold (fun acc key value -> f acc key value) acc m.items
        
    let numItems (key : 'a) (m : MultiSet<'a>) = 
        match Map.tryFind key m.items with
        | Some v -> v
        | None -> 0u

    let add (key : 'a) (amount : uint32) (m : MultiSet<'a>) : MultiSet<'a> =
        let count = numItems key m
        { items = Map.add key (count + amount) m.items }
    
    let remove (key : 'a) (amount : uint32) (m  : MultiSet<'a>) : MultiSet<'a> = 
        let count = numItems key m
        if count <= amount then  
            { items = Map.remove key m.items }
        else 
            { items = Map.add key (count - amount) m.items }
