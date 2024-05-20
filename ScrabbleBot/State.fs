module internal State

    open ScrabbleUtil
    open ScrabbleUtil.DebugPrint

    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player number, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        center        : coord
        dict          : Dictionary.Dict
        hand          : MultiSet.MultiSet<uint32>
        lettersPlaced : Map<coord, char>
        tilesLeft     : int32
    }

    type move = (coord * (uint32 * (char * int))) list
    type direction = Right | Down

    let mkState b d h lp tl = { center = b; dict = d; hand = h; lettersPlaced = lp ; tilesLeft = tl }
    let updateHand st hand = { st with hand = hand }

    let board st         = st.center
    let dict st          = st.dict
    let hand st          = st.hand
    let lettersPlaced st = st.lettersPlaced
    
    let add (pieces: (uint32 * uint32) list) (st: state): state =
        let updatedHand = List.fold (fun acc (id, amount) -> MultiSet.add id amount acc) st.hand pieces
        updateHand st updatedHand
    let remove (ms: move) (st: state): state =
        let updatedHand = List.fold (fun acc (_coord, (id, (_char, _val))) -> MultiSet.removeSingle id acc) st.hand ms
        updateHand st updatedHand

    let change (pieces: uint32 list) (st: state) (newTiles: (uint32 * uint32) list): state =
        let removeFromHand = List.fold (fun acc id -> MultiSet.removeSingle id acc) st.hand pieces
        let addToHand = List.fold (fun acc (x, k) -> MultiSet.add x k acc) removeFromHand newTiles
        updateHand st addToHand
        
    let updateBoard (move: move) (st: state): state =
        let updateLettersPlaced = List.fold (fun acc (coord, (_id, (char, _val))) -> Map.add coord char acc) st.lettersPlaced move
        let updatedTilesLeft = max 0 (st.tilesLeft - List.length move)
        debugPrint $"Tiles left: {updatedTilesLeft}\n"
        { st with lettersPlaced = updateLettersPlaced ; tilesLeft = updatedTilesLeft }
    
    