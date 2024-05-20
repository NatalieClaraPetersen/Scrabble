module internal State

    open ScrabbleUtil

    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Parser.board
        dict          : Dictionary.Dict
        hand          : MultiSet.MultiSet<uint32>
        lettersPlaced : Map<coord, char>
    }

    type move = (coord * (uint32 * (char * int))) list
    type direction = Right | Down

    let mkState b d h lp = { board = b; dict = d; hand = h; lettersPlaced = lp }

    let board st         = st.board
    let dict st          = st.dict
    let hand st          = st.hand
    let lettersPlaced st = st.lettersPlaced
    
    let add (pieces: (uint32 * uint32) list) (st: state): state =
        let updateHand = List.fold (fun acc (id, amount) -> MultiSet.add id amount acc) st.hand pieces
        mkState st.board st.dict updateHand st.lettersPlaced
    let remove (ms: move) (st: state): state =
        let updateHand = List.fold (fun acc (_coord, (id, (_char, _val))) -> MultiSet.removeSingle id acc) st.hand ms
        mkState st.board st.dict updateHand st.lettersPlaced
        
    let change (pieces: uint32 list) (st: state) (newTiles: (uint32 * uint32) list): state =
        let removeFromHand = List.fold (fun acc id -> MultiSet.removeSingle id acc) st.hand pieces
        let addToHand = List.fold (fun acc (x, k) -> MultiSet.add x k acc) removeFromHand newTiles
        mkState st.board st.dict addToHand st.lettersPlaced
        
    let updateBoard (ms: move) (st: state): state =
        let updateLettersPlaced = List.fold (fun acc (coord, (_id, (char, _val))) -> Map.add coord char acc) st.lettersPlaced ms
        mkState st.board st.dict st.hand updateLettersPlaced