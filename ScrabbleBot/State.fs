module internal State

    open ScrabbleUtil

    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Parser.board
        dict          : Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        lettersPlaced : Map<coord, char>
    }
    
    type move = (coord * (uint32 * (char * int))) list
    let mkState b d pn h lp = {board = b; dict = d;  playerNumber = pn; hand = h; lettersPlaced = lp}
    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    
    
    let add (pieces: (uint32 * uint32) list) (st: state): state =
        let updateHand = List.fold (fun acc (id, amount) -> MultiSet.add id amount acc) st.hand pieces
        mkState st.board st.dict st.playerNumber updateHand st.lettersPlaced
    let remove (ms: move) (st: state): state =
        let updateHand = List.fold (fun acc (_coord, (id, (_char, _val))) -> MultiSet.removeSingle id acc) st.hand ms
        mkState st.board st.dict st.playerNumber updateHand st.lettersPlaced
        
    let swap (pieces: uint32 list) (st: state): state =
        let updateHand = List.fold (fun acc id -> MultiSet.removeSingle id acc) st.hand pieces
        mkState st.board st.dict st.playerNumber updateHand st.lettersPlaced
        
    let updateBoard (ms: move) (st: state): state =
        let updateLettersPlaced = List.fold (fun acc (coord, (_id, (char, _val))) -> Map.add coord char acc) st.lettersPlaced ms
        mkState st.board st.dict st.playerNumber st.hand updateLettersPlaced
        
    let getHandIds st = (hand st) |> MultiSet.fold (fun acc x i -> (List.replicate (int i) x) @ acc) []
    let getHandChars st : char List =
        let ids = st |> getHandIds
        let offset = uint32 'A' - 1u
        let idToChar (id: uint32) : char = char (id + offset)
        List.map idToChar ids
        
    let idToChar (id: uint32) : char =
        if id = 0u then 'A'
        else char (id + (uint32 'A' - 1u))
    
    let idToValue pieces (id : uint32) : int = (Map.find id pieces) |> Set.minElement |> snd
        
    let charToLetter ((id, char) : uint32*char) (pieces : Map<uint, tile>): uint32 * (char * int) =
        id, (char, idToValue pieces id)
    
    let maxPiecesToChange st =
        let letterCount = Map.count st.lettersPlaced
        let handCount = MultiSet.size st.hand
        let amountOfPiecesLeft = 104 - int handCount - letterCount
        min 7 amountOfPiecesLeft
        
    let validStartPosition (x, y) (st : state) dir =
        let checkTile dx dy = Map.tryFind (x + dx, y + dy) (st.lettersPlaced) |> Option.isNone
        match dir with
        | false -> checkTile 0 -1 && checkTile 0 1 // lodret
        | true  -> checkTile -1 0 && checkTile 1 0 // vandret