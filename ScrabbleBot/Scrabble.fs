namespace Hoved

open ScrabbleLib
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open ScrabbleUtil.DebugPrint
open System.IO


// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList
        
    // Function to print a single MoveCommand
    let printMoveCommand (moves : ((int * int) * (uint32 * (char * int))) list) =
        moves |> List.iteri (fun index ((x, y), (pieceId, (character, pointValue))) ->
        printfn "Move %d: (%d, %d) -> (%u, ('%c', %d))" (index + 1) x y pieceId character pointValue
    )



 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : ScrabbleLib.simpleBoardFun
        dict          : Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        lastLetter    : (int * int) * (uint32 * (char * int))
        direction     : bool // true = horizontal, false = vertical
    }

    let mkState b d pn h ll dir = {board = b; dict = d;  playerNumber = pn; hand = h; lastLetter = ll; direction = dir}

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    
    let getHandIds st = (hand st) |> MultiSet.fold (fun acc x i -> (List.replicate (int i) x) @ acc) []
    let getHandChars st : char List =
        let ids = st |> getHandIds
        let offset = uint32 'A' - 1u
        let idToChar (id: uint32) : char = char (id + offset)
        List.map idToChar ids
    
    let charToLetter (char : char) (pieces : Map<uint, tile>): (uint32 * (char * int)) =
        let offset = uint32 'A' - 1u
        let charToId (c : char) : uint32 = uint32 c - offset
        let charToValue (id : uint32) = (Map.find id pieces) |> Set.minElement |> snd
        charToId char, (char, charToValue (charToId char))


module Scrabble =
    open System.Threading

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)
            // remove the force print when you move on from manual input (or when you have learnt the format)
            // 0 0 5E1 0 1 1A1 0 2 20T1  -> EAT
            // 0 0 19S1 0 1 5E1 0 2 1A1  -> SEA
            // 0 0 6F4 0 1 9I1 0 2 14N1  -> FIN (bemærk [('F',4)] angiver point value)
                    // 1 2 15O1 2 2 20T1 -> OT efter N ovenfor (dvs NOT vandret)
            forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"

            let prefix = st.lastLetter |> snd |> (fun (id, (char, value)) -> string char)
            let handAsCharList = State.getHandChars st
            let possibleSuffixes = MakeWord.findPossibleSuffixes (State.dict st) handAsCharList prefix
            
            debugPrint (sprintf "Possible suffixes: %A" possibleSuffixes)
            if Set.isEmpty possibleSuffixes then
                send cstream (SMChange ((State.hand st) |> MultiSet.fold (fun acc x _ -> x :: acc) []))
                let msg = recv cstream
                match msg with
                | RCM (CMChangeSuccess(newTiles)) ->
                    (forcePrint "RCMChangeSuccess**")
                    let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty newTiles
                    let st' = State.mkState st.board st.dict st.playerNumber handSet st.lastLetter st.direction
                    aux st'
                | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st
            
            let suffix = Seq.head possibleSuffixes            
            let moveStartPos = st.lastLetter |> fst
            let addPos (x, y) index = if st.direction then (x + index, y) else (x, y + index)
            let move = List.fold (fun acc char -> acc@[(addPos moveStartPos (acc.Length + 1)),(State.charToLetter char pieces)]) [] suffix
            
            RegEx.printMoveCommand move

            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                forcePrint "RCMPlaySuccess**"
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let leftoverHand = List.fold (fun acc (_,(id, _)) -> MultiSet.remove id 1u acc) st.hand move
                let newHand = List.fold (fun acc (id, amount) -> MultiSet.add id amount acc) leftoverHand newPieces
                let lastLetterPlaced = List.last move
                let st' = State.mkState st.board st.dict st.playerNumber newHand lastLetterPlaced (not st.direction)
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                forcePrint "RCMPlayed**"
                (* Successful play by other player. Update your state *)
                let st' = st // This state needs to be update
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                forcePrint "RCMPlaySuccess**"
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMGameOver _) -> (forcePrint "CMGameOver**")
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st


        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            $"Starting game!
                      number of players = %d{numPlayers}
                      player id = %d{playerNumber}
                      player turn = %d{playerTurn}
                      hand =  %A{hand}
                      timeout = %A{timeout}\n\n"

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = simpleBoardLangParser.parseSimpleBoardProg boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand
        let startingLetter = ((-1, 0), (0u, (' ', 0))) // Only the position is important, the rest is irrelevant

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet startingLetter true)
        