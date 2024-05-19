namespace Hoved

open System.Threading
open ScrabbleLib
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open ScrabbleUtil.DebugPrint
open System.IO
open StateMonad


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
        debugPrint $"Move %d{index + 1}: (%d{x}, %d{y}) -> (%u{pieceId}, ('%c{character}', %d{pointValue}))"
    )



 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> debugPrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : simpleBoardFun
        dict          : Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        lettersPlaced : List<(int * int) * (uint32 * (char * int))>
        direction     : bool // true = horizontal, false = vertical
    }
    let mkState b d pn h lp dir = {board = b; dict = d;  playerNumber = pn; hand = h; lettersPlaced = lp; direction = dir}
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
        
    let idToChar (id: uint32) : char =
        if id = 0u then 'A'
        else char (id + (uint32 'A' - 1u))
    
    let idToValue pieces (id : uint32) : int = (Map.find id pieces) |> Set.minElement |> snd
        
    let charToLetter ((id, char) : uint32*char) (pieces : Map<uint, tile>): uint32 * (char * int) =
        id, (char, idToValue pieces id)
        
    let validStartPosition (x, y) (st : state) dir =
        let checkTile dx dy = Map.tryFind (x + dx, y + dy) (Map.ofList st.lettersPlaced) |> Option.isNone
        match dir with
        | false -> checkTile 0 (-1) && checkTile 0 1 // lodret
        | true  -> checkTile (-1) 0 && checkTile 1 0 // vandret


module Scrabble =

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)
            let checkSuffixes (acc: List<uint32 * char> * (int * int)) letter =
                let moveStartPos = fst letter
                let validstart =  State.validStartPosition moveStartPos st st.direction
                if not validstart then acc else
                let start = (List.last st.lettersPlaced |> fst) = (-1,0)
                let prefix = snd letter |> (fun (_id, (char, _value)) -> char)
                let prefixNode = MakeWord.getNodeAfterPrefix (State.dict st) prefix
                let handMultiset = MultiSet.fold (fun acc id amount -> MultiSet.add (id, (State.idToChar id)) amount acc) MultiSet.empty (State.hand st)
                let possibleSuffixes = MakeWord.findPossibleSuffixes prefixNode handMultiset moveStartPos st.lettersPlaced st.direction start
            
                debugPrint (sprintf "Possible suffixes: %A (%A) %b\n" possibleSuffixes moveStartPos validstart)
                // Determine the longest suffix in the possible suffixes
                let longestSuffix = 
                    if Set.isEmpty possibleSuffixes then 
                        []
                    else
                        possibleSuffixes |> Set.toList |> List.maxBy List.length 

                debugPrint (sprintf "Longest suffix: %A\n" longestSuffix)

                // Update the accumulator if the current longest suffix is longer than the previous one
                if List.length longestSuffix > List.length (fst acc) then
                    (longestSuffix, moveStartPos)
                else
                    acc
            
            // Fold over the lettersPlaced to accumulate possible suffixes
            let initialAcc = ([], (0,0))
            let (longestSuffix, longestMoveStartPos) = List.fold checkSuffixes initialAcc st.lettersPlaced
            
            debugPrint (sprintf "Final suffix: %A (%A)\n" longestSuffix longestMoveStartPos)
            if List.isEmpty longestSuffix then
                let rec changeHand handIdList =
                    send cstream (SMChange handIdList)
                    let msg = recv cstream
                    match msg with
                    | RCM (CMChangeSuccess(newTiles)) ->
                        (debugPrint "RCMChangeSuccess**\n")
                        let leftoverHand = List.fold (fun acc id -> MultiSet.remove id 1u acc) st.hand handIdList
                        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) leftoverHand newTiles
                        let st' = State.mkState st.board st.dict st.playerNumber handSet st.lettersPlaced st.direction
                        aux st'
                    | RCM (CMGameOver _) -> (debugPrint "CMGameOver: **Three CMChangeSuccess in a row**\n");
                    | RGPE _err ->
                        //printfn "Gameplay Error**%A\n" err
                        (debugPrint "Gameplay Error: **Not enough pieces**\n")
                        changeHand handIdList.Tail
                changeHand (State.getHandIds st)
            else         
            let addPos (x, y) index = if st.direction then (x + index, y) else (x, y + index)
            let move = List.fold (fun acc char -> acc@[(addPos longestMoveStartPos (acc.Length + 1)),(State.charToLetter char pieces)]) [] longestSuffix
            
            //RegEx.printMoveCommand move

            //debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            Thread.Sleep(1000)
            //debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(_ms, _points, newPieces)) ->
                debugPrint "RCMPlaySuccess**\n"
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let leftoverHand = List.fold (fun acc (_,(id, _)) -> MultiSet.removeSingle id acc) st.hand move
                let newHand = List.fold (fun acc (id, amount) -> MultiSet.add id amount acc) leftoverHand newPieces
                let updatedLetterPlaced =
                    if (List.last st.lettersPlaced |> fst) = (-1,0) then
                        move
                    else
                        List.append st.lettersPlaced move
        
                let st' = State.mkState st.board st.dict st.playerNumber newHand updatedLetterPlaced (not st.direction)
                aux st'
            | RCM (CMPlayed (_pid, _ms, _points)) ->
                debugPrint "RCMPlayed**\n"
                (* Successful play by other player. Update your state *)
                let st' = st // This state needs to be update
                aux st'
            | RCM (CMPlayFailed (_pid, _ms)) ->
                debugPrint "RCMPlayFailed**\n"
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> debugPrint $"Gameplay Error:\n%A{err}"; aux st


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
        let initialPlacedLetters = [startingLetter]
        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet initialPlacedLetters true)
        