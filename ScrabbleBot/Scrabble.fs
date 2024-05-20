namespace Hoved

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


module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> debugPrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()
module Scrabble =

    let playGame cstream pieces (st : State.state) =
        let rec aux (st : State.state) =
            let move = MakeWord.getMove st pieces
            
            if move.IsEmpty then
                if MakeWord.canSwap st then
                    debugPrint "No moves found. Swapping..."
                    send cstream (SMChange (MakeWord.handToList st))
                else
                    debugPrint "No moves found. Passing."
                    send cstream SMPass
            else
                send cstream (SMPlay move)
                
            //debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            
            let msg = recv cstream
            
           // debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            
            match msg with
            | RCM (CMChangeSuccess(newTiles)) ->
                let st' =
                    State.swap(MakeWord.handToList st) st |> State.add newTiles
                aux st'
                
            | RCM (CMGameOver _) ->
                let handCount = MultiSet.size st.hand |> int
                (debugPrint $"CMGameOver: **Three CMChangeSuccess in a row** | hand: {handCount}\n")
                
            | RGPE err -> debugPrint $"Gameplay Error:\n%A{err}"; aux st
            
            | RCM (CMPlaySuccess(ms, _points, newPieces)) ->
                debugPrint "RCMPlaySuccess**\n"
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' =
                    State.remove ms st
                    |> State.add newPieces
                    |> State.updateBoard ms
                aux st'
                
            | RCM (CMPlayed (_pid, ms, _points)) ->
                let st' = State.updateBoard ms st
                aux st'

            | RCM (CMPlayFailed (_pid, _ms)) ->
                debugPrint "RCMPlayFailed**\n"
                (* Failed play. Update your state *)
                let st' = st
                aux st'

            | RCM a -> failwith (sprintf "not implmented: %A" a)


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
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand
        let initialPlacedLetters = Map.empty
        
        fun () -> playGame cstream tiles (State.mkState board dict handSet initialPlacedLetters)
        