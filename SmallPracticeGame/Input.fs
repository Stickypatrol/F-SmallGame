module Input

open Actors
open CstateMonad
open System
open System.IO
open Microsoft.Xna.Framework.Input

//handling the input

let HandleInput (kbs:Keys[]) : Cstate<Unit, GameState<Box>> =
  fun (gs:GameState<Box>) ->
    if kbs.Length > 0 then
      Console.Beep()
    let gs' = {gs with Players =
                        (List.fold (fun boxlist' box ->
                          (Array.fold (fun box arrentry ->
                            match (box.IB.TryFind arrentry) with
                            | Some reaction ->  Console.Beep()
                                                reaction box
                            | None -> box) box kbs)::boxlist')
                        List<Box>.Empty) gs.Players
              }
    Done((), gs')
