open Utility
open Actors
open Logic
open Input
open CstateMonad
open Microsoft.Xna.Framework.Input

type Sprites = Map<(int*int), string>

let UpdateGS : Cstate<Unit, GameState<Box>> =
  cs{
    let! _ = HandleInput (Keyboard.GetState().GetPressedKeys())
    let! locations' = Positions
    let! sprites = makeSprites locations'
    do! Draw sprites
    return ()
  }

let player1Controls = [ Keys.W, (fun (box:Box) -> Box.Move box -1.0 0.0);
                        Keys.S, (fun (box:Box) -> Box.Move box 1.0 0.0);
                        Keys.A, (fun (box:Box) -> Box.Move box 0.0 -1.0);
                        Keys.D, (fun (box:Box) -> Box.Move box 0.0 1.0);
                        ] |> Map.ofList

let player2Controls = [ Keys.Up, (fun (box:Box) -> Box.Move box -1.0 0.0);
                        Keys.Down, (fun (box:Box) -> Box.Move box 1.0 0.0);
                        Keys.Left, (fun (box:Box) -> Box.Move box 0.0 -1.0);
                        Keys.Right, (fun (box:Box) -> Box.Move box 0.0 1.0);
                        ] |> Map.ofList

let beginstate =  {
                    Players = [(createPlayer {X = 1.0;Y = 1.0} player1Controls)]
                  }
let rec GameLoop (prevGameState : GameState<Box>) =
  let tentativeGameState =
    match UpdateGS prevGameState with
    | Done ((), gs) -> gs
    | _ -> failwith "ARGH, error alert!"
  System.Threading.Thread.Sleep(20)
  GameLoop tentativeGameState
  

do GameLoop beginstate