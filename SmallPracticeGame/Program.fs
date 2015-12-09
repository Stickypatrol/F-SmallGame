open System.Threading
open System
open StateMonad
open Utility
open Actors


let mutable gamestate = { Boxes =
                          [(createBox {X = 1.0;Y = 1.0});
                        (*(createBox {X = 50.0;Y = 20.0});
                        (createBox {X = 20.0;Y = 5.0});
                        (createBox {X = 5.0;Y = 5.0});
                        (createBox {X = 20.0;Y = 20.0}) *)
                          ]
                        }


type Sprites = Map<(int*int), string>

let UpdateGS : State<Sprites, GameState<Box>> =
  fun gs ->
    let gs' = {gs with Boxes = [for x in gs.Boxes -> x.Move]}
    let LocandSize = [for x in gs'.Boxes ->
                          {X = int <| x.Pos.X;Y = int <| x.Pos.Y},
                          {X = int <| x.Size.X;Y = int <| x.Size.Y}
                    ]
    let looper loc size =
      [
        for i = loc.X to loc.X + size.X do
          for p = loc.Y to loc.Y + size.Y do
            yield ((i, p), "X")
      ]
    let rec createList lslist xs =
      match lslist with
      | [] -> xs
      | h::t -> let loc, size = fst(h), snd(h)
                let ys = looper loc size
                let xs' = ys @ xs
                createList t xs'
    let LocandSize' = createList LocandSize []
    let sprites = Map.ofList LocandSize'
    (sprites, gs')

let MainUpdate : State<Unit, GameState<Box>> =
  state{
    let! sprites = UpdateGS
    do! Draw sprites
    return ()
  }
while true do
  //in here we perform update with the gs -> (gs:'s, spritebatch:'a) pair
  //we then use the spritebatch contents to run draw and make draw a (gs, ()) pair
  //we then make the simulation pause with thread.sleep and run it again
  //we store the GS in a mutable variable state
  Thread.Sleep(100)
  Console.Clear()
  gamestate <- snd(MainUpdate gamestate)
  