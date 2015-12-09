open System.Threading
open System
open StateMonad
open Utility
open Actors

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

let MainUpdate() : State<Unit, GameState<Box>> =
  state{
    let! sprites = UpdateGS
    do! Draw sprites
    return ()
  }
let beginstate =  { Boxes =
                    [(createBox {X = 1.0;Y = 1.0});
                  (*(createBox {X = 50.0;Y = 20.0});
                  (createBox {X = 20.0;Y = 5.0});
                  (createBox {X = 5.0;Y = 5.0});
                  (createBox {X = 20.0;Y = 20.0}) *)
                    ]
                  }
let rec GameLoop prevGameState =
  let tentativeGameState = snd(MainUpdate() prevGameState)
  Thread.Sleep(100)
  GameLoop tentativeGameState

do GameLoop beginstate