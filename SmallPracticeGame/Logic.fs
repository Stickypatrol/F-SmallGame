module Logic

open StateMonad
open Utility
open Actors

let Positions : State<(Vector2<int>*Vector2<int>) list, GameState<Box>> =
  fun gs ->
    let gs' = {gs with Boxes = [for x in gs.Boxes -> x.Move]}
    let LocandSize =  [for x in gs'.Boxes ->
                          {X = int <| x.Pos.X;Y = int <| x.Pos.Y},
                          {X = int <| x.Size.X;Y = int <| x.Size.Y}
                      ]
    (LocandSize,gs')

let makeSprites dimensions :State<Map<(int*int), string> , GameState<Box>> =
  fun gs ->
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
    let LocandSize' = createList dimensions []
    let sprites = Map.ofList LocandSize'
    (sprites, gs)
