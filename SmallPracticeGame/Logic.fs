module Logic

open Utility
open Actors
open CstateMonad
open Microsoft.Xna.Framework.Input


let Positions : Cstate<(Vector2<int>*Vector2<int>) list, GameState<Box>> =
  fun gs ->
    let gs' = {gs with Players = [for box in gs.Players -> box.Translate]}
    let LocandSize =  [for box in gs'.Players ->
                          {X = int <| box.Pos.X;Y = int <| box.Pos.Y},
                          {X = int <| box.Size.X;Y = int <| box.Size.Y}
                      ]
    Done(LocandSize,gs')

let makeSprites dimensions :Cstate<Map<(int*int), string> , GameState<Box>> =
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
    Done(sprites, gs)

let Draw (sprites: Map<int*int, string>) : Cstate<Unit, GameState<Box>>=
  fun gs ->
    System.Console.Clear()
    let mutable buffer = ""
    for x = 0 to 24 do
      for y = 0 to 79 do
        if x = 24 && y = 79 then
          buffer <- buffer
        else if x = 0 || x = 24 || y = 0 || y = 79 then
          buffer <- buffer + "X"
        else
          match sprites.TryFind (y, x) with
          | Some x -> buffer <- buffer + x
          | None -> buffer <- buffer + " "
    printf "%s" buffer
    Done((), gs)