module Utility

type Vector2<'a> = {X : 'a;Y : 'a}
  with static member (+) (a, b) = {X = a.X + b.X; Y = a.Y + b.Y}

let Draw (sprites: Map<int*int, string>) =
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
    ((), gs)