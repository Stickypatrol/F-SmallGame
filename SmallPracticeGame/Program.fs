open System.Threading
open System
open StateMonad
open Utility
open Actors
open Logic

type Sprites = Map<(int*int), string>

let UpdateGS : State<Unit, GameState<Box>> =
  state{
    let! locations = Positions
    let! sprites = makeSprites locations
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
  let tentativeGameState = snd(UpdateGS prevGameState)
  Thread.Sleep(100)
  GameLoop tentativeGameState

do GameLoop beginstate