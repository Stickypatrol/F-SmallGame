module Actors

open Utility
open Microsoft.Xna.Framework.Input

type InputBehavior<'a> = Map<Keys, ('a -> 'a)>


type Box =
  {
    Size  : Vector2<float>
    Pos   : Vector2<float>
    Vel   : Vector2<float>
    IB    : InputBehavior<Box>
  }
    with  member self.Translate =
                            let px', vx' = 
                                match (self.Pos.X, self.Size.X, self.Vel.X) with
                                | px, sx, vx when px + vx > 78.0-sx -> 78.0-sx, vx * -0.6
                                | px, sx, vx when px + vx < 1.0 -> 1.0, vx * -0.6
                                | px, sx, vx -> px + vx, vx
                            let py', vy' = 
                                match (self.Pos.Y, self.Size.Y, self.Vel.Y) with
                                | py, sy, vy when py + vy > 23.0-sy -> 23.0-sy, vy * -0.6
                                | py, sy, vy when py + vy < 1.0 -> 1.0, vy * -0.6
                                | py, sy, vy -> py + vy, vy
                            {self with Pos = {X = px';Y = py'}; Vel = {X = vx'; Y = vy'}}
          static member Move (box:Box) x' y' =
                        {box with
                          Vel = {X = box.Vel.X + x'; Y = box.Vel.Y + y'}
                        }

let createPlayer (p:Vector2<float>) ib = {
                                      Size = {X = 2.0;Y = 2.0};
                                      Pos = {X = p.X; Y = p.Y};
                                      Vel = {X = 2.0; Y = 1.0};
                                      IB = ib
                                    }

type GameState<'a> =
  {
    Players : List<'a> //dalijk playernumber er uit halen want is niet nodig
  }
