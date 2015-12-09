module Actors

open Utility

type Box =
  {
    Size  : Vector2<float>
    Pos   : Vector2<float>
    Vel   : Vector2<float>
  }
    with member self.Move =
                            let px', vx' = 
                                match (self.Pos.X, self.Size.X, self.Vel.X) with
                                | px, sx, vx when px + vx > 78.0-sx -> 78.0-sx, -vx
                                | px, sx, vx when px + vx < 1.0 -> 1.0, -vx
                                | px, sx, vx -> px + vx, vx
                            let py', vy' = 
                                match (self.Pos.Y, self.Size.Y, self.Vel.Y) with
                                | py, sy, vy when py + vy > 23.0-sy -> 23.0-sy, -vy
                                | py, sy, vy when py + vy < 1.0 -> 1.0, -vy
                                | py, sy, vy -> py + vy, vy
                            {self with Pos = {X = px';Y = py'}; Vel = {X = vx'; Y = vy'}}

let createBox (p:Vector2<float>) =  {
                                      Size = {X = 2.0;Y = 2.0};
                                      Pos = {X = p.X; Y = p.Y};
                                      Vel = {X = 2.0; Y = 1.0}
                                    }