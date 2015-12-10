module Utility

type Vector2<'a> = {X : 'a;Y : 'a}
  with static member (+) (a, b) = {X = a.X + b.X; Y = a.Y + b.Y}