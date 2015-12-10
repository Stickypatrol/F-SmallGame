module StateMonad

type State<'a, 's> = 's -> 'a * 's

type GameState<'a> =
  {
    Boxes : List<'a>
  }

let ret a = fun s -> a, s

let bind (p:State<'a, 's>) (k:'a -> State<'b, 's>) : State<'b, 's> =
  fun s ->
    let a, s' = p s
    k a s'

let (>>=) = bind

type StateBuilder() =
  member this.Return(a) = ret a
  member this.ReturnFrom(s) = s
  member this.Bind(p,k) = p >>= k
let state = StateBuilder()