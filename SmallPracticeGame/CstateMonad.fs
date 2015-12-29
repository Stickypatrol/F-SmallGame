module CstateMonad

open System


type Cstate<'a, 's> = 's -> CstateStep<'a, 's>
and CstateStep<'a, 's> =
  | Done of 'a * 's
  | NotDone of Cstate<'a, 's> * 's

let ret (a:'a) : Cstate<'a, 's> = fun s -> Done(a, s)

let rec bind (p:Cstate<'a, 's>) (k: 'a -> Cstate<'b, 's>) : Cstate<'b, 's> =
  fun s ->
    match p s with
    | Done (a, s') -> k a s'
    | NotDone (p', s') -> NotDone(bind p' k, s')

type CstateBuilder() =
  member this.Return a = ret a
  member this.ReturnFrom (a:Cstate<'a, 's>) = a
  member this.Bind (p,k) = bind p k
  member this.Zero() : Cstate<Unit, 'a> = this.Return ()
let cs = CstateBuilder()

let rec (.||) (p1 : Cstate<'a, 's>) (p2 : Cstate<'b, 's>) : Cstate<Choice<'a, 'b>, 's> =
  fun s ->
    match p1 s, p2 s with
    | Done (a, s'), _ -> Done(Choice1Of2 a, s')
    | _, Done (b, s') -> Done(Choice2Of2 b, s')
    | NotDone(k1, s'), NotDone(k2, _) -> NotDone(k1 .|| k2, s')

let yield_ : Cstate<Unit, 's> =
  fun s -> NotDone((fun s -> Done((), s)), s)
                     //^ this is a Cstate with a predefined result

let emptyFunction : Cstate<Unit, 's> =
  fun gs ->
    Done((), gs)
 
(*let rec wait_for action interval =
  //wait for the curtime-starttime to be >= interval and then continue
  let gettime : Cstate<DateTime, 's> =
    fun s -> Done(DateTime.Now), s
  cs{
    let! t0 = gettime
    let rec wait s =
      cs{
        let! t = gettime
        let dt = (t-t0).TotalMilliseconds
        if dt < interval then
          do! yield_
          do! action dt
          return! wait s
      }
    do! wait()
  }

let rec run (c:Coroutine<'a>) : 'a  =
  match c () with
  | Return a -> a
  | Yield c' -> run c'
  | ArrowYield c' -> run c'

let yield_ =
  fun s -> Yield(fun s -> Return ())

let arrow_yield_ =
  fun s -> ArrowYield(fun s -> Return ())

let ignore_ s =
  co{
    let! _ = s
    return ()
  }
  
let rec (.||) (s1:Coroutine<'a>) (s2:Coroutine<'b>) : Coroutine<Choice<'a, 'b>> =
  fun s ->
    match s1 s, s2 s with
    | Return a, _ -> Return(Choice1Of2 a)
    | _, Return b -> Return(Choice2Of2 b)
    | ArrowYield k1, _ ->
      co{
        let! res = k1
        return Choice1Of2 res
      } |> Yield
    | _, ArrowYield k2 ->
      co{
        let! res = k2
        return Choice2Of2 res
      } |> Yield
    | Yield k1, Yield k2 -> (.||) k1 k2 |> Yield

let(.||>) s1 s2 = ignore_ (s1 .|| s2)

let rec (=>) (c:Coroutine<bool>) (s:Coroutine<'a>) : Coroutine<'a> =
  co{
    let! x = c
    if x then
      do! arrow_yield_
      let! res = s
      return res
    else
      do! yield_
      return! (=>) c s
  }
let rec repeat_ (s:Coroutine<Unit>) : Coroutine<Unit> =
  co{
    do! s
    return! repeat_ s
  }
let wait_doing (action:float -> Coroutine<Unit>) (interval:float) : Coroutine<Unit> =
  let time : Coroutine<DateTime> =
    fun _ -> Return(DateTime.Now)
  co{
    let! t0 = time
    let rec wait() =
      co{
        let! t = time
        let dt = (t-t0).TotalSeconds
        if dt < interval then
          do! yield_
          do! action dt
          return! wait()
      }
    do! wait()
  }

let wait = wait_doing(fun dt -> co{return ()})*)