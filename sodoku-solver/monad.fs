namespace sodokusolver
module monad =
    let addThreeNumbersImperitive() =
        let getNum msg =
          printf "%s" msg
          match System.Int32.TryParse(System.Console.ReadLine()) with
            | (true, n) when n >= 0 && n <= 100 -> Some(n)
            | _ -> None
        match getNum "#1: " with
          | Some(x) ->
            match getNum "#2: " with
              | Some(y) ->
                match getNum "#3: " with
                  | Some(z) -> Some(x + y + z)
                  | None -> None
              | None -> None
          | None -> None
    let addThreeNumbersBind() =
      let bind(input, rest) =
        match System.Int32.TryParse(input()) with
          | (true, n) when n >= 0 && n <= 100 -> rest(n)
          | _ -> None
      let createMsg (msg) = fun () -> printf "%s" msg; System.Console.ReadLine()
      bind(createMsg "#1: ", fun x ->
           bind(createMsg "#2: ", fun y ->
                bind(createMsg "#3: ", fun z -> Some(x + y + z) ) ) )

    type MaybeBuilder() =
      member this.Bind(x, f) =
        match x with
          | Some(x) when x >= 0 && x <= 100 -> f(x)
          | _ -> None
      member this.Delay(f) = f()
      member this.Return(x) = Some x

    let addThreeNumbersMaybeBuilder() =
      let maybe = MaybeBuilder ()
      maybe.Delay (fun () ->
        let x = 10
        maybe.Bind (Some 11, fun y ->
            maybe.Bind (Some 12, fun z ->
                maybe.Return (x + y + z))))
