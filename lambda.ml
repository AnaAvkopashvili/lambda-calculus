  type 'a lambda = Var of 'a | Fun of 'a * 'a lambda | App of 'a lambda * 'a lambda

  let rec free_variables t = match t with
  | Var x -> [x]
  | Fun (x, t) -> List.filter (fun e -> e <> x) (free_variables t)
  | App (t1, t2) -> (free_variables t1) @ (free_variables t2)

  let new_variable x =
    let rec helper x n =
      if List.mem (x ^ "_" ^ string_of_int n) (free_variables (Var x)) then helper x (n + 1)
      else (x ^ "_" ^ string_of_int n)
    in helper x 1
                                                              
let rec substitute x r t = match t with
  | Var e -> if x = e then r else Var e
  | Fun (e, t) ->
      if x = e then Fun (e, t)
      else if not (List.mem e (free_variables r)) then Fun (e, substitute x r t) 
      else Fun (new_variable e, substitute x r (substitute e (Var (new_variable e)) t))
  | App (t1, t2) -> App (substitute x r t1, substitute x r t2)
  

(*first version: *)
let rec beta_rule t = match t with 
  | Var x -> Var x
  | Fun (x, t) -> Fun(x, beta_rule t )
  | App (Fun (x, t), r) -> substitute x r t
  | App (x, y) -> App (beta_rule x, beta_rule y)


(* second version: if it has to be simplified completely*)
let rec apply_beta t  =
  let new_t = beta_rule t in
  if new_t = beta_rule new_t then new_t
  else apply_beta new_t



  




















(* type 'a lambda = Var of 'a | Fun of 'a * 'a lambda | App of 'a lambda * 'a lambda

let rec fv t =
  match t with
  | Var x -> [x]
  | Fun (x, l) -> List.filter (fun e -> e <> x) (fv l)
  | App (t1, t2) -> fv t1 @ fv t2

let alpha names nb t =
  let rec alpha bound t =
    match t with
    | Var s -> if List.mem s bound then Var (s ^ nb) else t
    | App (l1, l2) -> App (alpha bound l1, alpha bound l2)
    | Fun (s, l) ->
        if List.mem s names then Fun (s ^ nb, alpha (s :: bound) l)
        else Fun (s, alpha bound l)
  in
  alpha [] t

let rec ssubst body s arg =
  match body with
  | Var s0 -> if s = s0 then arg else Var s0
  | App (l1, l2) -> App (ssubst l1 s arg, ssubst l2 s arg)
  | Fun (o, l) -> if o = s then body else Fun (o, ssubst l s arg)

let gen, init_nb =
  let nb = ref 0 in
  (fun () -> incr nb; !nb), (fun () -> nb := 0)

let beta (App (Fun (s, body), arg)) =
  let nb = string_of_int (gen ()) in
  ssubst (alpha (fv arg) nb body) s arg

let rec cbn t =
  match t with
  | Var _ -> t
  | Fun _ -> t
  | App (e1, e2) ->
      let e1' = cbn e1 in
      match e1' with
      | Fun (x, t') -> cbn (beta (App (e1', e2)))
      | _ -> App (e1', e2)

let test1 = cbn (App (Fun ("x", App (Var "x", Var "x")), Var "y"))
let test2 = cbn (App (App (Fun ("x", Fun ("y", Var "x")), Var "y"), Var "z"))
let test3 = cbn (App (Fun ("x", App (Fun ("y", App (Var "x", Var "y")), Var "z")), Var "l"))
 *)