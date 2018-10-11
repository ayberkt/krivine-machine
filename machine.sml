structure KrivineMachine = struct
  structure L = List

  datatype ty = Nat | ==> of ty * ty

  datatype term =
      TmVar of int
    | TmApp of term * term
    | TmLam of ty * term

  datatype clos =
      ClTm of term * env
    | ClApp of clos * clos
  and
    env = Env of clos L.list

  datatype value = VLam of ty * term * env

  val isVal =
    fn
      ClTm (TmLam (_, _), _) => true
    | _ => false

  fun evalStep (tm : clos) : clos =
    case tm of
      ClTm (TmVar 0, Env (c :: rho)) => c
    | ClTm (TmVar n, Env (c :: rho)) => evalStep (ClTm (TmVar (n-1), Env rho))
    | ClApp (ClTm (TmLam (ty, tm), Env rho), cl1) => ClTm (tm, Env (cl1 :: rho))
    | ClApp (c0, c1) => ClApp (evalStep c0, c1)
    | ClTm (TmApp (tm0, tm1), env) => ClApp (ClTm (tm0, env), ClTm (tm1, env))

  fun evalRec tm =
    if isVal tm then
      tm
    else
      evalRec (evalStep tm)

  val eval =
      fn tm =>
        case evalRec (ClTm (tm, Env [])) of
          ClTm (tm, _) => tm

end
