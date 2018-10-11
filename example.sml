structure Example = struct
  open KrivineMachine

  val example1 =
    eval (TmApp (TmLam (Nat, TmVar 0), TmLam (Nat, TmVar 0)))
end
