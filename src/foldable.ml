module Make_list (M : Intf.Core) :
  Intf.Foldable with type 'a t = 'a list and type 'a m := 'a M.t = struct
  type 'a t = 'a list

  let fold f acc m =
    let open M.Infix in
    List.fold_left
      (fun acc elt ->
        let* acc = acc in
        f acc elt)
      (return acc)
      m

  let iter f m =
    let open M.Infix in
    List.fold_left
      (fun acc elt ->
        let* () = acc in
        f elt)
      (return ())
      m
end

module Make_seq (M : Intf.Core) :
  Intf.Foldable with type 'a t = 'a Seq.t and type 'a m := 'a M.t = struct
  type 'a t = 'a Seq.t

  let fold f acc m =
    let open M.Infix in
    Seq.fold_left
      (fun acc elt ->
        let* acc = acc in
        f acc elt)
      (return acc)
      m

  let iter f m =
    let open M.Infix in
    Seq.fold_left
      (fun acc elt ->
        let* () = acc in
        f elt)
      (return ())
      m
end

module Make_array (M : Intf.Core) :
  Intf.Foldable with type 'a t = 'a array and type 'a m := 'a M.t = struct
  type 'a t = 'a array

  let fold f acc m =
    let open M.Infix in
    Array.fold_left
      (fun acc elt ->
        let* acc = acc in
        f acc elt)
      (return acc)
      m

  let iter f m =
    let open M.Infix in
    Array.fold_left
      (fun acc elt ->
        let* () = acc in
        f elt)
      (return ())
      m
end
