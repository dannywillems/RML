let () =
  Ocamlbuild_plugin.dispatch (fun phase ->
    Ocamlbuild_cppo.dispatcher phase
  )
