module C = Configurator.V1

let () =
  C.main ~name:"" (fun _ ->
      C.Flags.write_sexp "flags.sexp" (match Sys.getenv_opt "SODIUM_LIB_DIR" with | Some path -> ["-cclib"; "-L" ^ path] | None -> []))
