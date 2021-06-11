let program_name = "build_matrix"

let () = Prometheus_unix.Logging.init ()

let pipeline () =
  let ctx = Matrix_ci_current.context ~host:"127.0.0.1" ~port:8008 ~user:"bot_2" ~pwd:"$crapaud$" ~device:None in
  let msg =
    Current.return ~label:"message" "message 2"
  in
  Matrix_ci_current.post ctx ~key:"key2" msg

let main config mode =
  let engine = Current.Engine.create ~config pipeline in
  let site = Current_web.Site.(v ~has_role:allow_all) ~name:program_name (Current_web.routes engine) in
  Lwt_main.run begin
    Lwt.choose [
      Current.Engine.thread engine;
      Current_web.run ~mode site;
    ]
  end

(* Command-line parsing *)

open Cmdliner

let cmd =
  let doc = "Sends a messaes to a matrix servery." in
  Term.(term_result (const main $ Current.Config.cmdliner $ Current_web.cmdliner)),
  Term.info program_name ~doc

let () = Term.(exit @@ eval cmd)
