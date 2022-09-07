
open Core_kernel
open Cmdliner
open Batsh_lib

(* Options common to all commands *)

type output_type = Code | Ast | Symbols

type copts = {
  output_type : output_type;
  output_file : string option
}

let copts_sect = "COMMON OPTIONS"

let copts_t =
  let docs = copts_sect in

  let output_type =
    let doc = "Print abstract syntax tree instead." in
    let quiet = Ast, Arg.info ["ast"] ~docs ~doc in

    let doc = "Print symbol table instead." in
    let verbose = Symbols, Arg.info ["symbols"] ~docs ~doc in
    Arg.(last & vflag_all [Code] [quiet; verbose])
  in

  let output_file =
    let doc = "Write output to $(docv)." in
    let opts = ["o"; "output"] in
    Arg.(value & opt (some string) None & info opts ~docs ~doc ~docv:"FILE")
  in
  let copts_cons output_type output_file = { output_type; output_file } in
  Term.(pure copts_cons $ output_type $ output_file)

let get_outx opts =
  match opts.output_file with
  | Some filename -> Out_channel.create filename
  | None -> Out_channel.stdout

let print_common opts ~batsh ~code ~ast =
  let outx = get_outx opts in
  match opts.output_type with
  | Code ->
    fprintf outx "%s\n" (Lazy.force code)
  | Ast ->
    fprintf outx "%a\n" Sexp.output_hum (Lazy.force ast)
  | Symbols ->
    let symtable_sexp = Symbol_table.sexp_of_t (Parser.symtable batsh) in
    fprintf outx "%a\n" Sexp.output_hum symtable_sexp
