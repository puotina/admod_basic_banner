open Core
open Unix.Process_channels
open OUnit
open Batsh_lib

let script_dir = "test_scripts"

let drop_carrage_return str =
  let buffer = Buffer.create (String.length str) in
  String.iter str ~f:(fun ch ->
      if not (Char.equal ch '\r') then
        Buffer.add_char buffer ch
    );
  Buffer.contents 