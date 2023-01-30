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
  Buffer.contents buffer

let test_result expected output exit_status =
  let exit_message = Unix.Exit_or_signal.to_string_hum exit_status in
  assert_equal "exited normally" exit_message ~printer: Fn.id;
  assert_equal expected output ~printer: Fn.id

let test_bash name batsh expected =
  let bash = Bash.compile batsh in
  let code = (Bash.print bash) ^ "\n" in
  (* Code *)
  let inx = In_channel.create (script_dir ^ "/bash/" ^ name ^ ".sh") in
  let code_expected = In_channel.input_all inx in
  In_channel.close inx;
  assert_equal code_expected code ~printer: Fn.id;
  (* Run result *)
  let stdout, stdin = Unix.open_process "bash" in