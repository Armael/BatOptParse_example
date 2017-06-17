open Batteries
module P = BatOptParse

(* HEAD(1) supports a multiplier suffix for the argument to [bytes] and [lines]
   (e.g. MB = 1000*1000, M = 1024*1024, ...).

   Here we only implement support for a naked integer parameter.

   Given a parser function [parse : string -> int] which implements parsing of
   integers with multiplier suffixes, we could extend the current implementation
   to support the multiplier as follows:

   let int_with_multiplier_option () =
     P.Opt.callback_option
       "NUM"
       parse
       (fun exn s -> "Invalid argument: " ^ s)

   let bytes = int_with_multiplier_option ()
   let lines = int_with_multiplier_option ()
*)

(* The [metavar] optional argument is a string that represents the argument of
   the option, and can be mentioned in its description (see the [help] argument
   to [P.OptParser.add] below).
*)
let bytes_opt = P.StdOpt.int_option ~metavar:"NUM" ()
let lines_opt = P.StdOpt.int_option ~metavar:"NUM" ~default:10 ()

(* False by default; will be true is the option is present. *)
let zero_terminated_opt = P.StdOpt.store_true ()

(* We could have more simply done:

   let quiet = P.StdOpt.store_true ()
   let verbose = P.StdOpt.store_true ()

   and then decided arbitrarily than one overrides the other if both are present.

   Here, for the sake of the exercise, we decide to mimic the behavior of the
   GNU head program, where the option that appears last overrides the other.
*)
let is_verbose = ref (Some false)
let store_const const = P.Opt.{
  option_metavars = [];
  option_defhelp = None;
  option_get = (fun _ -> !is_verbose);
  option_set_value = (fun x -> is_verbose := Some x);
  option_set = (fun _ _ -> is_verbose := Some const)
}

let quiet_opt = store_const false
let verbose_opt = store_const true

(* Creates a [P.OptParser.t] value, which will collect the set of options we
   want to parse, associated to option names, and some documentation.
*)
let optparser : P.OptParser.t =
  P.OptParser.make
    ~prog:"head"
    ~usage:"%prog - output the first part of files"
    ~version:"0.1"
    ()

(* Add our options to [optparser]. *)
let () =
  P.OptParser.add optparser
    ~help:"print the first NUM bytes of each file"
    ~short_name:'c'
    ~long_name:"bytes"
    bytes_opt;
  P.OptParser.add optparser
    ~help:"print the first NUM lines instead of the first 10"
    ~short_name:'n'
    ~long_name:"lines"
    lines_opt;
  P.OptParser.add optparser
    ~help:"never print headers giving file names"
    ~short_name:'q'
    ~long_names:["quiet"; "silent"]
    quiet_opt;
  P.OptParser.add optparser
    ~help:"always print headers giving file names"
    ~short_name:'v'
    ~long_name:"verbose"
    verbose_opt;
  P.OptParser.add optparser
    ~help:"line delimiter is NUL, not newline"
    ~short_name:'z'
    ~long_name:"zero-terminated"
    zero_terminated_opt

(* Parse the command-line options added to [optparser]. Returns the list of
   non-optional arguments; in our case, a list of filenames.
*)
let files = P.OptParser.parse_argv optparser

(* Read and interpret the values of parsed options.
*)
let line_terminator =
  if P.Opt.get zero_terminated_opt then Char.chr 0
  else '\n'

let verbose = P.Opt.get verbose_opt

let n, mode =
  match P.Opt.opt bytes_opt with
  | None -> P.Opt.get lines_opt, `Lines
  | Some n -> n, `Bytes

(*** Now, the actual implementation of [head]. ***)

(* Reads a line, terminated by [term]. *)
let read_until (term : char) (input : BatIO.input) : string =
  let buf = Buffer.create 37 in
  let rec loop () =
    try
      let c = input_char input in
      Buffer.add_char buf c;
      if c = term then Buffer.contents buf
      else loop ()
    with End_of_file | BatInnerIO.Input_closed ->
      Buffer.contents buf
  in
  loop ()

(* A functions that wraps some computation [f input filename], providing
   resource management (cleanly closes the input at the end, if needed).
*)
let with_file (f : BatIO.input -> string -> 'a) (filename : string) : 'a =
  if filename = "-" then f stdin filename
  else
    let input = open_in filename in
    let ret = f input filename in
    close_in input;
    ret

let () =
  List.iter (with_file (fun input filename ->
    if verbose then Printf.printf "==> %s <==\n%!"
        (if filename = "-" then "standard input" else filename);

    for _i = 0 to n - 1 do
      match mode with
      | `Lines ->
        output_string stdout @@ read_until line_terminator input;
        flush stdout
      | `Bytes ->
        output_char stdout @@ input_char input
    done
  )) files
