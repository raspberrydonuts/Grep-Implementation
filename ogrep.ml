(* OCaml grep implementation*)

(* apply fn to each line of a file *)
let for_each_line in_file (fn : string -> unit) =
  let rec loop () =
    let line = try Some (fn (input_line in_file)) with End_of_file ->  None
    in match line with Some _ -> loop () | None -> ()
  in loop ()

let grep_file (re : RegExpr.t) (filename : string) =
  let fh = if filename = "-" then "" else (filename ^ ":") in
  let in_f = if filename = "-" then stdin else open_in filename in
  let () = for_each_line in_f
    (fun s -> if RegExpr.regex_match re s then print_endline (fh^s) else ())
  in close_in in_f

let rec grep_file_list (re : RegExpr.t) (flist : string list) = match flist with
| [] -> ()
| fname::tl -> let () = grep_file re fname in grep_file_list re tl

let _ = if Array.length (Sys.argv) < 2 then failwith "insufficient arguments" else
  let (re_string::argl) = List.tl (Array.to_list Sys.argv) in
  let rex = RegExpr.rex_parse re_string in
  let argl' = match argl with [] -> ["-"] | _ -> argl in
  grep_file_list rex argl'
