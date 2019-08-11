open Core

type t =
  | Neg of t
  | Pattern of Re.re
  | File of string
  | Directory of string

let rec compile = function%pcre
  | {|^!(?<rest>.*)|} -> Neg (compile rest)
  | {|^(?<prefix>.*?)\s*#|} -> compile prefix
  | {|^(?<prefix>.*?)\*\*(?<suffix>.*)|} ->
      let open Re.Pcre in
      let prefix = quote prefix in
      let suffix = quote suffix in
      let pattern = prefix ^ {|.*|} ^ suffix in
      let r = regexp pattern in
      Pattern r
  | {|?<line>.*?\*.*|} ->
      let r = Re.Glob.glob line |> Re.compile in
      Pattern r
  | {|^(?<line>.*?/)|} -> Directory line
  | line -> File line

let is_in_directory file directory =
  match%pcre file with
  | {|^(?<path>.*/)|} -> path = directory
  | _ -> false

let compare_patterns p1 p2 =
  match (p1,p2) with
  | Neg _, _ -> -1
  | Directory a, Directory b -> String.compare a b
  | Directory _, _ -> -1
  | _, Directory _ -> 1
  | File _, File _ -> 0
  | File _, _ -> -1
  | _, _ -> 1

let rec matches pattern file = match pattern with
  | Neg a -> not(matches a file)
  | Directory a -> is_in_directory file a
  | File a -> a = file
  | Pattern a -> (match Re.exec_opt a file with
    | Some _ -> true
    | None -> false)

let compile_file ?(acc=[]) filename : t list =
  let open In_channel in
  with_file filename ~f:(fun ic ->
    fold_lines ic ~init:acc ~f:(fun acc line ->
      let p = compile line in
      p::acc
    )
  )

let ignored patterns entry =
  let exception Ignored in
  try
    List.iter patterns ~f:(fun p ->
      match (matches p entry) with
      | true -> raise Ignored
      | _ -> ()
    ); false
  with Ignored -> true

let strip_dot_slash filename = match%pcre filename with
| {|^\./(?<f>.*)|} -> f
| _ -> filename

let rec walkdir start ignore_patterns ~f =
  let open Lwt in
  let open Lwt_unix in
  let%lwt dir = opendir start in
  try%lwt
    while%lwt true do
      let%lwt entry = readdir dir in
      let path = Filename.concat start entry |> strip_dot_slash in
      match entry with
      | "." | ".." | ".git" -> return_unit
      | _ -> begin
        match (ignored ignore_patterns path) with
        | true -> return_unit
        | _ -> let%lwt s = stat path in
          match s.st_kind with
          | S_DIR -> walkdir path ignore_patterns ~f
          | S_REG -> Lwt_preemptive.detach f path
          | _ -> return_unit
      end
    done
  with
  | Caml.End_of_file -> closedir dir

let%test _ = matches (compile "foo/*") "foo/bar"
let%test _ = matches (compile "foo/*/bar") "foo/xxx/bar"
let%test _ = matches (compile "foo/**/bar") "foo/xxx/yyy/bar"
let%test _ = matches (compile "*.sw[op]") "foo.swo"
let%test _ = matches (compile "~*") "~foo"
let%test _ = not @@ matches (compile "!foo/*/bar") "foo/xxx/bar"
let%test _ = not @@ matches (compile "foo/*/bar") "foo/xxx/yyy/bar"
let%test _ = matches (compile "foo/") "foo/"
let%test _ = matches (compile "foo/") "foo/bar"
let%test _ = compare_patterns (Directory "foo") (File "bar") = -1
let%test _ = compare_patterns (File "foo") (File "bar") = 0
let%test _ = List.sort ~compare:compare_patterns [compile "foo/*"; Directory "foo/"; File "foo"] = [Directory "foo/"; File "foo"; compile "foo/*"]
let%test _ = compile_file "../../.gitignore" |> ignore; true
let%test _ =
  let () = Unix.chdir "../../" in
  let () = () in Lwt_main.run begin
    walkdir "." (compile_file ".gitignore") ~f:print_endline
  end; true
