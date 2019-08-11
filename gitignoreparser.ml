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

let rec matches pattern file = match pattern with
  | Neg a -> not(matches a file)
  | File a -> a = file
  | Directory a -> is_in_directory file a
  | Pattern a -> (match Re.exec_opt a file with
    | Some _ -> true
    | None -> false)

let%test _ = matches (compile "foo/*") "foo/bar"
let%test _ = matches (compile "foo/*/bar") "foo/xxx/bar"
let%test _ = matches (compile "foo/**/bar") "foo/xxx/yyy/bar"
let%test _ = matches (compile "*.sw[op]") "foo.swo"
let%test _ = matches (compile "~*") "~foo"
let%test _ = not @@ matches (compile "!foo/*/bar") "foo/xxx/bar"
let%test _ = not @@ matches (compile "foo/*/bar") "foo/xxx/yyy/bar"
let%test _ = matches (compile "foo/") "foo/"
let%test _ = matches (compile "foo/") "foo/bar"
