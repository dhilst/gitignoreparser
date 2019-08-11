# Git ignore parser in Ocaml

Provide functions to parse .gitignore lines and use

``` ocaml
let open Gitignoreparser in
let p1 = compile "foo/*" in
let p2 = compile "!foo/bar" in
let () = assert matches p1 "foo/bar" in
let () = assert not (matches p2 "foo/bar") in
()
```

