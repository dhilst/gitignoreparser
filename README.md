# Git ignore parser in Ocaml

Provide functions to parse .gitignore lines and use

Some examples from the tests
``` ocaml
let%test _ = matches (compile "foo/*") "foo/bar"
let%test _ = matches (compile "foo/*/bar") "foo/xxx/bar"
let%test _ = matches (compile "foo/**/bar") "foo/xxx/yyy/bar"
let%test _ = matches (compile "*.sw[op]") "foo.swo"
let%test _ = matches (compile "~*") "~foo"
let%test _ = not @@ matches (compile "!foo/*/bar") "foo/xxx/bar"
let%test _ = not @@ matches (compile "foo/*/bar") "foo/xxx/yyy/bar"
```

