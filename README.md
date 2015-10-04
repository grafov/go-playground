# Local Golang playground

Simple Emacs mode for local [Go language](http://golang.org) playground similar
to [play.golang.org](http://play.golang.org). It just uses `go-mode` and all its benefits.
I recommend to use `goimports` for automatically add import clauses to your snippet.

You may share snippets with `go-play-buffer` from `go-mode`
and publish gists with `gist-buffer` (install it separately from melpa).

`go-playground` needs `go-mode` package installed.

## Usage

* `M-x go-playground` for start playground buffer for new snippet
* `Ctl-Return` for compile and run your code.
* `M-x go-playground-remove-current-snippet` for removing snippet after you done with it.

Mode not offers default bindings except `Ctl-Return` for `go run` your snippet.

Under terms of GPL v3.
