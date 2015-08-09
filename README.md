# go-playground

Very simple Emacs mode for local golang playground similar
to play.golang.org. It just uses `go-mode` and all its benefits.
I recommend to use `goimports` for
automatic imports.

You may share snippets with `go-play-buffer` from `go-mode`
and publish gists with `gist-buffer` (install it separately from melpa).

`go-playground` needs `go-mode` and `gotest` packages installed.

Usage:

* `M-x go-playground` for start playground buffer for new snippet
* `Ctl-Return` for compile and run it.
