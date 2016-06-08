# Local Golang playground [![MELPA](http://melpa.org/packages/go-playground-badge.svg)](http://melpa.org/#/go-playground)

Simple Emacs mode for local [Go language](http://golang.org) playground similar
to [play.golang.org](http://play.golang.org).

Web playground at play.golang.org is nice idea. It has not require setup and it executes your code
in a restricted environment where you can't damage you system. But it brings many restrictions too.
Local playground requres you to setup Go environment but if you developing in Go (or want to do it)
you are anyway need setup `go-mode` and helper modes for working with code in a comfort way.

So with local go-playground you get:

* all features you have for `go-mode` will work in `go-playground`
* you can keep snippets library locally under GOPATH
* there are no containers and no restrictions on compile time so you can test the complex code
* you are free for using any external packages from your GOPATH
* you feel free for publishing your snippets to play.golang.org


## Install

Firstly install `go-mode` and `gotest` — these are mandatory. Setup any additional tools you want for Golang
(see Emacs Wiki for guides or google it for "emacs+golang"). Personally I recommend use `goimports` for
automatically add import clauses to your snippets. Because usually when you test some ideas you want get
working code as fast as possible. And `go-playground` helps with it.

Install `go-playground` from MELPA:

    M-x package-install RET go-playground

If you want share snippets use `go-play-buffer` from `go-mode`.
Install `gist-buffer` from MELPA if you want publish gists on github.com.

`go-playground` needs `go-mode` and `gotest` package installed.

## Usage

* `M-x go-playground` for start playground buffer for a new snippet from any mode
* `Ctl-Return` for compile and run your code
* `M-x go-playground-remove-current-snippet` for removing current snippet (with its directory and all files) after you done with it

Mode not offers default bindings except `Ctl-Return`. Just left it for you.

## Similar projects

* Try [go-scratch](https://github.com/shosti/go-scratch.el) it even simplier than `go-playground`. But it prevents you from using many of tools because it not keeps code in files.
* [gorepl-mode](https://github.com/manute/gorepl-mode]) is REPL for Go, helpful for interactive research of code.

## Licence

Under terms of GPL v3. See LICENSE file.

## Future plans and accepting contributions

I don't want to make this package much complex. Most of `go-playground` functions depends on other packages around `go-mode`.
And it is good way. 
But one thing I want to add is snippet list buffer where you can easily find and edit/delete snippets of your library.
Bugfixes and small features accepted as well.
