;;; go-playground.el --- Local Golang playground for short snippets.  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Alexander I.Grafov (axel)

;; Author: Alexander I.Grafov (axel) <grafov@gmail.com>
;; Keywords: tools, golang

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'go-mode)
(require 'compile)
(require 'time-stamp)

(define-minor-mode go-playground-mode
  "A place for playing with golang code and export it in short snippets."
  :init-value nil
  :lighter ""
  :keymap '(([C-return] . go-playground-save-and-run))
  (setq mode-name "Play(Go)"))

(defun go-playground-save-and-run ()
  (interactive)
  (save-buffer t)
  (make-local-variable 'compile-command)       
  (compile (concat go-command " run *.go")))

;; draft
(defun go-playground-print-unused ()
  (interactive)
  (save-buffer t)
  (let ((snippet-buf (current-buffer)) (compile-buf (compile (go-run-get-program (go-run-arguments)))))
    (set-buffer compile-buf)
    (looking-at "^.*:[0-9]+: \\([_.a-zA-Z0-9]+\\) declared and not used")
    (let ((not-used-var (match-string 0)))
            (set-buffer snippet-buf)
      (insert not-used-var))))
      

(defun go-playground-send-to-play.golang.org ()
  (interactive)
  (goto-char (point-min))
  (forward-line)
  (insert (go-play-buffer)))
       
(defgroup go-playground nil
   "Options specific to `go-playground`."
  :group 'go)

(defcustom go-playground-basedir "~/go/src/playground"
  "Base directory for playground snippets. Better to set it under GOPATH."
  :group 'go-playground)

(defun go-playground ()
  (interactive)
  (let ((snippet-file-name (concat (go-playground-snippet-unique-dir) "/snippet.go")))
    (switch-to-buffer (create-file-buffer snippet-file-name))
    (insert "// snippet of code
// run snippet with Ctl-Return 

package main

func main() {
  
}
")
    (backward-char 3)
    (go-mode)
    (go-playground-mode)
    (set-visited-file-name snippet-file-name t)))


(defun go-playground-snippet-unique-dir ()
  (let ((dir-name (concat go-playground-basedir "/" (time-stamp-string "%:y-%02m-%02d-%02H:%02M:%02S"))))
    (make-directory dir-name t)
    dir-name))


(provide 'go-playground)
;;; goplay-mode.el ends here
