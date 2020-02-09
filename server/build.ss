#!/usr/bin/env gxi

(import :std/make)


(def bin-build-spec
  '((static-exe: "app" "-ld-options" "-lssl -lcrypto")))


;; the source directory anchor
(def srcdir
  (path-normalize (path-directory (this-source-file))))

;; the main function of the script
(def (main . args)
  (match args
    ;; this action computes the dependency graph for the project
    (["deps"]
     (cons-load-path srcdir)
     (let (build-deps (make-depgraph/spec bin-build-spec))
       (call-with-output-file "build-deps" (cut write build-deps <>))))

    (["bin"]
     ;; this action builds the static executables -- no debug introspection
     (let (depgraph (call-with-input-file "build-deps" read))
       (make srcdir: srcdir
             bindir: srcdir
             optimize: #t
             debug: #f               ; no debug bloat for executables
             static: #t              ; generate static compilation artifacts; required!
             depgraph: depgraph
             prefix: "example"
             bin-build-spec)))

    ;; this is the default action, builds the project using the depgraph produced by deps
    ([]
     (main "bin"))))
