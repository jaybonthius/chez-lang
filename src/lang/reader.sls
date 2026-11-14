(library (lang reader)
  (export
    default-readtable
    readtable?
    readtable-extend-datum-start
    readtable-extend-terminating
    readtable-extend-dispatch
    readtable-handler
    readtable-dispatch-handler

    read-mode
    read-mode?
    make-reader
    make-annotating-reader
    reader?
    reader-read
    reader-read-list
    reader-port
    reader-sfd
    reader-readtable
    read-all
    open-source-port

    reader-annotate
    reader-comment
    reader-error
    raise-read-error)
  (import (chezscheme))

  (include "reader/readtable.ss")
  (include "reader/state.ss")
  (include "reader/errors.ss")
  (include "reader/number.ss")
  (include "reader/lexer.ss")
  (include "reader/graph.ss")
  (include "reader/parser.ss")
  (include "reader/api.ss"))
