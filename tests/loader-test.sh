#!/bin/sh
set -eu

scheme=${SCHEME:-/usr/lib/csv10.4.1/ta6le/scheme}
dir=$(mktemp -d /tmp/chez-lang-loader-test.XXXXXX)
trap 'rm -rf "$dir"' EXIT
cp -R src "$dir/src"
cp -R tests/lib "$dir/lib"
cd "$dir"

failures=0
fail() {
  echo "FAIL $1"
  failures=$((failures + 1))
}

cat > import.sps <<'EOF'
(import (chezscheme) (lang language) (lang loader))
(install-loader! '(".page"))
(define (argument i) (read (open-input-string (list-ref (command-line-arguments) i))))
(guard (c [(or (error? c) (violation? c)) (display-condition c) (newline) (exit 1)])
  (write (call-with-library-lock
           (lambda () (eval `(let () (import ,@(argument 0)) ,(argument 1))))))
  (newline))
EOF

import_value() {
  code=0
  output=$("$scheme" -q --libdirs src:lib --compile-imported-libraries --program import.sps "$1" "$2" 2>&1) || code=$?
  printf '%s\n' "$output" | tail -n 1
  return $code
}

expect() {
  actual=$(import_value "$2" "$3") || fail "$1: import failed: $actual"
  [ "$actual" = "$4" ] || fail "$1: expected $4, got $actual"
}

expect_error() {
  if actual=$(import_value "$2" "$3"); then
    fail "$1: import succeeded: $actual"
  else
    [ "$actual" = "$4" ] || fail "$1: expected $4, got $actual"
  fi
}

mtime() {
  stat -c %.9Y "$1"
}

hello='(("Hello, " (page-up "reader")) "text")'

expect "first import" "((pages hello))" doc "$hello"
[ -f lib/pages/hello.so ] || fail "the page was not compiled"
[ -f lib/pages/greet.so ] || fail "the library beside the page was not compiled"
before=$(mtime lib/pages/hello.so)
expect "second import" "((pages hello))" doc "$hello"
[ "$(mtime lib/pages/hello.so)" = "$before" ] || fail "a second import recompiled the page"

expect "page defines and exports a name its language exports" "((pages hello))" "(up 1)" "(page-up 1)"
expect "parser language page" "((pages lines))" doc '("first line" "second line")'
expect "plain language page" "((pages plain))" plain '"plain"'
printf '#!test/doc\n(define here (this-library))\n(this-library)\n' > lib/pages/named.sls
expect "body macro given the library name" "((pages named))" "(list here doc)" '((pages named) ((pages named)))'

printf '#!test/lines\nnote\n' > lib/pages/note.page
expect "page with an installed extension" "((pages note))" doc '("note")'
printf '#!test/lines\ntwice\n' > lib/pages/twice.chezscheme.sls
expect "longest extension is removed from the name" "((pages twice))" doc '("twice")'

expect "unrelated library" "((pages hello) (pages unrelated))" unrelated unrelated
for touched in lib/pages/hello.sls lib/test/doc.sls lib/test/doc/lang.sls \
    src/lang/reader.sls src/lang/language.sls src/lang/loader.sls \
    lib/pages/greet.sls; do
  before=$(mtime lib/pages/hello.so)
  sleep 1
  touch "$touched"
  expect "import after touching $touched" "((pages hello))" doc "$hello"
  [ "$(mtime lib/pages/hello.so)" != "$before" ] || fail "touching $touched did not recompile the page"
done
before=$(mtime lib/pages/hello.so)
unrelated_before=$(mtime lib/pages/unrelated.so)
sleep 1
touch lib/pages/unrelated.sls
expect "import after touching an unrelated library" "((pages hello) (pages unrelated))" unrelated unrelated
[ "$(mtime lib/pages/unrelated.so)" != "$unrelated_before" ] || fail "the unrelated library was not recompiled"
[ "$(mtime lib/pages/hello.so)" = "$before" ] || fail "touching an unrelated library recompiled the page"

cat > plain.sps <<'EOF'
(import (chezscheme) (pages hello))
(write (list doc (compile-imported-libraries)))
(newline)
EOF
before=$(mtime lib/pages/hello.so)
output=$("$scheme" -q --libdirs src:lib --program plain.sps 2>&1) || fail "plain scheme failed: $output"
[ "$output" = "($hello #f)" ] || fail "plain scheme: expected ($hello #f), got $output"
[ "$(mtime lib/pages/hello.so)" = "$before" ] || fail "plain scheme recompiled the page"

cat > program.sps <<'EOF'
#!test/doc
(import (pages greet))
(write (list (greet "program") (this-library)))
EOF
cat > compile-program.sps <<'EOF'
(import (chezscheme) (lang loader))
(install-loader!)
(maybe-compile-program "program.sps")
EOF
"$scheme" -q --libdirs src:lib --compile-imported-libraries --program compile-program.sps > /dev/null 2>&1 \
  || fail "compiling a program failed"
output=$("$scheme" -q --libdirs src:lib --program program.so 2>&1) || fail "the program failed: $output"
[ "$output" = '(("Hello, " "program") #f)' ] || fail "program: expected ((\"Hello, \" \"program\") #f), got $output"

printf '#!test/nope\n1\n' > lib/pages/unknown.sls
expect_error "unknown language" "((pages unknown))" doc \
  'Exception in read-language: unknown language test/nope: library (test nope lang) not found "test/nope" at line 1, char 3 of lib/pages/unknown.sls'

mkdir lib/test/nobody
cat > lib/test/nobody/lang.sls <<'EOF'
(library (test nobody lang)
  (export language)
  (import (chezscheme) (lang reader) (lang language))
  (define language
    (make-s-exp-language default-readtable (read-mode chez) '((test doc)) 'nobody-begin)))
EOF
printf '#!test/nobody\n1\n' > lib/pages/nobody.sls
expect_error "body macro not exported" "((pages nobody))" doc \
  'Exception in compile-lang-library: body macro nobody-begin is not exported by ((test doc))'

printf '#!test/doc\n(list "a\n' > lib/pages/unterminated.sls
expect_error "reader error" "((pages unterminated))" doc \
  'Exception in reader-read: unexpected end-of-file reading string "\"a\n" at line 2, char 7 of lib/pages/unterminated.sls'
[ ! -e lib/pages/unterminated.so ] || fail "a reader error left an object"

printf '#!test/doc\n"x"\n(list (lambda))\n' > lib/pages/invalid.sls
expect_error "syntax error" "((pages invalid))" doc \
  'Exception: invalid syntax (lambda) at line 3, char 7 of lib/pages/invalid.sls'
[ ! -e lib/pages/invalid.so ] || fail "a syntax error left an object"

cat > lib/pages/warns.sls <<'EOF'
(library (pages warns)
  (export warns)
  (import (chezscheme))
  (define-syntax warns
    (lambda (x)
      (warning 'warns "warning while expanding")
      #'1)))
EOF
printf '#!test/doc\n(import (pages warns))\n(warns)\n' > lib/pages/warning.sls
cat > warning.sps <<'EOF'
(import (chezscheme) (lang language) (lang loader))
(install-loader!)
(write (guard (c [(warning? c) 'caught])
         (call-with-library-lock (lambda () (eval '(let () (import (pages warning)) doc))))))
EOF
output=$("$scheme" -q --libdirs src:lib --compile-imported-libraries --program warning.sps 2>&1 | tail -n 1)
[ "$output" = caught ] || fail "warning caught by a guard: got $output"
[ -e lib/pages/warning.so ] || fail "a warning caught by a guard left no partial object"
before=$(mtime lib/pages/warning.so)
expect "import after a warning caught by a guard" "((pages warning))" doc "(1)"
[ "$(mtime lib/pages/warning.so)" != "$before" ] || fail "the partial object was not recompiled"
before=$(mtime lib/pages/warning.so)
expect "second import of a page that warns" "((pages warning))" doc "(1)"
[ "$(mtime lib/pages/warning.so)" = "$before" ] || fail "the recompiled page was compiled again"

printf '#!test/plain\n(library (pages extra) (export) (import (rnrs)))\n(display 1)\n' > lib/pages/extra.sls
expect_error "plain language with two forms" "((pages extra))" 0 \
  'Exception in compile-lang-library: unexpected form after the library form (display 1) at line 3, char 1 of lib/pages/extra.sls'

mkdir outside
printf '#!test/doc\n1\n' > outside/page.sls
cat > outside.sps <<'EOF'
(import (chezscheme) (lang loader))
(install-loader!)
(guard (c [#t (display-condition c) (newline) (exit 1)])
  (maybe-compile-library "outside/page.sls"))
EOF
output=$("$scheme" -q --libdirs src:lib --compile-imported-libraries --program outside.sps 2>&1 | tail -n 1)
[ "$output" = 'Exception in compile-lang-library: source file outside/page.sls is not in a library directory' ] \
  || fail "source outside the library directories: got $output"

cat > install.sps <<'EOF'
(import (chezscheme) (lang loader))
(define (who-of thunk)
  (guard (c [(assertion-violation? c) (condition-who c)]) (thunk) #f))
(write
  (list (who-of (lambda () (install-loader! '(page))))
        (begin (install-loader! '(".page" ".sls")) (compile-imported-libraries))
        (map car (library-extensions))
        (who-of install-loader!)))
(newline)
EOF
output=$("$scheme" -q --libdirs src:lib --program install.sps 2>&1) || fail "install-loader! checks failed: $output"
[ "$output" = '(install-loader! #t (".chezscheme.sls" ".ss" ".sls" ".scm" ".sch" ".page") install-loader!)' ] \
  || fail "install-loader! checks: got $output"

mkdir -p lib/only/doc
cat > lib/only/doc/lang.sls <<'EOF'
(library (only doc lang)
  (export language)
  (import (chezscheme) (lang reader) (lang language))
  (define language
    (make-s-exp-language default-readtable (read-mode chez) '((test lines)) 'lines-begin)))
EOF
printf '#!only/doc\n1 2\n' > lib/pages/keyword.sls
expect "language name starting with an import-set keyword" "((pages keyword))" doc '(1 2)'

printf '#!test/doc\n(define (twice x) (* 2 x))\n(twice 21)\n' > lib/pages/profiled.sls
cat > compile-profiled.sps <<'EOF'
(import (chezscheme) (lang loader))
(install-loader!)
(compile-profile 'block)
(maybe-compile-library "lib/pages/profiled.sls")
EOF
cat > run-profiled.sps <<'EOF'
(import (chezscheme) (pages profiled))
(write
  (list doc
        (exists (lambda (entry)
                  (and (equal? (source-file-descriptor-path (source-object-sfd (car entry)))
                               "lib/pages/profiled.sls")
                       (positive? (cdr entry))))
                (profile-dump))))
EOF
"$scheme" -q --libdirs src:lib --compile-imported-libraries --program compile-profiled.sps > /dev/null 2>&1 \
  || fail "compiling a page with block profiling failed"
output=$("$scheme" -q --libdirs src:lib --program run-profiled.sps 2>&1) || fail "the profiled page failed: $output"
[ "$output" = '((42) #t)' ] || fail "block profiling counts for the page: got $output"

cat > requirements.sps <<'EOF'
(import (chezscheme) (lang language) (lang loader))
(install-loader!)
(define (tracks? page names)
  (let ([imports (call-with-library-lock
                   (lambda ()
                     (environment page)
                     (library-requirements page (library-requirements-options import))))])
    (map (lambda (name) (and (member name imports) #t)) names)))
(write (list (tracks? '(pages hello) '((test doc lang) (lang reader) (lang loader)))
             (tracks? '(pages lines) '((test lines lang) (lang reader) (lang loader)))))
EOF
output=$("$scheme" -q --libdirs src:lib --compile-imported-libraries --program requirements.sps 2>&1 | tail -n 1)
[ "$output" = '((#t #t #t) (#t #f #t))' ] || fail "tracking imports: got $output"

mkdir lib/test/own
cat > lib/test/own.sls <<'EOF'
(library (test own)
  (export own-begin module)
  (import (except (chezscheme) module))

  (define (module x) (list 'own-module x))

  (define-syntax own-begin
    (lambda (x)
      (syntax-case x ()
        [(k library-name form ...)
         (with-syntax ([doc (datum->syntax #'k 'doc)])
           #'(begin (export doc) (define doc (list form ...))))]))))
EOF
cat > lib/test/own/lang.sls <<'EOF'
(library (test own lang)
  (export language)
  (import (chezscheme) (lang reader) (lang language))
  (define language
    (make-s-exp-language default-readtable (read-mode chez) '((test own)) 'own-begin)))
EOF
printf '#!test/own\n(module 1)\n' > lib/pages/own.sls
expect "language that exports its own module" "((pages own))" doc '((own-module 1))'

mkdir lib/test/cell
cat > lib/test/cell.sls <<'EOF'
(library (test cell)
  (export cell-begin define-cell)
  (import (chezscheme) (lang body))
  (export (import (chezscheme)))

  (define-syntax cell-begin
    (syntax-rules ()
      [(_ library-name form ...)
       (begin (implicit-exports #t) (body-begin library-name form ...))]))

  (define-syntax define-cell
    (syntax-rules ()
      [(_ name e)
       (begin
         (define cell (box e))
         (define-syntax name
           (identifier-syntax [id (unbox cell)] [(set! id v) (set-box! cell v)])))]))

  (define-property define-cell definer
    (lambda (form) (syntax-case form () [(_ name e) (list #'name)]))))
EOF
cat > lib/test/cell/lang.sls <<'EOF'
(library (test cell lang)
  (export language)
  (import (chezscheme) (lang reader) (lang language))
  (define language
    (make-s-exp-language default-readtable (read-mode chez) '((test cell)) 'cell-begin)))
EOF
cat > lib/pages/cell.sls <<'EOF'
#!test/cell
(define-cell count 1)
(define (bump!) (set! count (+ count 1)) count)
(define (map f x) (list 'mine (f x)))
(define (negated x) (map - x))
EOF
expect "implicit exports in the body module" "((pages cell))" "(list (bump!) count (negated 1))" '(2 2 (mine -1))'

printf '#!test/plain\n(import (rnrs) (pages greet))\n(write (greet "plain"))\n' > plain-program.sps
cat > compile-plain-program.sps <<'EOF'
(import (chezscheme) (lang loader))
(install-loader!)
(maybe-compile-program "plain-program.sps")
EOF
"$scheme" -q --libdirs src:lib --compile-imported-libraries --program compile-plain-program.sps > /dev/null 2>&1 \
  || fail "compiling a plain program failed"
output=$("$scheme" -q --libdirs src:lib --program plain-program.so 2>&1) || fail "the plain program failed: $output"
[ "$output" = '("Hello, " "plain")' ] || fail "plain program: got $output"

printf '#!test/plain\n' > lib/pages/empty.sls
expect_error "plain language without a library form" "((pages empty))" 0 \
  'Exception in compile-lang-library: expected one library form "lib/pages/empty.sls"'
printf '#!test/plain\n(library (pages short) (export))\n' > lib/pages/short.sls
expect_error "plain library without an import clause" "((pages short))" 0 \
  'Exception in compile-lang-library: expected (library name (export ...) (import ...) body ...) (library (pages short) (export)) at line 2, char 1 of lib/pages/short.sls'

cat > typed.sps <<'EOF'
(import (chezscheme) (lang language) (lang loader))
(install-loader!)
(define (argument i) (read (open-input-string (list-ref (command-line-arguments) i))))
(guard (c [(language-error? c) (display "&language-error ") (display-condition c) (newline) (exit 1)])
  (call-with-library-lock (lambda () (eval `(let () (import ,(argument 0)) 0)))))
EOF
expect_cycle() {
  if output=$(timeout 60 "$scheme" -q --libdirs src:lib --compile-imported-libraries --program typed.sps "$2" 2>&1); then
    fail "$1: import succeeded"
  else
    output=$(printf '%s\n' "$output" | tail -n 1)
    [ "$output" = "$3" ] || fail "$1: expected $3, got $output"
  fi
}

mkdir lib/cyc
cat > lib/cyc/lang.sls <<'EOF'
(library (cyc lang)
  (export language)
  (import (chezscheme) (lang reader) (lang language))
  (define language (make-s-exp-language default-readtable (read-mode chez) '((cyc)) 'doc-begin)))
EOF
printf '#!cyc\n1\n' > lib/cyc.sls
expect_cycle "page that is its language's module language" "(cyc)" \
  '&language-error Exception in compile-lang-library: language cyc depends on the page lib/cyc.sls being compiled'
cat > refix.sps <<'EOF'
(import (chezscheme) (lang language) (lang loader))
(install-loader!)
(define (import-cyc)
  (call-with-library-lock (lambda () (eval '(let () (import (cyc)) doc)))))
(guard (c [(language-error? c) #t]) (import-cyc))
(call-with-output-file "lib/cyc.sls" (lambda (p) (display "#!test/doc\n\"fixed\"\n" p)) 'replace)
(guard (c [#t (display-condition c) (newline) (exit 1)])
  (write (import-cyc)))
EOF
output=$(timeout 60 "$scheme" -q --libdirs src:lib --compile-imported-libraries --program refix.sps 2>&1 | tail -n 1)
[ "$output" = '("fixed")' ] || fail "page fixed after a cycle error, in the same process: got $output"
printf '#!cyc\n1\n' > lib/cyc.sls
mkdir lib/self
printf '#!self\n1\n' > lib/self/lang.sls
expect_cycle "language library in its own language" "(self lang)" \
  '&language-error Exception in read-language: language self depends on itself through library (self lang) "self" at line 1, char 3 of lib/self/lang.sls'

mkdir lib/m8 lib/p8
cat > lib/m8/lang.sls <<'EOF'
(library (m8 lang)
  (export language)
  (import (chezscheme) (lang language))
  (define language
    (make-meta-language (lambda (readtable) (eval '(let () (import (p8 page)) 0)) readtable))))
EOF
printf '#!m8 test/doc\n1\n' > lib/p8/page.sls
expect_cycle "meta-language extension that imports a page in its language" "(p8 page)" \
  '&language-error Exception in read-language: language m8 depends on itself through library (m8 lang) "m8" at line 1, char 3 of lib/p8/page.sls'

echo "loader-test: $failures failures"
[ "$failures" -eq 0 ]
