#!/bin/sh
set -eu

scheme=${SCHEME:-/usr/lib/csv10.4.1/ta6le/scheme}
dir=$(mktemp -d /tmp/chez-lang-reload-test.XXXXXX)
trap 'rm -rf "$dir"' EXIT
cp -R src "$dir/src"
cp -R tests/lib "$dir/lib"
cd "$dir"

failures=0
check() {
  if [ "$2" != "$3" ]; then
    echo "FAIL $1"
    printf '%s\n' "$2" > expected.txt
    printf '%s\n' "$3" > actual.txt
    diff expected.txt actual.txt || true
    failures=$((failures + 1))
  fi
}

run() {
  "$scheme" -q --libdirs src:lib --compile-imported-libraries --program "$@" 2>&1 || true
}

cat > lib/pages/shown.sls <<'EOF'
(library (pages shown)
  (export shown)
  (import (chezscheme) (pages hello))
  (define shown (list 'shown doc)))
EOF
printf '#!test/doc\n(define-meta title "Post")\n(this-library)\n' > lib/pages/post.sls
sed "s/'((test doc))/'((only (test doc) up define-meta))/" lib/test/doc/lang.sls > doc-lang-without-body.sls

cat > helpers.ss <<'EOF'
(define (show label x)
  (printf "~a: ~s\n" label x))

(define (outcome thunk)
  (guard (c [(or (error? c) (violation? c))
             (call-with-string-output-port (lambda (p) (display-condition c p)))])
    (thunk)))

(define (value library name)
  (call-with-library-lock (lambda () (eval name (environment library)))))

(define (imported libraries expression)
  (call-with-library-lock (lambda () (eval `(let () (import ,@libraries) ,expression)))))

(define (reload name)
  (outcome (lambda () (reload-library name))))

;; File times are coarse, so a source written in the second its object was
;; compiled could look no newer than the object.
(define (edit! path text)
  (sleep (make-time 'time-duration 0 1))
  (call-with-output-file path (lambda (p) (put-string p text)) 'replace))

(define (objects . paths)
  (map file-exists? paths))
EOF

cat > reload.sps <<'EOF'
(import (chezscheme) (chez-lang language) (chez-lang loader) (chez-lang reload))
(install-loader!)
(include "helpers.ss")

(show "start" (value '(pages shown) 'shown))
(show "nothing changed" (reload '(pages hello)))
(show "not loaded" (reload '(pages unrelated)))

(edit! "lib/pages/hello.sls" "#!test/doc\n(import (pages greet))\n(greet \"edited\")\n")
(show "edited page" (reload '(pages hello)))
(show "environment after page" (value '(pages hello) 'doc))
(show "importer after page" (value '(pages shown) 'shown))
(show "import after page" (imported '((pages hello) (pages shown)) '(list doc shown)))
(show "nothing changed after page" (reload '(pages hello)))

(edit! "lib/pages/greet.sls"
  "(library (pages greet) (export greet) (import (chezscheme)) (define (greet name) (list \"Hi, \" name)))")
(show "edited library" (reload '(pages greet)))
(show "importer after library" (value '(pages shown) 'shown))

(edit! "lib/pages/hello.sls" "#!test/doc\n(list (lambda))\n")
(show "page that fails" (reload '(pages hello)))
(show "page after failure" (value '(pages hello) 'doc))
(show "importer after failure" (value '(pages shown) 'shown))
(show "import after failure" (imported '((pages shown)) 'shown))
(show "objects after failure" (objects "lib/pages/hello.so" "lib/pages/shown.so"))
(show "page that fails again" (reload '(pages hello)))
(show "page after second failure" (value '(pages hello) 'doc))

(edit! "lib/pages/hello.sls" "#!test/doc\n(import (pages greet))\n(greet \"fixed\")\n")
(show "fixed page" (reload '(pages hello)))
(show "importer after fix" (value '(pages shown) 'shown))

(edit! "lib/pages/greet.sls"
  "(library (pages greet) (export hi) (import (chezscheme)) (define (hi name) (list \"Hey, \" name)))")
(show "library whose importer fails" (reload '(pages greet)))
(show "library after its importer failed" (library-exports '(pages greet)))
(show "importer after rollback" (value '(pages shown) 'shown))
(show "objects after rollback" (objects "lib/pages/greet.so" "lib/pages/hello.so" "lib/pages/shown.so"))
(edit! "lib/pages/hello.sls" "#!test/doc\n(import (pages greet))\n(hi \"both\")\n")
(show "page needing the library not yet reloaded" (reload '(pages hello)))
(show "library and page" (reload '(pages greet)))
(show "importer after both" (value '(pages shown) 'shown))

(show "post and part" (list (value '(pages post) 'doc) (value '(pages post metas) 'metas)))
(edit! "lib/pages/post.sls" "#!test/doc\n(define-meta title \"Edited\")\n")
(show "page with a part" (list-sort (lambda (a b) (< (length a) (length b))) (reload '(pages post))))
(show "post and part after reload" (list (value '(pages post) 'doc) (value '(pages post metas) 'metas)))

(edit! "lib/test/doc/lang.sls" (call-with-input-file "lib/test/doc/lang.sls" get-string-all))
(show "language library"
  (list-sort (lambda (a b) (string<? (format "~s" a) (format "~s" b))) (reload '(test doc lang))))
(define doc-lang (call-with-input-file "lib/test/doc/lang.sls" get-string-all))
(edit! "lib/test/doc/lang.sls" (call-with-input-file "doc-lang-without-body.sls" get-string-all))
(show "language whose imports lose its body macro" (reload '(test doc lang)))
(edit! "lib/test/doc/lang.sls" doc-lang)
(show "language restored"
  (list-sort (lambda (a b) (string<? (format "~s" a) (format "~s" b))) (reload '(test doc lang))))

(show "no source" (reload '(pages nowhere)))
(show "not a library name" (reload 'pages))
EOF

check "reload in the process that compiled every library" 'start: (shown (("Hello, " (page-up "reader")) "text"))
nothing changed: ()
not loaded: ()
edited page: ((pages hello) (pages shown))
environment after page: (("Hello, " "edited"))
importer after page: (shown (("Hello, " "edited")))
import after page: ((("Hello, " "edited")) (shown (("Hello, " "edited"))))
nothing changed after page: ()
edited library: ((pages greet) (pages hello) (pages shown))
importer after library: (shown (("Hi, " "edited")))
page that fails: "Exception: invalid syntax (lambda) at line 2, char 7 of lib/pages/hello.sls"
page after failure: (("Hi, " "edited"))
importer after failure: (shown (("Hi, " "edited")))
import after failure: (shown (("Hi, " "edited")))
objects after failure: (#f #t)
page that fails again: "Exception: invalid syntax (lambda) at line 2, char 7 of lib/pages/hello.sls"
page after second failure: (("Hi, " "edited"))
fixed page: ((pages hello) (pages shown))
importer after fix: (shown (("Hi, " "fixed")))
library whose importer fails: "Exception: attempt to reference unbound identifier greet at line 3, char 2 of lib/pages/hello.sls"
library after its importer failed: (greet)
importer after rollback: (shown (("Hi, " "fixed")))
objects after rollback: (#f #f #t)
page needing the library not yet reloaded: "Exception: attempt to reference unbound identifier hi at line 3, char 2 of lib/pages/hello.sls"
library and page: ((pages greet) (pages hello) (pages shown))
importer after both: (shown (("Hey, " "both")))
post and part: (((pages post)) ((title . "Post")))
page with a part: ((pages post) (pages post metas))
post and part after reload: (() ((title . "Edited")))
language library: ((pages hello) (pages post metas) (pages post) (pages shown) (test doc lang))
language whose imports lose its body macro: "Exception in compile-lang-library: body macro doc-begin is not exported by ((only (test doc) up define-meta))"
language restored: ((pages hello) (pages post metas) (pages post) (pages shown) (test doc lang))
no source: "Exception in reload-library: library (pages nowhere) has no source file"
not a library name: "Exception in reload-library: pages is not a library name"' \
  "$(run reload.sps | grep -v '^compiling ')"

# Every library this process uses loads from the object the run above wrote,
# so nothing compiles until a reload.
cat > fresh.sps <<'EOF'
(import (chezscheme) (chez-lang language) (chez-lang loader) (chez-lang reload))
(install-loader!)
(include "helpers.ss")

(show "start" (value '(pages shown) 'shown))
(edit! "lib/pages/hello.sls" "#!test/doc\n(import (pages greet))\n(hi \"fresh\")\n")
(show "edited page" (reload '(pages hello)))
(show "importer after page" (value '(pages shown) 'shown))
(edit! "lib/pages/greet.sls"
  "(library (pages greet) (export hi) (import (chezscheme)) (define (hi name) (list \"Ho, \" name)))")
(show "edited library" (reload '(pages greet)))
(show "importer after library" (value '(pages shown) 'shown))
(edit! "lib/pages/hello.sls" "#!test/doc\n(list (lambda))\n")
(show "page that fails" (reload '(pages hello)))
(show "importer after failure" (imported '((pages shown)) 'shown))
(edit! "lib/pages/hello.sls" "#!test/doc\n(import (pages greet))\n(hi \"fixed\")\n")
(show "fixed page" (reload '(pages hello)))
(show "importer after fix" (value '(pages shown) 'shown))
EOF

check "reload in a process that loaded every library from its object" 'start: (shown (("Hey, " "both")))
compiling lib/pages/hello.sls with output to lib/pages/hello.so
compiling lib/pages/shown.sls with output to lib/pages/shown.so
edited page: ((pages hello) (pages shown))
importer after page: (shown (("Hey, " "fresh")))
compiling lib/pages/greet.sls with output to lib/pages/greet.so
compiling lib/pages/hello.sls with output to lib/pages/hello.so
compiling lib/pages/shown.sls with output to lib/pages/shown.so
edited library: ((pages greet) (pages hello) (pages shown))
importer after library: (shown (("Ho, " "fresh")))
compiling lib/pages/hello.sls with output to lib/pages/hello.so
page that fails: "Exception: invalid syntax (lambda) at line 2, char 7 of lib/pages/hello.sls"
importer after failure: (shown (("Ho, " "fresh")))
compiling lib/pages/hello.sls with output to lib/pages/hello.so
compiling lib/pages/shown.sls with output to lib/pages/shown.so
fixed page: ((pages hello) (pages shown))
importer after fix: (shown (("Ho, " "fixed")))' "$(run fresh.sps)"

# A compiled program only runs the libraries it imports, so they have no
# compile-time information until something imports them.
cat > main.sps <<'EOF'
(import (chezscheme) (chez-lang language) (chez-lang loader) (chez-lang reload) (pages shown))
(install-loader!)
(include "helpers.ss")

(edit! "lib/pages/hello.sls" "#!test/doc\n(import (pages greet))\n(hi \"compiled\")\n")
(show "edited page" (reload '(pages hello)))
(show "importer after page" (value '(pages shown) 'shown))
(show "the program's own import" shown)
EOF
cat > compile-main.sps <<'EOF'
(import (chezscheme) (chez-lang loader))
(install-loader!)
(compile-program "main.sps")
EOF
run compile-main.sps > /dev/null
check "reload from a compiled program" "compiling lib/pages/hello.sls with output to lib/pages/hello.so
compiling lib/pages/shown.sls with output to lib/pages/shown.so
edited page: ((pages hello) (pages shown))
importer after page: (shown ((\"Ho, \" \"compiled\")))
the program's own import: (shown ((\"Ho, \" \"fixed\")))" "$(run main.so)"

echo "reload-test: $failures failures"
[ "$failures" -eq 0 ]
