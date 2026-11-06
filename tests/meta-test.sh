#!/bin/sh
set -eu

scheme=${SCHEME:-/usr/lib/csv10.4.1/ta6le/scheme}
dir=$(mktemp -d /tmp/chez-lang-meta-test.XXXXXX)
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
(import (chezscheme) (chez-lang language) (chez-lang loader))
(install-loader!)
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

cat > lib/pages/quoted.sls <<'EOF'
#!test/quote test/doc
(define (greet name) (list "Hello, " name))
(greet $reader)
^$x
EOF
quoted='(("Hello, " reader) (up x))'

expect "first import" "((pages quoted))" doc "$quoted"
[ -f lib/pages/quoted.so ] || fail "the page was not compiled"
before=$(mtime lib/pages/quoted.so)
expect "second import" "((pages quoted))" doc "$quoted"
[ "$(mtime lib/pages/quoted.so)" = "$before" ] || fail "a second import recompiled the page"

expect "unrelated library" "((pages quoted) (pages unrelated))" unrelated unrelated
for touched in lib/pages/quoted.sls lib/test/quote/lang.sls lib/test/doc/lang.sls \
    src/chez-lang/reader.sls src/chez-lang/loader.sls; do
  before=$(mtime lib/pages/quoted.so)
  sleep 1
  touch "$touched"
  expect "import after touching $touched" "((pages quoted))" doc "$quoted"
  [ "$(mtime lib/pages/quoted.so)" != "$before" ] || fail "touching $touched did not recompile the page"
done
before=$(mtime lib/pages/quoted.so)
sleep 1
touch lib/pages/unrelated.sls
expect "import after touching an unrelated library" "((pages quoted) (pages unrelated))" unrelated unrelated
[ "$(mtime lib/pages/quoted.so)" = "$before" ] || fail "touching an unrelated library recompiled the page"

cp lib/test/quote/lang.sls quote-lang.sls
sleep 1
sed "s/(list 'quote datum)/(list 'quote (list 'changed datum))/" quote-lang.sls > lib/test/quote/lang.sls
expect "import after changing the meta-language" "((pages quoted))" doc \
  '(("Hello, " (changed reader)) (up (changed x)))'
sleep 1
cp quote-lang.sls lib/test/quote/lang.sls
expect "import after restoring the meta-language" "((pages quoted))" doc "$quoted"

cat > plain.sps <<'EOF'
(import (chezscheme) (pages quoted))
(write doc)
(newline)
EOF
before=$(mtime lib/pages/quoted.so)
output=$("$scheme" -q --libdirs src:lib --program plain.sps 2>&1) || fail "plain scheme failed: $output"
[ "$output" = "$quoted" ] || fail "plain scheme: expected $quoted, got $output"
[ "$(mtime lib/pages/quoted.so)" = "$before" ] || fail "plain scheme recompiled the page"

printf '#!test/quote test/quote test/doc\n$x\n^$y\n' > lib/pages/twice-quoted.sls
expect "two meta-languages" "((pages twice-quoted))" doc '(x (up y))'

printf '#!test/quote test/lines\nline\n' > lib/pages/quoted-lines.sls
expect_error "meta-language over a parser language" "((pages quoted-lines))" doc \
  'Exception in read-language: language test/lines reads with its own parser and cannot be extended by test/quote "test/lines" at line 1, char 14 of lib/pages/quoted-lines.sls'
[ ! -e lib/pages/quoted-lines.so ] || fail "a meta-language error left an object"

printf '#!test/quote\n$x\n' > lib/pages/no-base.sls
expect_error "meta-language without a base language" "((pages no-base))" doc \
  'Exception in read-language: missing language name after meta-language test/quote "" at line 1, char 13 of lib/pages/no-base.sls'

cat > lib/pages/quoted-chez.sls <<'EOF'
#!test/quote chez-lang/chez
(library (pages quoted-chez)
  (export quoted)
  (import (rnrs))
  (define quoted $(a b)))
EOF
expect "meta-language over chez-lang/chez" "((pages quoted-chez))" quoted '(a b)'
for touched in src/chez-lang/chez/lang.sls lib/test/quote/lang.sls; do
  before=$(mtime lib/pages/quoted-chez.so)
  sleep 1
  touch "$touched"
  expect "chez-lang/chez page after touching $touched" "((pages quoted-chez))" quoted '(a b)'
  [ "$(mtime lib/pages/quoted-chez.so)" != "$before" ] \
    || fail "touching $touched did not recompile the chez-lang/chez page"
done

cat > lib/pages/chez.sls <<'EOF'
#!chez-lang/chez
(library (pages chez)
  (export chez)
  (import (rnrs))
  (define chez '(chez)))
EOF
expect "chez-lang/chez library" "((pages chez))" chez '(chez)'

printf '#!chez-lang/chez\n(library (pages primitive) (export primitive) (import (rnrs)) (define primitive #%%car))\n' \
  > lib/pages/primitive.sls
expect_error "chez-lang/chez reads in R6RS mode" "((pages primitive))" primitive \
  'Exception in reader-read: #% primitive syntax is not allowed in #!r6rs mode "#%" at line 2, char 81 of lib/pages/primitive.sls'

printf '#!chez-lang/chez\n#!chezscheme\n(library (pages chezscheme) (export primitive) (import (chezscheme)) (define primitive (#%%car (quote (1)))))\n' \
  > lib/pages/chezscheme.sls
expect "chez-lang/chez after #!chezscheme" "((pages chezscheme))" primitive 1

printf '#!chez-lang/chez\n(import (rnrs) (pages greet))\n(write (greet "chez"))\n' > chez-program.sps
printf '#!test/quote test/doc\n(import (pages greet))\n(write (greet $quoted))\n' > quoted-program.sps
cat > compile-programs.sps <<'EOF'
(import (chezscheme) (chez-lang loader))
(install-loader!)
(maybe-compile-program "chez-program.sps")
(maybe-compile-program "quoted-program.sps")
EOF
"$scheme" -q --libdirs src:lib --compile-imported-libraries --program compile-programs.sps > /dev/null 2>&1 \
  || fail "compiling the programs failed"
output=$("$scheme" -q --libdirs src:lib --program chez-program.so 2>&1) || fail "the chez-lang/chez program failed: $output"
[ "$output" = '("Hello, " "chez")' ] || fail "chez-lang/chez program: got $output"
output=$("$scheme" -q --libdirs src:lib --program quoted-program.so 2>&1) || fail "the meta-language program failed: $output"
[ "$output" = '("Hello, " quoted)' ] || fail "meta-language program: got $output"

echo "meta-test: $failures failures"
[ "$failures" -eq 0 ]
