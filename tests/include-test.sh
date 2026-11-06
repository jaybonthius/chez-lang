#!/bin/sh
set -eu

scheme=${SCHEME:-/usr/lib/csv10.4.1/ta6le/scheme}
dir=$(mktemp -d /tmp/chez-lang-include-test.XXXXXX)
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

cat > lib/pages/part.txt <<'EOF'
#!test/doc
(define included (helper ^"part"))
EOF
cat > lib/pages/including.sls <<'EOF'
(library (pages including)
  (export included after inner)
  (import (chezscheme) (chez-lang include) (only (test doc) up))

  (define (helper x) (list 'helper x))
  (include/lang "part.txt")
  (define after (list 'after included))
  (define (inner)
    (include/lang "part.txt")
    included))
EOF

included='(helper (up "part"))'
value="(list included after (inner))"
expected="($included (after $included) $included)"

expect "first import" "((pages including))" "$value" "$expected"
[ -f lib/pages/including.so ] || fail "the including library was not compiled"
before=$(mtime lib/pages/including.so)
expect "second import" "((pages including))" "$value" "$expected"
[ "$(mtime lib/pages/including.so)" = "$before" ] || fail "a second import recompiled the including library"

before=$(mtime lib/pages/including.so)
sleep 1
touch lib/pages/part.txt
expect "import after touching the included file" "((pages including))" "$value" "$expected"
[ "$(mtime lib/pages/including.so)" != "$before" ] \
  || fail "touching the included file did not recompile the including library: \$require-include no longer records the file"

sleep 1
printf '#!test/doc\n(define included (helper ^"changed"))\n' > lib/pages/part.txt
expect "import after changing the included file" "((pages including))" included '(helper (up "changed"))'
sleep 1
printf '#!test/doc\n(define included (helper ^"part"))\n' > lib/pages/part.txt
expect "import after restoring the included file" "((pages including))" "$value" "$expected"

for touched in lib/test/doc/lang.sls src/chez-lang/include.sls; do
  before=$(mtime lib/pages/including.so)
  sleep 1
  touch "$touched"
  expect "import after touching $touched" "((pages including))" "$value" "$expected"
  [ "$(mtime lib/pages/including.so)" != "$before" ] || fail "touching $touched did not recompile the including library"
done

expect "unrelated library" "((pages including) (pages unrelated))" unrelated unrelated
before=$(mtime lib/pages/including.so)
sleep 1
touch lib/pages/unrelated.sls
expect "import after touching an unrelated library" "((pages including) (pages unrelated))" unrelated unrelated
[ "$(mtime lib/pages/including.so)" = "$before" ] || fail "touching an unrelated library recompiled the including library"

cat > lib/pages/doc-including.sls <<'EOF'
#!test/doc
(import (chez-lang include))
(define (helper x) (list 'helper x))
(include/lang "part.txt")
"text"
included
EOF
expect "include/lang in a page whose body macro collects expressions" "((pages doc-including))" doc \
  "(\"text\" $included)"
before=$(mtime lib/pages/doc-including.so)
sleep 1
touch lib/pages/part.txt
expect "page after touching the included file" "((pages doc-including))" doc "(\"text\" $included)"
[ "$(mtime lib/pages/doc-including.so)" != "$before" ] || fail "touching the included file did not recompile the page"

printf '(library (pages bad-name) (export) (import (chezscheme) (chez-lang include)) (include/lang part))\n' \
  > lib/pages/bad-name.sls
expect_error "file name that is not a string" "((pages bad-name))" 0 \
  'Exception in include/lang: expected (include/lang "file") (include/lang part) at line 1, char 78 of lib/pages/bad-name.sls'

printf '(define x 1)\n' > lib/pages/plain.txt
printf '(library (pages no-line) (export) (import (chezscheme) (chez-lang include)) (include/lang "plain.txt"))\n' \
  > lib/pages/no-line.sls
expect_error "included file without a language line" "((pages no-line))" 0 \
  'Exception in read-language: missing language line "(d" at line 1, char 1 of lib/pages/plain.txt'

printf '(library (pages missing) (export) (import (chezscheme) (chez-lang include)) (include/lang "missing.txt"))\n' \
  > lib/pages/missing.sls
expect_error "missing included file" "((pages missing))" 0 \
  'Exception in include/lang: file "missing.txt" not found in source directories'

echo "include-test: $failures failures"
[ "$failures" -eq 0 ]
