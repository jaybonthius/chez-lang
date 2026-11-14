#!/bin/sh
set -eu

scheme=${SCHEME:-/usr/lib/csv10.4.1/ta6le/scheme}
dir=$(mktemp -d /tmp/chez-lang-parts-test.XXXXXX)
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
(guard (c [(language-error? c) (display "&language-error ") (display-condition c) (newline) (exit 1)]
          [(or (error? c) (violation? c)) (display-condition c) (newline) (exit 1)])
  (write (call-with-library-lock
           (lambda () (eval `(let () (import ,@(argument 0)) ,(argument 1))))))
  (newline))
EOF

# Two libraries in one object file make a warm import loop forever; the
# deadline turns that into a failure instead of a hang.
import_output() {
  timeout 60 "$scheme" -q --libdirs src:lib --compile-imported-libraries --program import.sps "$1" "$2" 2>&1
}

import_value() {
  code=0
  output=$(import_output "$1" "$2") || code=$?
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

cat > lib/pages/post.sls <<'EOF'
#!test/doc
(define-meta title "Post")
(define-meta author ^"me")
(define instantiated (begin (display "page instantiated\n") #t))
"body"
EOF
metas='((title . "Post") (author up "me"))'
loaded="(list metas (and (member '(pages post) (library-list)) #t))"

output=$(import_output "((pages post metas))" "$loaded") || fail "first import of the part failed: $output"
[ "$(printf '%s\n' "$output" | tail -n 1)" = "($metas #f)" ] \
  || fail "first import of the part: expected ($metas #f), got $output"
case $output in
  *"page instantiated"*) fail "importing the part instantiated the page" ;;
esac
[ -f lib/pages/post/metas.so ] || fail "the part was not compiled to its own object"
[ ! -e lib/pages/post.so ] || fail "importing the part compiled the page"
part_before=$(mtime lib/pages/post/metas.so)
expect "second import of the part" "((pages post metas))" "$loaded" "($metas #f)"
[ "$(mtime lib/pages/post/metas.so)" = "$part_before" ] || fail "a second import recompiled the part"

output=$(import_output "((pages post))" doc) || fail "importing the page failed: $output"
[ "$(printf '%s\n' "$output" | tail -n 1)" = '("body")' ] || fail "the page: expected (\"body\"), got $output"
case $output in
  *"page instantiated"*) ;;
  *) fail "importing the page did not instantiate it: $output" ;;
esac
[ "$(mtime lib/pages/post/metas.so)" = "$part_before" ] || fail "importing the page recompiled the part"
expect "page and part" "((pages post) (pages post metas))" "(list doc metas)" "((\"body\") $metas)"

part_before=$(mtime lib/pages/post/metas.so)
page_before=$(mtime lib/pages/post.so)
sleep 1
touch lib/pages/post.sls
expect "part after touching the page" "((pages post metas))" metas "$metas"
[ "$(mtime lib/pages/post/metas.so)" != "$part_before" ] || fail "touching the page did not recompile the part"
[ "$(mtime lib/pages/post.so)" = "$page_before" ] || fail "importing the part recompiled the page"
expect "page after touching the page" "((pages post))" doc '("body")'
[ "$(mtime lib/pages/post.so)" != "$page_before" ] || fail "touching the page did not recompile the page"
part_before=$(mtime lib/pages/post/metas.so)
page_before=$(mtime lib/pages/post.so)
expect "page and part with nothing changed" "((pages post) (pages post metas))" "(list doc metas)" \
  "((\"body\") $metas)"
[ "$(mtime lib/pages/post/metas.so)" = "$part_before" ] || fail "the part was recompiled with nothing changed"
[ "$(mtime lib/pages/post.so)" = "$page_before" ] || fail "the page was recompiled with nothing changed"

for touched in lib/test/doc/lang.sls src/lang/reader.sls src/lang/loader.sls; do
  before=$(mtime lib/pages/post/metas.so)
  sleep 1
  touch "$touched"
  expect "part after touching $touched" "((pages post metas))" metas "$metas"
  [ "$(mtime lib/pages/post/metas.so)" != "$before" ] || fail "touching $touched did not recompile the part"
done
expect "part and an unrelated library" "((pages post metas) (pages unrelated))" unrelated unrelated
for touched in lib/pages/unrelated.sls lib/test/doc.sls; do
  before=$(mtime lib/pages/post/metas.so)
  sleep 1
  touch "$touched"
  expect "part after touching $touched" "((pages post metas) (pages unrelated))" metas "$metas"
  [ "$(mtime lib/pages/post/metas.so)" = "$before" ] || fail "touching $touched recompiled the part"
done

sleep 1
printf '#!test/doc\n(define-meta title "Changed")\n' > lib/pages/post.sls
expect "part after changing the page" "((pages post metas))" metas '((title . "Changed"))'

cat > requirements.sps <<'EOF'
(import (chezscheme) (lang language) (lang loader))
(install-loader!)
(define part '(pages post metas))
(let ([imports (call-with-library-lock
                 (lambda ()
                   (environment part)
                   (library-requirements part (library-requirements-options import))))])
  (write (map (lambda (name) (and (member name imports) #t))
              '((test doc lang) (lang reader) (lang loader) (pages post) (test doc)))))
EOF
output=$(timeout 60 "$scheme" -q --libdirs src:lib --compile-imported-libraries --program requirements.sps 2>&1 | tail -n 1)
[ "$output" = '(#t #t #t #f #f)' ] || fail "the part's imports: got $output"

cat > lib/pages/self.sls <<'EOF'
#!test/doc
(import (pages self metas))
(define-meta title "Self")
metas
EOF
expect "page importing its own part" "((pages self))" doc '(((title . "Self")))'

cat > plain.sps <<'EOF'
(import (chezscheme) (pages self) (prefix (pages post metas) post:))
(write (list doc post:metas (compile-imported-libraries)))
(newline)
EOF
part_before=$(mtime lib/pages/post/metas.so)
self_before=$(mtime lib/pages/self.so)
output=$(timeout 60 "$scheme" -q --libdirs src:lib --program plain.sps 2>&1) || fail "plain scheme failed: $output"
[ "$output" = '((((title . "Self"))) ((title . "Changed")) #f)' ] || fail "plain scheme: got $output"
[ "$(mtime lib/pages/post/metas.so)" = "$part_before" ] || fail "plain scheme recompiled the part"
[ "$(mtime lib/pages/self.so)" = "$self_before" ] || fail "plain scheme recompiled the page"

cat > lib/pages/post/other.sls <<'EOF'
(library (pages post other)
  (export other)
  (import (chezscheme))
  (define other 'other))
EOF
expect "library beside a page's parts" "((pages post other))" other other
cat > lib/pages/post/metas.sls <<'EOF'
(library (pages post metas)
  (export metas)
  (import (chezscheme))
  (define metas 'library))
EOF
expect "library that takes a part's name" "((pages post metas))" metas library
sleep 1
touch lib/pages/post.sls
expect_error "page with a part whose name a library takes" "((pages post))" doc \
  '&language-error Exception in compile-lang-library: library (pages post metas) is both a part of lib/pages/post.sls and the library lib/pages/post/metas.sls'
rm lib/pages/post/metas.sls
sleep 1
touch lib/pages/post.sls
expect "part after the library is removed" "((pages post metas))" metas '((title . "Changed"))'
expect "page after the library is removed" "((pages post))" doc '()'

sleep 1
touch lib/pages/post/other.sls
expect "library beside a page, compiled after the page" "((pages post other))" other other
rm lib/pages/post/other.sls
expect "object-only library beside an older page" "((pages post other))" other other
sleep 1
touch lib/pages/post.sls
expect_error "object-only library beside a newer page is taken as a part" "((pages post other))" other \
  '&language-error Exception in compile-lang-library: language test/doc has no part other'
cp lib/pages/post.sls post.sls.saved
printf '#!test/nope\n1\n' > lib/pages/post.sls
cat > lib/pages/post/other.sls <<'EOF'
(library (pages post other)
  (export other)
  (import (chezscheme))
  (define other 'other))
EOF
expect "library beside a page in an unknown language" "((pages post other))" other other
cp post.sls.saved lib/pages/post.sls

printf '#!test/quote test/doc\n(define-meta title $x)\n' > lib/pages/quoted.sls
expect "part of a page in a meta-language chain" "((pages quoted metas))" metas "((title quote x))"
before=$(mtime lib/pages/quoted/metas.so)
sleep 1
touch lib/test/quote/lang.sls
expect "part after touching the meta-language" "((pages quoted metas))" metas "((title quote x))"
[ "$(mtime lib/pages/quoted/metas.so)" != "$before" ] || fail "touching the meta-language did not recompile the part"

printf '#!test/doc\n(define-meta title "Long")\n' > lib/pages/long.chezscheme.sls
expect "part of a page with a two-part extension" "((pages long metas))" metas '((title . "Long"))'
[ -f lib/pages/long/metas.chezscheme.so ] || fail "the part of a .chezscheme.sls source has no object"
printf '#!test/doc\n(define-meta title "Note")\n' > lib/pages/note.page
expect "part of a page with an installed extension" "((pages note metas))" metas '((title . "Note"))'
[ -f lib/pages/note/metas.so ] || fail "the part of a .page source has no object"

expect_error "unknown part" "((pages post nope))" 0 \
  '&language-error Exception in compile-lang-library: language test/doc has no part nope'
[ ! -e lib/pages/post/nope.so ] || fail "an unknown part left an object"
expect_error "part of a language without parts" "((pages plain metas))" 0 \
  '&language-error Exception in compile-lang-library: language test/plain has no part metas'
[ -d lib/pages/plain ] || fail "import no longer creates the directory of an unknown part's object"
expect_error "part of an ordinary library" "((pages greet metas))" 0 \
  'Exception: library (pages greet metas) not found'
[ ! -e lib/pages/greet/metas.so ] || fail "an ordinary library was given a part object"
expect_error "part of a missing library" "((pages missing metas))" 0 \
  'Exception: library (pages missing metas) not found'

mkdir lib/test/malformed
cat > lib/test/malformed/lang.sls <<'EOF'
(library (test malformed lang)
  (export language)
  (import (chezscheme) (lang reader) (lang language))
  (define language
    (language-with-part
      (make-s-exp-language default-readtable (read-mode chez) '((test doc)) 'doc-begin)
      'bad
      (lambda (name forms) `(library ,name)))))
EOF
printf '#!test/malformed\n1\n' > lib/pages/malformed.sls
expect_error "part generator returning a malformed library" "((pages malformed bad))" 0 \
  'Exception in compile-lang-library: expected (library name (export ...) (import ...) body ...) (library (pages malformed bad))'

mkdir lib/blog
cat > lib/blog/lang.sls <<'EOF'
(library (blog lang)
  (export language)
  (import (chezscheme) (lang reader) (lang language))
  (define language (make-s-exp-language default-readtable (read-mode chez) '((test doc)) 'doc-begin)))
EOF
printf '#!blog\n"post"\n' > lib/blog.sls
expect "page beside its language library, cold" "((blog))" doc '("post")'
expect "page beside its language library, warm" "((blog))" doc '("post")'
for touched in lib/blog.sls lib/blog/lang.sls; do
  before=$(mtime lib/blog.so)
  sleep 1
  touch "$touched"
  expect "page beside its language library after touching $touched" "((blog))" doc '("post")'
  [ "$(mtime lib/blog.so)" != "$before" ] || fail "touching $touched did not recompile the page beside its language"
done

mkdir -p olib/blog2 olib/tools
cat > olib/blog2/lang.sls <<'EOF'
(library (blog2 lang)
  (export language)
  (import (chezscheme) (lang reader) (lang language))
  (define language (make-s-exp-language default-readtable (read-mode chez) '((test doc)) 'doc-begin)))
EOF
cat > olib/tools/util.sls <<'EOF'
(library (tools util)
  (export util)
  (import (chezscheme))
  (define util 'util))
EOF
cat > compile-olib.sps <<'EOF'
(import (chezscheme))
(compile-library "olib/blog2/lang.sls" "olib/blog2/lang.so")
(compile-library "olib/tools/util.sls" "olib/tools/util.so")
EOF
timeout 60 "$scheme" -q --libdirs src:lib:olib --compile-imported-libraries --program compile-olib.sps > /dev/null 2>&1 \
  || fail "compiling the second library directory failed"
rm olib/blog2/lang.sls olib/tools/util.sls
printf '#!blog2\n"post"\n' > lib/blog2.sls
printf '#!test/doc\n"tools"\n' > lib/tools.sls
olib_value() {
  timeout 60 "$scheme" -q --libdirs src:lib:olib --compile-imported-libraries --program import.sps "$1" "$2" 2>&1 | tail -n 1
}
for run in cold warm; do
  output=$(olib_value "((blog2))" doc)
  [ "$output" = '("post")' ] || fail "page whose compiled language is in another library directory, $run: got $output"
done
output=$(olib_value "((tools util))" util)
[ "$output" = util ] || fail "compiled library in another library directory beside a page: got $output"
[ ! -e lib/blog2 ] || fail "a library in another directory left a part directory"
[ ! -e lib/tools ] || fail "a library in another directory left a part directory"

echo "parts-test: $failures failures"
[ "$failures" -eq 0 ]
