#!/bin/sh
set -eu

scheme=${SCHEME:-/usr/lib/csv10.4.1/ta6le/scheme}
runs=20
root=$(pwd)
dir=
trap 'if [ -n "$dir" ]; then rm -rf "$dir"; fi' EXIT

failures=0
run=1
while [ "$run" -le "$runs" ]; do
  dir=$(mktemp -d /tmp/chez-lang-threads-test.XXXXXX)
  cp -R "$root/src" "$dir/src"
  cp -R "$root/tests/lib" "$dir/lib"
  for page in t1 t2 t3 t4; do
    printf '#!test/doc\n"%s"\n' "$page" > "$dir/lib/pages/$page.sls"
  done
  cat > "$dir/threads.sps" <<'EOF'
(import (chezscheme) (lang language) (lang loader))
(install-loader!)

(define pages '((pages t1) (pages t2) (pages t3) (pages t4)))
(define mutex (make-mutex))
(define finished (make-condition))
(define results '())

(define (import-page page)
  (guard (c [#t (call-with-string-output-port
                  (lambda (p) (if (condition? c) (display-condition c p) (write c p))))])
    (call-with-library-lock
      (lambda () (eval `(let () (import ,page) doc))))))

(for-each
  (lambda (page)
    (fork-thread
      (lambda ()
        (let ([result (import-page page)])
          (with-mutex mutex
            (set! results (cons (cons page result) results))
            (condition-signal finished))))))
  pages)

(with-mutex mutex
  (let loop ()
    (unless (= (length results) (length pages))
      (condition-wait finished mutex)
      (loop))))

(define (expected page) (list (symbol->string (cadr page))))
(for-each
  (lambda (result)
    (unless (equal? (cdr result) (expected (car result)))
      (printf "~s: ~a\n" (car result) (cdr result))))
  results)
(exit (if (for-all (lambda (result) (equal? (cdr result) (expected (car result)))) results) 0 1))
EOF
  for phase in fresh touched; do
    if [ "$phase" = touched ]; then
      sleep 1
      touch "$dir/lib/test/doc.sls"
    fi
    if ! output=$(cd "$dir" && "$scheme" -q --libdirs src:lib --compile-imported-libraries --program threads.sps 2>&1); then
      echo "FAIL run $run ($phase):"
      printf '%s\n' "$output" | grep -v '^compiling '
      failures=$((failures + 1))
    fi
  done
  rm -rf "$dir"
  dir=
  run=$((run + 1))
done

echo "threads-test: $failures failures in $runs runs"
[ "$failures" -eq 0 ]
