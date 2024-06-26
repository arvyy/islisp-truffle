export ISLISP="sh islisp.sh"
SBCL_NAME="$(sbcl --noinform --non-interactive --eval '(format t "~A ~A~%" (lisp-implementation-type) (lisp-implementation-version))')"
ISLISP_NAME="ISLISP $(git rev-parse --short HEAD)"

function run_benchmark() {
  #hyperfine "sbcl --script src/$1/cl.lisp" "\$ISLISP src/$1/islisp.lisp" \
  #  --runs 3 \
  #  --export-json results/$1.json \
  #  -n "$SBCL_NAME" \
  #  -n "$ISLISP_NAME"

  node svggenerator/dist/index.js results/$1.json ../docs/images/$1.svg
}

run_benchmark fib
run_benchmark bf
run_benchmark dyndispatch