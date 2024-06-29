export ISLISP="sh islisp.sh"

SBCL_EXE="${SBCL_EXE:-sbcl}"
CLOZURE_EXE="${CLOZURE_EXE:-lx86cl64}"

SBCL_NAME="$($SBCL_EXE --noinform --non-interactive --eval '(format t "SBCL ~A~%" (lisp-implementation-version))')"
CLOZURE_NAME="$($CLOZURE_EXE -b -Q --eval '(format t "CCL ~A~%" (lisp-implementation-version))' --eval '(cl-user::quit)')"
ISLISP_NAME="ISLISP $(git rev-parse --short HEAD)"

function run_benchmark() {
  hyperfine \
    "$ISLISP src/$1/islisp.lisp" \
    "$SBCL_EXE --script src/$1/cl.lisp" \
    "$CLOZURE_EXE -l src/$1/cl.lisp --eval '(cl-user::quit)'" \
    --runs 2 \
    --export-json results/$1.json \
    -n "$ISLISP_NAME" \
    -n "$SBCL_NAME" \
    -n "$CLOZURE_NAME"

  node svggenerator/dist/index.js results/$1.json ../docs/images/$1.svg
}

run_benchmark fib
run_benchmark bf
run_benchmark dyndispatch