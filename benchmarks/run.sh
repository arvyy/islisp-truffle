export ISLISP="sh islisp.sh"

export SBCL_NAME="$(sbcl --noinform --non-interactive --eval '(format t "~A ~A~%" (lisp-implementation-type) (lisp-implementation-version))')"
export ISLISP_NAME="ISLISP $(git rev-parse --short HEAD)"

hyperfine 'sbcl --script fib/cl.lisp' '$ISLISP fib/islisp.lisp' --show-output --export-json fib.json -n "$SBCL_NAME" -n "$ISLISP_NAME"
hyperfine 'sbcl --script bf/cl.lisp' '$ISLISP bf/islisp.lisp' --show-output --export-json bf.json -n "$SBCL_NAME" -n "$ISLISP_NAME"