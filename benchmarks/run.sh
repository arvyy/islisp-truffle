export ISLISP="sh islisp.sh"

export SBCL_NAME="$(sbcl --noinform --non-interactive --eval '(format t "~A ~A~%" (lisp-implementation-type) (lisp-implementation-version))')"
export ISLISP_NAME="ISLISP $(git rev-parse --short HEAD)"

hyperfine 'sbcl --script fib/cl.lisp' '$ISLISP fib/islisp.lisp' --runs 3 --show-output --export-json fib.json -n "$SBCL_NAME" -n "$ISLISP_NAME"
hyperfine 'sbcl --script bf/cl.lisp' '$ISLISP bf/islisp.lisp' --runs 3 --show-output --export-json bf.json -n "$SBCL_NAME" -n "$ISLISP_NAME"
hyperfine 'sbcl --script dyndispatch/cl.lisp' '$ISLISP dyndispatch/islisp.lisp' --runs 3 --show-output --export-json dyndispatch.json -n "$SBCL_NAME" -n "$ISLISP_NAME"
