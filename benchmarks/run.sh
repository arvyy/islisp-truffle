export ISLISP="sh islisp.sh"

hyperfine 'sbcl --script fib/cl.lisp' '$ISLISP fib/islisp.lisp' --show-output --export-json fib.json
