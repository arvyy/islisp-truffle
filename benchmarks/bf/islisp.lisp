(defun char-code (c)
    (convert c <integer>))

(defun code-char (i)
    (convert i <character>))

(defun parse (input)
    (let ((n 0)
          (jump-positions (create-vector (length input) -1))
          (unclosed-jump-starts (list))
          (instructions (create-vector (length input) #\?)))
        (for ((i 0 (+ 1 i)))
             ((>= i (length input)) (list n instructions jump-positions))
          (let ((c (elt input i)))
            (if (char= #\[ c)
                (progn
                    (setf unclosed-jump-starts (cons n unclosed-jump-starts))))
            (if (char= #\] c)
                (let ((start (car unclosed-jump-starts)))
                  (setf (elt jump-positions start) n)
                  (setf (elt jump-positions n) start)
                  (setf unclosed-jump-starts (cdr unclosed-jump-starts))))
            (if (or (char= #\[ c)
                    (char= #\] c)
                    (char= #\. c)
                    (char= #\, c)
                    (char= #\+ c)
                    (char= #\- c)
                    (char= #\> c)
                    (char= #\< c))
                (progn
                   (setf (elt instructions n) c)
                   (setf n (+ 1 n))))))))

(defun interpret (source-code std-input)
  (block interpret
      (let* ((parsed (parse source-code))
             (n (elt parsed 0))
             (instructions (elt parsed 1))
             (jump-positions (elt parsed 2))

             (tape (create-vector 30000 0))
             (tape-pointer 0)
             (instruction-pointer 0)
             (std-input-pointer 0))
        (for ()
             (nil)
          (if (>= instruction-pointer n) (return-from interpret nil))
          (let ((c (elt instructions instruction-pointer)))
            (cond
              ((char= #\[ c)
               (if (= 0 (elt tape tape-pointer))
                   (setf instruction-pointer (+ 1 (elt jump-positions instruction-pointer)))
                   (setf instruction-pointer (+ 1 instruction-pointer))))
              ((char= #\] c)
               (if (= 0 (elt tape tape-pointer))
                   (setf instruction-pointer (+ 1 instruction-pointer))
                   (setf instruction-pointer (+ 1 (elt jump-positions instruction-pointer)))))
              ((char= #\+ c)
               (let ((v (+ 1 (elt tape tape-pointer))))
                 (if (>= v 256)
                     (setf v 0))
                 (setf (elt tape tape-pointer) v)
                 (setf instruction-pointer (+ 1 instruction-pointer))))
              ((char= #\- c)
               (let ((v (+ -1 (elt tape tape-pointer))))
                 (if (< v 0)
                     (setf v 255))
                 (setf (elt tape tape-pointer) v)
                 (setf instruction-pointer (+ 1 instruction-pointer))))
              ((char= #\> c)
               (progn
                 (setf tape-pointer (+ 1 tape-pointer))
                 (setf instruction-pointer (+ 1 instruction-pointer))))
              ((char= #\< c)
               (progn
                 (setf tape-pointer (+ -1 tape-pointer))
                 (setf instruction-pointer (+ 1 instruction-pointer))))
              ((char= #\. c)
               (progn
                 (format (standard-output) "~A" (code-char (elt tape tape-pointer)))
                 (setf instruction-pointer (+ 1 instruction-pointer))))
              ((char= #\, c)
               (progn
                 (setf (elt tape tape-pointer) (char-code (elt std-input std-input-pointer)))
                 (setf std-input-pointer (+ 1 std-input-pointer))
                 (setf instruction-pointer (+ 1 instruction-pointer))))))))))

(defun do-main ()

(interpret "
>>>+[[-]>>[-]++>+>+++++++[<++++>>++<-]++>>+>+>+++++[>++>++++++<<-]+>>>,<++[[>[
->>]<[>>]<<-]<[<]<+>>[>]>[<+>-[[<+>-]>]<[[[-]<]++<-[<+++++++++>[<->-]>>]>>]]<<
]<]<[[<]>[[>]>>[>>]+[<<]<[<]<+>>-]>[>]+[->>]<<<<[[<<]<[<]+<<[+>+<<-[>-->+<<-[>
+<[>>+<<-]]]>[<+>-]<]++>>-->[>]>>[>>]]<<[>>+<[[<]<]>[[<<]<[<]+[-<+>>-[<<+>++>-
[<->[<<+>>-]]]<[>+<-]>]>[>]>]>[>>]>>]<<[>>+>>+>>]<<[->>>>>>>>]<<[>.>>>>>>>]<<[
>->>>>>]<<[>,>>>]<<[>+>]<<[+<<]<]
[input a brainfuck program and its input, separated by an exclamation point.
Daniel B Cristofani (cristofdathevanetdotcom)
http://www.hevanet.com/cristofd/brainfuck/]
"

"
++++> +<+[
    >[>+>+<<-]++>>[<<+>>-]>>>[-]++>[-]+
    >>>+[[-]++++++>>>]<<<[[<++++++++<++>>-]+<.<[>----<-]<]
    <<[>>>>>[>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<]]<[>+<-]>]<<-]<<-
]
[Outputs square numbers from 0 to 100.
Daniel B Cristofani (cristofdathevanetdotcom)
http://www.hevanet.com/cristofd/brainfuck/]!
")


)

(for ((i 0 (+ i 1)))
     ((> i 1000))
  (format (standard-output) "Run ~A~%" i)
  (do-main))
