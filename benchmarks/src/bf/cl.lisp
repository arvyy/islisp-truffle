(defun parse (input)
    (let ((n 0)
          (jump-positions (make-array (length input) :initial-element -1))
          (unclosed-jump-starts (list))
          (instructions (make-array (length input) :initial-element #\?)))
        (do ((i 0 (+ 1 i)))
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
  (let* ((parsed (parse source-code))
         (n (elt parsed 0))
         (instructions (elt parsed 1))
         (jump-positions (elt parsed 2))

         (tape (make-array 30000 :initial-element 0))
         (tape-pointer 0)
         (instruction-pointer 0)
         (std-input-pointer 0))
    (do ()
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
             (format t "~a" (code-char (elt tape tape-pointer)))
             (setf instruction-pointer (+ 1 instruction-pointer))))
          ((char= #\, c)
           (progn
             (setf (elt tape tape-pointer) (char-code (elt std-input std-input-pointer)))
             (setf std-input-pointer (+ 1 std-input-pointer))
             (setf instruction-pointer (+ 1 instruction-pointer)))))))))


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

(do ((i 0 (+ i 1)))
    ((> i 100))
  (do-main))
