(format-integer (standard-output) 10 2)
(format-char (standard-output) #\newline)

(format-integer (standard-output) 10 8)
(format-char (standard-output) #\newline)

(format-integer (standard-output) 10 10)
(format-char (standard-output) #\newline)

(format-integer (standard-output) 10 16)
(format-char (standard-output) #\newline)

(format-float (standard-output) 1.2)
(format-char (standard-output) #\newline)

(let ((output-stream (standard-output)))

    ;; examples taken from spec

    (format output-stream "No result")
    (format-char (standard-output) #\newline)

    (format output-stream "The result is ~A and nothing else." "meningitis")
    (format-char (standard-output) #\newline)

    (format output-stream "The result i~C" #\s)
    (format-char (standard-output) #\newline)

    (format output-stream "The results are ~S and ~S." 1 #\a)
    (format-char (standard-output) #\newline)

    (format output-stream "Binary code ~B" 150)
    (format-char (standard-output) #\newline)

    (format output-stream "permission ~O" 493)
    (format-char (standard-output) #\newline)

    (format output-stream "You ~X ~X" 2989 64206)
    (format-char (standard-output) #\newline)

    (format output-stream "This will be split into~%two lines.")
    (format-char (standard-output) #\newline)

    (format output-stream "This is a tilde: ~~")
    (format-char (standard-output) #\newline)
)
