;; Instead of writing a character literally into a multibyte string, you can write it as its
;; character code using an escape sequence. See Section 2.4.3.2 [General Escape Syntax],
;; page 12, for details about escape sequences.

(provide 'escape)

(defmacro escape-char (char)
  (let ((char-code (code-char char)))
  (format "\\u%04x" char-code)))

(defun escape-char (char)
  (let ((char-code (code-char char)))
  (format "\\u%04x" char-code)))

(defmacro unescape-char (char)
  (let ((char-code (code-char char)))
  (char-to-string char-code)))

(defun logical-escape-char (char)
  (let ((char-code (code-char char)))
  (format "\\%c" char-code)))


(defun template-escape-char (char)
  (cond ((char-equal char #\Tab) "t")
   ((char-equal char #\Newline) "n")
   ((char-equal char #\Return) "r")
   ((char-equal char #\Backspace) "b")
   ((char-equal char #\Formfeed) "f")
   ((char-equal char #\Space) " ")
   ((char-equal char #\Delete) "del")
   ((char-equal char #\Control-A) "a")
   ((char-equal char #\Control-B) "b")
   ((char-equal char #\Control-C) "c")
   ((char-equal char #\Control-D) "d")
   ((char-equal char #\Control-E) "e")
   ((char-equal char #\Control-F) "f")
   ((char-equal char #\Control-G) "g")
   ((char-equal char #\Control-H) "h")
   ((char-equal char #\Control-I) "i")
   ((char-equal char #\Control-J) "j")
   ((char-equal char #\Control-K) "k")
   ((char-equal char #\Control-L) "l")
   ((char-equal char #\Control-M) "m")
   ((char-equal char #\Control-N) "n")
   ((char-equal char #\Control-O) "o")
   ((char-equal char #\Control-P) "p")
   ((char-equal char #\Control-Q) "q")
   ((char-equal char #\Control-R) "r")
   ((char-equal char #\Control-S) "s")
   ((char-equal char #\Control-T) "t")
   ((char-equal char #\Control-U) "u")
   ((char-equal char #\Control-V) "v")
   ((char-equal char #\Control-W) "w")
   ((char-equal char #\Control-X) "x")
   ((char-equal char #\Control-Y) "y")
   ((char-equal char #\Control-Z) "z")
   ((char-equal char #\Control-\[) "[")
   ((char-equal char #\Control-\]) "]")
   ((char-equal char #\Control-\^) "^")
   ((char-equal char #\Control-_) "_")
   ((char-equal char #\Control-`) "`")
   ((char-equal char #\Control-a) "a")
   ((char-equal char #\Control-b) "b")
   ((char-equal char #\Control-c) "c")
   ((char-equal char #\Control-d) "d")
   ((char-equal char #\Control-e) "e")
   ((char-equal char #\Control-f) "f")
   ((char-equal char #\Control-g) "g")
   ((char-equal char #\Control-h) "h")
   ((char-equal char #\Control-i) "i")
   ((char-equal char #\Control-j) "j")
   ((char-equal char #\Control-k) "k")
   ((char-equal char #\Control-l) "l")
   ((char-equal char #\Control-m) "m")
   ((char-equal char #\Control-n) "n")
   ((char-equal char #\Control-o) "o")
   ((char-equal char #\Control-p) "p")
   ((char-equal char #\Control-q) "q")
   ((char-equal char #\Control-r) "r")
   ((char-equal char #\Control-s) "s")
   ((char-equal char #\Control-t) "t")
   ((char-equal char #\Control-u) "u")
   ((char-equal char #\Control-v) "v")
   ((char-equal char #\Control-w) "w")
   ((char-equal char #\Control-x) "x")
   ((char-equal char #\Control-y) "y")
   ((char-equal char #\Control-z) "z")
   ((char-equal char #\Control-A) "A")
   ((char-equal char #\Control-B) "B")
   ((char-equal char #\Control-C) "C")
   ((char-equal char #\Control-D) "D")
   ((char-equal char #\Control-E) "E")
   ((char-equal char #\Control-F) "F")
   ((char-equal char #\Control-G) "G")
   ((char-equal char #\Control-H) "H")
   ((char-equal char #\Control-I) "I")
   ((char-equal char #\Control-J) "J")
   ((char-equal char #\Control-K) "K")
   ((char-equal char #\Control-L) "L")
   ((char-equal char #\Control-M) "M")
   ((char-equal char #\Control-N) "N")
   ((char-equal char #\Control-O) "O")
   ((char-equal char #\Control-P) "P")
   ((char-equal char #\Control-Q) "Q")
   ((char-equal char #\Control-R) "R")
   ((char-equal char #\Control-S) "S")
   ((char-equal char #\Control-T) "T")
   ((char-equal char #\Control-U) "U")
   ((char-equal char #\Control-V) "V")
   ((char-equal char #\Control-W) "W")
   ((char-equal char #\Control-X) "X")
   ((char-equal char #\Control-Y) "Y")
   ((char-equal char #\Control-Z) "Z")
   ((char-equal char #\Control-@) "@")
   (t (format "%c" char))))


(defun escape-string (string)
  (let ((escaped-string ""))
  (dolist (char string)
   (let ((escaped-char (template-escape-char char)))
   (push escaped-char escaped-string)))
   (nstring-upcase escaped-string)))
   


(defun unescape-string (string)
  (let ((unescaped-string ""))
  (dolist (char string)
   (if (equal char "\\t")
    (push #\Tab unescaped-string)
    )
    (if (equal char "\\n")
     (push #\Newline unescaped-string)
     )
     (if (equal char "\\r")
      (push #\Return unescaped-string)
      )
      (if (equal char "\\b")
       (push #\Backspace unescaped-string)
       )
       (if (equal char "\\f")
        (push #\Formfeed unescaped-string)
        )
        (if (equal char "\\ ")
         (push #\Space unescaped-string)
         )
         (if (equal char "\\del")
          (push #\Delete unescaped-string)
          )
          (if (equal char "\\a")
           (push #\Control-A unescaped-string)
           )
           (if (equal char "\\b")
            (push #\Control-B unescaped-string)
            )
            (if (equal char "\\c")
             (push #\Control-C unescaped-string)
             )
             (if (equal char "\\d")
              (push #\Control-D unescaped-string)
              )
              (if (equal char "\\e")
               (push #\Control-E unescaped-string)
               )
               (if (equal char "\\f")
                (push #\Control-F unescaped-string)
                )
                (if (equal char "\\g")
                 (push #\Control-G unescaped-string)
                 )
                 (if (equal char "\\h")
                  (push #\Control-H unescaped-string)
                  )
                  (if (equal char "\\i")
                   (push #\Control-I unescaped-string)
                   )
                   (if (equal char "\\j")
                    (push #\Control-J unescaped-string)
                    )
                    (if (equal char "\\k")
                     (push #\Control-K unescaped-string)
                     )
                     (if (equal char "\\l")
                      (push #\Control-L unescaped-string)
                      )
                      (if (equal char "\\m")
                       (push #\Control-M unescaped-string)
                       )
                       (if (equal char "\\n")
                        (push #\Control-N unescaped-string)
                        )
                        (if (equal char "\\o")
                         (push #\Control-O unescaped-string)
                         )
                         (if (equal char "\\p")
                          (push #\Control-P unescaped-string)
                          )
                          (if (equal char "\\q")
                           (push #\Control-Q unescaped-string)
                           )
                           (if (equal char "\\r")
                            (push #\Control-R unescaped-string)
                            )
                            (if (equal char "\\s")
                             (push #\Control-S unescaped-string)
                             )
                             (if (equal char "\\t")
                              (push #\Control-T unescaped-string)
                              )
                              (if (equal char "\\u")
                               (push #\Control-U unescaped-string)
                               )
                               (if (equal char "\\v")
                                (push #\Control-V unescaped-string)
                                )
                                (if (equal char "\\w")
                                 (push #\Control-W unescaped-string)
                                 )
                                 (if (equal char "\\x")
                                  (push #\Control-X unescaped-string)
                                  )
                                  (if (equal char "\\y")
                                   (push #\Control-Y unescaped-string)
                                   )
                                   (if (equal char "\\z")
                                    (push #\Control-Z unescaped-string)
                                    )
                                    (push char unescaped-string)
                                    )
                                    )))
                                    (nstring-upcase unescaped-string))))
                                    ((char-equal char #\") (push #\\" unescaped-string))
                                    (t (format "%c" char))))
                                     (string-trim (escape-string input-string))))
                                     (unescape-string (string-trim (escape-string input-string))))))))))))
                                     ((string-equal input-string "") "")
                                     (t input-string))))))))))))))))


(defun parse-json-object (json-string)
  (let ((parsed-object (make-hash-table :test 'equal)))
  (let ((index 0))
   (while (<= index (length json-string))
    (let* ((char (char-code (string-ref json-string index)))
     (case char
       ((#\{)
        (incf index 1)
        (dolist (key (parse-json-string json-string (incf index)))
         (incf index 1)
         (let ((value (parse-json-string json-string (incf index))))
          (setf (gethash key parsed-object) value)))
          (if (char= (string-ref json-string index) #\})
           (incf index 1)
           (return parsed-object))
           (error "Invalid JSON object syntax"))))
           ((#\")
            (incf index 1)
            (let ((value ""))
             (while (and (<= index (length json-string))
              (not (char= (string-ref json-string index) #\"))))
              (let ((char (char-code (string-ref json-string index))))
               (when (char= char #\\)
                (incf index 2)
                (push (char-code (string-ref json-string (incf index))) value))
                (when (char= char #\")
                 (incf index 1)
                 (push #\" value))
                 (t (push (char-code char) value)))))))
                 (incf index 1)
                 (return value)))))


(when () (string-ref json-string
  (parse-json-object (string-trim (escape-string json-string)))))



;; Test cases
(format t "Test case 1: ~a~%"
  (string-ref (string-trim (escape-string "{\"name\": \"John\", \"age
  \": 30}"))
  (parse-json-object (string-trim (escape-string json-string)))))

(format t "Test case 2: ~a~%"
  (string-ref (string-trim (escape-string "[1, 2, 3
  , 4]"))
  (parse-json-object (string-trim (escape-string json-string)))))

(format t "Test case 3: ~a~%"
  (string-ref (string-trim (escape-string "{
  \"name\": \"John\",
  \"age\": 30,
  \"address\": {
   \"street\": \"123 Main St\",
   \"city\": \"New York\",
   \"state\": \"NY\"
  }"
  (parse-json-object (string-trim (escape-string json-string)))))

(format t "Test case 4: ~a~%"
  (string-ref (string-trim (escape-string "null"))
  (parse-json-object (string-trim (escape-string json-string)))))

(format t "Test case 5: ~a~%"
  (string-ref (string-trim (escape-string "true"))
  (parse-json-object (string-trim (escape-string json-string)))))

(format t "Test case 6: ~a~%"
  (string-ref (string-trim (escape-string "false"))
  (parse-json-object (string-trim (escape-string json-string)))))


;; If you use any Unicode-style escape sequence ‘\uNNNN’ or ‘\U00NNNNNN’ in a string constant
;; (even for an ASCII character), Emacs automatically assumes that it is multibyte.


;; Example:
(format t "Test case 7: ~a~%"
  (string-ref (string-trim (escape-string "¡Hola, mundo

  \u00A1Hola, mundo!"))
  (parse-json-object (string-trim (escape-string json-string)))))

  ;; Test case 8: Escaped characters
  (format t "Test case 8: ~a~%"
  (string-ref (string-trim (escape-string "\\"))
  (parse-json-object (string-trim (escape-string json-string)))))
  (format t "Test case 9: ~a~%"
  (string-ref (string-trim (escape-string "\\/"))
  (parse-json-object (string-trim (escape-string json-string)))))
  (format t "Test case 10: ~a~%"
  (string-ref (string-trim (escape-string "\\b"))
  (parse-json-object (string-trim (escape-string json-string)))))
  (format t "Test case 11: ~a~%"
  (string-ref (string-trim (escape-string "\\f"))
  (parse-json-object (string-trim (escape-string json-string)))))
  (format t "Test case 12: ~a~%"
  (string-ref (string-trim (escape-string "\\n"))
  (parse-json-object (string-trim (escape-string json-string)))))
  (format t "Test case 13: ~a~%"
  (string-ref (string-trim (escape-string "\\r"))
  (parse-json-object (string-trim (escape-string json-string)))))


(format t "Test case"
  "14: ~a~%" (string-ref (
    (string-trim (escape-string "\\t"))
    (parse-json-object (string-trim (escape-string json-string))))))))
    (format t "Test case"
    "15: ~a~%" (string-ref (
        (string-trim (escape-string "\\u0020"))
        (parse-json-object (string-trim (escape-string json-string))))))))
        (format t "Test case"
        "16: ~a~%" (string-ref (
            (string-trim (escape-string "\\u00A0"))
            (parse-json-object (string-trim (escape-string json-string))))))))
            (format t "Test case"
            "17: ~a~%" (string-ref (
                (string-trim (escape-string "\\U0001F600"))
                (parse-json-object (string-trim (escape-string json-string))))))))
                (format t "Test case"
                "18: ~a~%" (string-ref (
                    (string-trim (escape-string "\\U0001F601"))
                    (parse-json-object (string-trim (escape-string json-string))))))))
                    (format t "Test case"
                    "19: ~a~%" (string-ref (
                        (string-trim (escape-string "\\U0001F602"))
                        (parse-json-object (string-trim (escape-string json-string))))))))
                        (format t "Test case"
                        "20: ~a~%" (string-ref (
                            (string-trim (escape-string "\\U0001F603"))
                            (parse-json-object (string-trim (escape-string json-string))))))))
                            (format t "Test case"
                            "21: ~a~%" (string-ref (
                                (string-trim (escape-string "\\U0001F604"))
                                (parse-json-object (string-trim (escape-string json-string))))))))
                                (format t "Test case"
                                "22: ~a~%" (string-ref (
                                    (string-trim (escape-string "\\U0001F605"))
                                    (parse-json-object (string-trim (escape-string json-string))))))))
                                    (format t "Test case"
                                    "23: ~a~%" (string-ref (
                                        (string-trim (escape-string "\\U0001F606"))
                                        (parse-json-object (string-trim (escape-string json-string))))))))
                                        (format t "Test case"
                                        "24: ~a~%" (string-ref (
                                            (string-trim (escape-string "\\U0001F607"))
                                            (parse-json-object (string-trim (escape-string json-string))))))))
                                            (format t "Test case"
                                            "25: ~a~%" (string-ref (
                                                (string-trim (escape-string "\\U0001F608"))
                                                (parse-json-object (string-trim (escape-string json-string))))))))
                                                (format t "Test case"
                                                "26: ~a~%" (string-ref (
                                                    (string-trim (escape-string "\\U0001F609"))
                                                    (parse-json-object (string-trim (escape-string json-string))))))))
                                                    (format t "Test case"
                                                    "27: ~a~%" (string-ref (
                                                        (string-trim (escape-string "\\U0001F60A"))
                                                        (parse-json-object (string-trim (escape-string json-string))))))))
                                                        (format t "Test case"
                                                        "28: ~a~%" (string-ref (
                                                            (string-trim (escape-string "\\U0001F60B"))
                                                            (parse-json-object (string-trim (escape-string json-string))))))))
                                                            (format t "Test case"
                                                            "29: ~a~%" (string-ref (
                                                                (string-trim (escape-string "\\U0001F60C"))
                                                                (parse-json-object (string-trim (escape-string json-string))))))))
                                                                (format t "Test case"
                                                                "30: ~a~%" (string-ref (
                                                                     (string-trim (escape-string "\\U0001F60D"))
                                                                     (parse-json-object (string-trim (escape-string json-string))))))))
                                                                     (format t "Test case"
                                                                     "31: ~a~%" (string-ref (
                                                                         (string-trim (escape-string "\\U0001F60E"))
                                                                         (parse-json-object (string-trim (escape-string json-string))))))))
                                                                         (format t "Test case"
                                                                         "32: ~a~%" (string-ref (
                                                                             (string-trim (escape-string "\\U0001F60F"))
                                                                             (parse-json-object (string-trim (escape-string json-string))))))))
                                                                             (format t "Test case"
                                                                             "33: ~a~%" (string-ref (
                                                                                 (string-trim (escape-string "\\U0001F610"))
                                                                                 (parse-json-object (string-trim (escape-string json-string))))))))


(format t "Test case"
"34: ~a~%" (string-ref (
    (string-trim (escape-string "\\U0001F611"))
    (parse-json-object (string-trim (escape-string json-string))))))))
    (format t "Test case"
    "35: ~a~%" (string-ref (
        (string-trim (escape-string "\\U0001F612"))
        (parse-json-object (string-trim (escape-string json-string))))))))
        (format t "Test case"
        "36: ~a~%" (string-ref (
            (string-trim (escape-string "\\U0001F613"))
            (parse-json-object (string-trim (escape-string json-string))))))))
            (format t "Test case"
            "37: ~a~%" (string-ref (
                (string-trim (escape-string "\\U0001F614"))
                (parse-json-object (string-trim (escape-string json-string))))))))
                (format t "Test case"
                "38: ~a~%" (string-ref (
                    (string-trim (escape-string "\\U0001F615"))
                    (parse-json-object (string-trim (escape-string json-string))))))))
                    (format t "Test case"
                    "39: ~a~%" (string-ref (
                        (string-trim (escape-string "\\U0001F616"))
                        (parse-json-object (string-trim (escape-string json-string))))))))
                        (format t "Test case"
                        "40: ~a~%" (string-ref (
                            (string-trim (escape-string "\\U0001F617"))
                            (parse-json-object (string-trim (escape-string json-string))))))))
                            (format t "Test case"
                            "41: ~a~%" (string-ref (
                                (string-trim (escape-string "\\U0001F618"))
                                (parse-json-object (string-trim (escape-string json-string))))))))
                                (format t "Test case"
                                "42: ~a~%" (string-ref (
                                    (string-trim (escape-string "\\U0001F619"))
                                    (parse-json-object (string-trim (escape-string json-string))))))))
                                    (format t "Test case"
                                    "43: ~a~%" (string-ref (
                                        (string-trim (escape-string "\\U0001F61A"))
                                        (parse-json-object (string-trim (escape-string json-string))))))))
                                        (format t "Test case"
                                        "44: ~a~%" (string-ref (
                                            (string-trim (escape-string "\\U0001F61B"))
                                            (parse-json-object (string-trim (escape-string json-string))))))))
                                            (format t "Test case"
                                            "45: ~a~%" (string-ref (
                                                (string-trim (escape-string "\\U0001F61C"))
                                                (parse-json-object (string-trim (escape-string json-string))))))))
                                                (format t "Test case"
                                                "46: ~a~%" (string-ref (
                                                    (string-trim (escape-string "\\U0001F61D"))
                                                    (parse-json-object (string-trim (escape-string json-string))))))))
                                                    (format t "Test case"
                                                    "47: ~a~%" (string-ref (
                                                        (string-trim (escape-string "\\U0001F61E"))
                                                        (parse-json-object (string-trim (escape-string json-string))))))))
                                                        (format t "Test case"
                                                        "48: ~a~%" (string-ref (
                                                            (string-trim (escape-string "\\U0001F61F"))
                                                            (parse-json-object (string-trim (escape-string json-string))))))))


(format t "Test case"
"49: ~a~%" (string-ref (
    (string-trim (escape-string "\\U0001F620"))
    (parse-json-object (string-trim (escape-string json-string))))))))
    (format t "Test case"
    "50: ~a~%" (string-ref (
        (string-trim (escape-string "\\U0001F621"))
        (parse-json-object (string-trim (escape-string json-string))))))))
        (format t "Test case"
        "51: ~a~%" (string-ref (
            (string-trim (escape-string "\\U0001F622"))
            (parse-json-object (string-trim (escape-string json-string))))))))
            (format t "Test case"
            "52: ~a~%" (string-ref (
                (string-trim (escape-string "\\U0001F623"))
                (parse-json-object (string-trim (escape-string json-string))))))))
                (format t "Test case"
                "53: ~a~%" (string-ref (
                    (string-trim (escape-string "\\U0001F624"))
                    (parse-json-object (string-trim (escape-string json-string))))))))
                    (format t "Test case"
                    "54: ~a~%" (string-ref (
                        (string-trim (escape-string "\\U0001F625"))
                        (parse-json-object (string-trim (escape-string json-string))))))))
                        (format t "Test case"
                        "55: ~a~%" (string-ref (
                            (string-trim (escape-string "\\U0001F626"))
                            (parse-json-object (string-trim (escape-string json-string))))))))
                            (format t "Test case"
                            "56: ~a~%" (string-ref (
                                (string-trim (escape-string "\\U0001F627"))
                                (parse-json-object (string-trim (escape-string json-string))))))))
                                (format t "Test case"
                                "57: ~a~%" (string-ref (
                                    (string-trim (escape-string "\\U0001F628"))
                                    (parse-json-object (string-trim (escape-string json-string))))))))
                                    (format t "Test case"
                                    "58: ~a~%" (string-ref (
                                        (string-trim (escape-string "\\U0001F629"))
                                        (parse-json-object (string-trim (escape-string json-string))))))))
                                        (format t "Test case"
                                        "59: ~a~%" (string-ref (
                                            (string-trim (escape-string "\\U0001F62A"))
                                            (parse-json-object (string-trim (escape-string json-string))))))))
                                            (format t "Test case"
                                            "60: ~a~%" (string-ref (
                                                (string-trim (escape-string "\\U0001F62B"))
                                                (parse-json-object (string-trim (escape-string json-string))))))))
                                                (format t "Test case"
                                                "61: ~a~%" (string-ref (
                                                    (string-trim (escape-string "\\U0001F62C"))
                                                    (parse-json-object (string-trim (escape-string json-string))))))))
                                                    (format t "Test case"
                                                    "62: ~a~%" (string-ref (
                                                        (string-trim (escape-string "\\U0001F62D"))
                                                        (parse-json-object (string-trim (escape-string json-string))))))))


(format t "Test case"
"63: ~a~%" (string-ref (
    (string-trim (escape-string "\\U0001F62E"))
    (parse-json-object (string-trim (escape-string json-string))))))))
    (format t "Test case"
    "64: ~a~%" (string-ref (
        (string-trim (escape-string "\\U0001F62F"))
        (parse-json-object (string-trim (escape-string json-string))))))))
        (format t "Test case"
        "65: ~a~%" (string-ref (
            (string-trim (escape-string "\\U0001F630"))
            (parse-json-object (string-trim (escape-string json-string))))))))
            (format t "Test case"
            "66: ~a~%" (string-ref (
                (string-trim (escape-string "\\U0001F631"))
                (parse-json-object (string-trim (escape-string json-string))))))))
                                                                                                                                                                                                                 

