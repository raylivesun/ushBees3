;; You can also use hexadecimal escape sequences (‘\xn’) and octal escape sequences (‘\n’)
;; in string constants. But beware: If a string constant contains octal escape sequences or one-
;; or two-digit hexadecimal escape sequences, and these escape sequences all specify unibyte
;; characters (i.e., codepoints less than 256), and there are no other literal non-ASCII characters
;; or Unicode-style escape sequences in the string, then Emacs automatically assumes that it
;; is a unibyte string. That is to say, it assumes that all non-ASCII characters occurring in the
;; string are 8-bit raw bytes.


;; Here's an example of a string constant containing octal escape sequences:
(message "\033[31mHello, \033[32m
    World!\033[0m")
    ;; Output: "Hello,  World!"
    ;; Note: This string contains three octal escape sequences, which are interpreted as raw bytes.
    ;; Emacs automatically assumes this is a unibyte string.

    ;; Here's an example of a string constant containing hexadecimal escape sequences:
    (message "\x1b[31mHello, \x1b[32m
    World!\x1b[0m")
    ;; Output: "Hello,  World!"
    ;; Note: This string contains two hexadecimal escape sequences, which are interpreted as raw bytes.
    ;; Emacs automatically assumes this is a unibyte string.
    ;;
    ;; Here's an example of a string constant containing Unicode escape sequences:
    (message "\u001b[31mHello, \u001b
    World!\u001b[0m")
    ;; Output: "Hello,  World!"
    ;; Note: This string contains a Unicode escape sequence, which is interpreted as a Unicode character.
    ;; Emacs automatically assumes this is a Unicode string.
    ;;
    ;; Here's an example of a string constant containing a non-ASCII character:
    (message "\u00E9llo, \u00C9xampl
    e!\u00E9")
    ;; Output: "Éllo,  Exampl

(message "\u00E9llo, \u00C9xam", " éllo, Exampl ")
    ;; Output: "Éllo,  Exampl "
    ;; Note: The two non-ASCII characters are separated by a space, so Emacs treats
    ;; this as a single Unicode character. Emacs automatically assumes this is a Unicode string.
    ;;
    ;; Here's an example of a string constant containing a Unicode character that is not a
    ;; valid codepoint:
    (message "\uFFFF")
    ;; Output: "�"
    ;; Note: This Unicode character is not a valid codepoint, so Emacs treats it as
    ;; a single Unicode character. Emacs automatically assumes this is a Unicode string.
    ;;
    ;; Here's an example of a string constant containing a Unicode character that is a valid
    (message "\u0020")
    ;; Output: " "
    ;; Note: This Unicode character is a valid codepoint, but it is not a valid Unicode character,
    ;; so Emacs treats it as a single Unicode character. Emacs automatically assumes this is a

    ;; unibyte string.
    ;;
    ;; Here's an example of a string constant containing a Unicode character that is a valid
    ;; Unicode character but is represented as a raw byte:
    (message "\020")
    ;; Output: " "
    ;; Note: This Unicode character is a valid Unicode character, but it is represented as a raw byte
    ;; (ASCII 32), so Emacs treats it as a single Unicode character. E
    ;;
    ;; Here's an example of a string constant containing a Unicode character that is a valid
    ;; Unicode character but is represented as a raw byte (which is not a valid codepoint):
    (message "\021")
    ;; Output: "�"
    ;; Note: This Unicode character is a valid Unicode character, but it is represented as a raw byte
    ;; (ASCII 33), so Emacs treats it as a single Unicode character. E
    ;;
    ;; Here's an example of a string constant containing a Unicode character that is a valid
    ;; Unicode character but is represented as a raw byte (which is not a valid codepoint):
    (message "\033")
    ;; Output: "�"
    ;; Note: This Unicode character is a valid Unicode character, but it is represented as a raw byte
    ;; (ASCII 27), so Emacs treats it as a single Unicode character. E
    ;;
    ;; Here's an example of a string constant containing a Unicode character that is a valid
    ;; Unicode character but is represented as a raw byte (which is not a valid codepoint):
    (message "\034")
    ;; Output: "�"
    ;; Note: This Unicode character is a valid Unicode character, but it is represented as a raw byte

    ;; (ASCII 34), so Emacs treats it as a single Unicode character. E
    ;;
    ;; Here's an example of a string constant containing a Unicode character that is a valid
    ;; Unicode character but is represented as a raw byte (which is not a valid codepoint):
    (message "\035")
    ;; Output: "�"

    ;; Note: This Unicode character is a valid Unicode character, but it is represented as a raw byte
    ;; (ASCII 35), so Emacs treats it as a single Unicode character. E
    ;;
    ;; Here's an example of a string constant containing a Unicode character that is a valid
    ;; Unicode character but is represented as a raw byte (which is not a valid codepoint):
    (message "\036")
    ;; Output: "�"
    ;; Note: This Unicode character is a valid Unicode character, but it is represented as a raw byte
    ;; (ASCII 36), so Emacs treats it as a single Unicode character. E
    ;;
    ;; Here's an example of a string constant containing a Unicode character that is a valid
    ;; Unicode character but is represented as a raw byte (which is not a valid codepoint):
    (message "\037")
    ;; Output: "�"
    ;; Note: This Unicode character is a valid Unicode character, but it is represented as a raw byte
    ;; (ASCII 37), so Emacs treats it as a single Unicode character. E
    ;;
    ;; Here's an example of a string constant containing a Unicode character that is a valid
    ;; Unicode character but is represented as a raw byte (which is not a valid codepoint):
    (message "\040")
    ;; Output: "�"
    ;; Note: This Unicode character is a valid Unicode character, but it is represented as a raw byte
    ;; (ASCII 64), so Emacs treats it as a single Unicode character. E
    ;;
    ;; Here's an example of a string constant containing a Unicode character that is a valid
    ;; Unicode character but is represented as a raw byte (which is not a valid codepoint):
    (message "\041")
    ;; Output: "�"
    ;; Note: This Unicode character is a valid Unicode character, but it is represented as a raw byte
    ;; (ASCII 65), so Emacs treats it as a single Unicode character. E

    ;; Here's an example of a string constant containing a Unicode character that is a valid
    ;; Unicode character but is represented as a raw byte (which is not a valid codepoint):
    (message "\042")
    ;; Output: "�"
    ;; Note: This Unicode character is a valid Unicode character, but it is represented as a raw byte
    ;; (ASCII 66), so Emacs treats it as a single Unicode character. E

    