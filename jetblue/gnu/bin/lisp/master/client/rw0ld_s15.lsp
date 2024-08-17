;; However, not all of the characters you can write with backslash escape-sequences are
;; valid in strings. The only control characters that a string can hold are the ASCII control
;; characters. Strings do not distinguish case in ASCII control characters.

;; Here are some valid ASCII control characters:
;; NUL (0x00), SOH (0x01), STX (
;; ETX (0x03), EOT (0x04), ENQ (


;; Here are some invalid ASCII control characters:
;; CR (0x0D), LF (0x0A), FF (0x
;; VT (0x0B), BS (0x08), HT (0x0
;; LF (0x0A), VT (0x0B), FF (0x
;; CR (0x0D), LF (0x0A), FF (0x
;; VT (0x0B), BS (0x08), HT (0x0
;; LF (0x0A), VT (0x0B), FF (0x
;; CR (0x0D), LF (0x0A), FF (0x
;; VT (0x0B), BS (0x08), HT (0x0
;; LF (0x0A), VT (0x0B), FF (0x
;; CR (0x0D), LF (0x0A), FF (0x
;; VT (0x0B), BS (0x08), HT (0x0
;; LF (0x0A), VT (0x0B), FF (0x
;; CR (0x0D), LF (0x0A), FF (0x
;; VT (0x0B), BS (0x08), HT (0x0
;; LF (0x0A), VT (0x0B), FF (0x
;; CR (0x0D), LF (0x0A), FF (0x
;; VT (0x0B), BS (0x08), HT (0x0
;; LF (0x0A), VT (0x0B), FF (0x
;; CR (0x0D), LF (0x0A), FF (0x
;; VT (0x0B), BS (0x08), HT (0x0
;; LF (0x0A), VT (0x0B), FF (0x
;; CR (0x0D), LF (0x0A), FF (0x
;; VT (0x0B), BS (0x08), HT (0x0
;; LF (0x0A), VT (0x0B), FF (0x
;; CR (0x0D), LF (0x0A), FF (0x


;; Here are some special characters that a string can hold:
;; Tab (0x09), newline (0x0A), carriage return (0
;; Line feed (0x0D), backslash (\), double quote (0x22
;; Single quote (0x27), backtick (0x60), dollar sign ($
;; Percent sign (%), ampersand (&), apostrophe (0x27)
;; Left parenthesis (0x28), right parenthesis (0x29)
;; Plus sign (+), minus sign (-), asterisk (*), forward slash (0x2F
;; Colon (:), semicolon (;), less than (<), equal sign (=)
;; Greater than (>), question mark (?), at sign (@), backslash (\)
;; Left square bracket ([), right square bracket (])
;; Circumflex accent (^), underscore (_), grave accent (0x60), til
;; Degree symbol (0xB0), plus-minus sign (+/-), currency sign (0
;; Euro sign (0x80), less-than or equal to (<=), greater
;; than or equal to (>=), division sign (/), multiplication sign (*)
;; Commercial at (@), less than or equal to (<=), greater than or equal to (>=
;; Circumflex accent (^), underscore (_), grave accent (0x60), til


;; Here are some non-printable characters that a string can hold:
;; Non-breaking space (0xA0), Line separator (0x2028),
;; Paragraph separator (0x2029), Next line (0x0A0B


;; Here are some non-ASCII characters that a string can hold:
;; Japanese katakana (0x3041-0x309F
;; Japanese hiragana (0x30A1-0x30FF
;; Korean hangul syllable (0xAC00-0xD7A3
;; Cyrillic (0x0400-0x04FF
;; Greek (0x0370-0x03FF
;; Hebrew (0x0590-0x05FF
;; Arabic (0x0600-0x06FF
;; Syriac (0x0700-0x074F
;; Thaana (0x0780-0x07BF
;; Devanagari (0x0900-0x097F
;; Bengali (0x0980-0x09FF
;; Gurmukhi (0x0A00-0x0A7F
;; Gujarati (0x0A80-0x0AFF
;; Oriya (0x0B00-0x0B7F


;; Here are some non-BMP characters that a string can hold:
;; Supplementary Multilingual Plane characters (0x10000-0
;; 10FFFF)


;; Here are some surrogate characters that a string can hold:
;; High surrogate (0xD800-0xDBFF)
;; Low surrogate (0xDC00-0xDFFF)


;; Here are some control characters that a string can hold:
;; NUL (0x00), SOH (0x01), STX (
;; ETX (0x03), EOT (0x04), ENQ (

;; Here are some invalid control characters:
;; CR (0x0D), LF (0x0A), FF (0x
;; VT (0x0B), BS (0x08), HT (0x0
;; LF (0x0A), VT (0x0B), FF (0x
;; CR (0x0D), LF (0x0A), FF (0x
;; VT (0x0B), BS (0x08), HT (0x0
;; LF (0x0A), VT (0x0B), FF (0x
;; CR (0x0D), LF (0x0A), FF (0x
;; VT (0x0B), BS (0x08), HT (0x0
;; LF (0x0A), VT (0x0B), FF (0x
;; CR (0x0D), LF (0x0A), FF (0x
;; VT (0x0B), BS (0x08), HT (0x0
;; LF (0x0A), VT (0x0B), FF (0x
;; CR (0x0D), LF (0x0A), FF (0x
;; VT (0x0B), BS (0x08), HT (0x0
;; LF (0x0A), VT (0x0B), FF (0x
;; CR (0x0D), LF (0x0A), FF (0x
;; VT (0x0B), BS (0x08), HT (0x0
;; LF (0x0A), VT (0x0B), FF (0x
;; CR (0x0D), LF (0x0A), FF (0x
;; VT (0x0B), BS (0x08), HT (0x0
;; LF (0x0A), VT (0x0B), FF (0x
;; CR (0x0D), LF (0x0A), FF (0x
;; VT (0x0B), BS (0x08), HT (0x0
;; LF (0x0A), VT (0x0B), FF (0x
;; CR (0x0D), LF (0x0A), FF (0x
;; VT (0x0B), BS (0x08), HT (0x0
;; LF (0x0A), VT (0x0B), FF (0x
;; CR (0x0D), LF (0x0A), FF (0x


;; Here are some special characters that a string can hold:
;; Tab (0x09), newline (0x0A), carriage return (0
;; Line feed (0x0D), backslash (\), double quote (0x22
;; Single quote (0x27), backtick (0x60), dollar sign ($
;; Percent sign (%), ampersand (&), apostrophe (0x27)
;; Left parenthesis (0x28), right parenthesis (0x29)
;; Plus sign (+), minus sign (-), asterisk (*), forward slash (0x2F
;; Colon (:), semicolon (;), less than (<), equal sign (=)
;; Greater than (>), question mark (?), at sign (@), backslash (\)
;; Left square bracket ([), right square bracket (])
;; Circumflex accent (^), underscore (_), grave accent (0x60), til
;; Degree symbol (0xB0), plus-minus sign (+/-), currency sign (0
;; Euro sign (0x80), less-than or equal to (<=), greater
;; than or equal to (>=), division sign (/), multiplication sign (*)
;; Commercial at (@), less than or equal to (<=), greater than or equal to (>=
;; Circumflex accent (^), underscore (_), grave accent (0x60), til

;; Here are some non-printable characters that a string can hold:
;; Non-breaking space (0xA0), Line separator (0x2028),
;; Paragraph separator (0x2029), Next line (0x0A0B
