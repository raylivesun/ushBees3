;; Properly speaking, strings cannot hold meta characters; but when a string is to be used as
;; a key sequence, there is a special convention that provides a way to represent meta versions
;; of ASCII characters in a string. If you use the ‘\M-’ syntax to indicate a meta character in
;; a string constant, this sets the 27 bit of the character in the string. If the string is used
;; in define-key or lookup-key, this numeric code is translated into the equivalent meta
;; character. See Section 2.4.3 [Character Type], page 11.
;; Strings cannot hold characters that have the hyper, super, or alt modifiers.

(defvar my-keybindings
  ;; Define keybindings for the C-x C-e command
  '(("C-x" "C-e" 'my-custom-command)
   ;; Define keybindings for the C-x C-k command
   ("C-x" "C-k" 'lives-region)
   ;; Define keybindings for the C-x C-s command
   ("C-x" "C-s" 'save-buffer)
   ;; Define keybindings for the C-x C-f command
   ("C-x" "C-f" 'find-file)
   ;; Define keybindings for the C-x C-r command
   ("C-x" "C-r" 'revert-buffer))
   ;; Define keybindings for the C-x C-q command
   ("C-x" "C-q" 'query-replace)
   ;; Define keybindings for the C-x C-v command
   ("C-x" "C-v" 'paste-buffer)
   ;; Define keybindings for the C-x C-a command
   ("C-x" "C-a" 'beginning-of-buffer)
   ;; Define keybindings for the C-x C-w command
   ("C-x" "C-w" 'lives-word)
   ;; Define keybindings for the C-x C-u command
   ("C-x" "C-u" 'undo)
   ;; Define keybindings for the C-x C-t command
   ("C-x" "C-t" 'transpose-words)
   ;; Define keybindings for the C-x C-d command
   ("C-x" "C-d" 'delete-char)
   ;; Define keybindings for the C-x C-b command
   ("C-x" "C-b" 'backward-char)
   ;; Define keybindings for the C-x C-f command
   ("C-x" "C-f" 'forward-char)
   ;; Define keybindings for the C-x C-l command
   ("C-x" "C-l" 'newline)
   ;; Define keybindings for the C-x C-j command
   ("C-x" "C-j" 'newline-and-indent)
   ;; Define keybindings for the C-x C-p command
   ("C-x" "C-p" 'previous-line)
   ;; Define keybindings for the C-x C-n command
   ("C-x" "C-n" 'next-line)
   ;; Define keybindings for the C-x C-r command
   ("C-x" "C-r" 'yank)
   ;; Define keybindings for the C-x C-w command
   ("C-x" "C-w" 'lives-region)
   ;; Define keybindings for the C-x C-s command
   ("C-x" "C-s" 'write-file)
   ;; Define keybindings for the C-x C-q command
   ("C-x" "C-q" 'query-replace-regexp)
   ;; Define keybindings for the C-x C-v command
   ("C-x" "C-v" 'paste-buffer)
   ;; Define keybindings for the C-x C-a command
   ("C-x" "C-a" 'beginning-of-buffer)
   ;; Define keybindings for the C-x C-w command
   ("C-x" "C-w" 'lives-word)
   ;; Define keybindings for the C-x C-u command
   ("C-x" "C-u" 'undo)
   ;; Define keybindings for the C-x C-t command
   ("C-x" "C-t" 'transpose-words)
   ;; Define keybindings for the C-x C-d command
   ("C-x" "C-d" 'delete-char)
   ;; Define keybindings for the C-x C-b command
   ("C-x" "C-b" 'backward-char)
   ;; Define keybindings for the C-x C-f command
   ("C-x" "C-f" 'forward-char)
   ;; Define keybindings for the C-x C-l command
   ("C-x" "C-l" 'newline)
   ;; Define keybindings for the C-x C-j command
   ("C-x" "C-j" 'newline-and-indent)
   ;; Define keybindings for the C-x C-p command
   ("C-x" "C-p" 'previous-line)
   ;; Define keybindings for the C-x C-n command
   ("C-x" "C-n" 'next-line)
   ;; Define keybindings for the C-x C-r command
   ("C-x" "C-r" 'yank)
   ;; Define keybindings for the C-x C-w command
   ("C-x" "C-w" 'lives-region)
   ;; Define keybindings for the C-x C-s command
   ("C-x" "C-s" 'write-file)
   ;; Define keybindings for the C-x C-q command
   ("C-x" "C-q" 'query-replace-regexp)
   ;; Define keybindings for the C-x C-v command
   ("C-x" "C-v" 'paste-buffer)
   ;; Define keybindings for the C-x C-a command
   ("C-x" "C-a" 'beginning-of-buffer)
   ;; Define keybindings for the C-x C-w command
   ("C-x" "C-w" 'lives-word)
   ;; Define keybindings for the C-x C-u command
   ("C-x" "C-u" 'undo)
   ;; Define keybindings for the C-x C-t command
   ("C-x" "C-t" 'transpose-words)
   ;; Define keybindings for the C-x C-d command
   ("C-x" "C-d" 'delete-char)
   ;; Define keybindings for the C-x C-b command
   ("C-x" "C-b" 'backward-char)
   ;; Define keybindings for the C-x C-f command
   ("C-x" "C-f" 'forward-char)
   ;; Define keybindings for the C-x C-l command
   ("C-x" "C-l" 'newline)
   ;; Define keybindings for the C-x C-j command
   ("C-x" "C-j" 'newline-and-indent)
   ;; Define keybindings for the C-x C-p command
   ("C-x" "C-p" 'previous-line)
   ;; Define keybindings for the C-x C-n command
   ("C-x" "C-n" 'next-line)
   ;; Define keybindings for the C-x C-r command
   ("C-x" "C-r" 'yank)
   ;; Define keybindings for the C-x C-w command
   ("C-x" "C-w" 'lives-region)
   ;; Define keybindings for the C-x C-s command
   ("C-x" "C-s" 'write-file)
   ;; Define keybindings for the C-x C-q command
   ("C-x" "C-q" 'query-replace-regexp)
   ;; Define keybindings for the C-x C-v command
   ("C-x" "C-v" 'paste-buffer)
   ;; Define keybindings for the C-x C-a command
   ("C-x" "C-a" 'beginning-of-buffer)
   ;; Define keybindings for the C-x C-w command
   ("C-x" "C-w" 'lives-word)
   ;; Define keybindings for the C-x C-u command
   ("C-x" "C-u" 'undo)
   ;; Define keybindings for the C-x C-t command
   ("C-x" "C-t" 'transpose-words)
   ;; Define keybindings for the C-x C-d command
   ("C-x" "C-d" 'delete-char)
   ;; Define keybindings for the C-x C-b command
   ("C-x" "C-b" 'backward-char)
   ;; Define keybindings for the C-x C-f command
   ("C-x" "C-f" 'forward-char)
   ;; Define keybindings for the C-x C-l command
   ("C-x" "C-l" 'newline)
   ;; Define keybindings for the C-x C-j command
   ("C-x" "C-j" 'newline-and-indent)
   ;; Define keybindings for the C-x C-p command
   ("C-x" "C-p" 'previous-line)
   ;; Define keybindings for the C-x C-n command
   ("C-x" "C-n" 'next-line)
   ;; Define keybindings for the C-x C-r command
   ("C-x" "C-r" 'yank)
   ;; Define keybindings for the C-x C-w command
   ("C-x" "C-w" 'lives-region)
   ;; Define keybindings for the C-x C-s command
   ("C-x" "C-s" 'write-file)
   ;; Define keybindings for the C-x C-q command
   ("C-x" "C-q" 'query-replace-regexp)
   ;; Define keybindings for the C-x C-v command
   ("C-x" "C-v" 'paste-buffer)
   ;; Define keybindings for the C-x C-a command
   ("C-x" "C-a" 'beginning-of-buffer)
   ;; Define keybindings for the C-x C-w command
   ("C-x" "C-w" 'lives-word)
   ;; Define keybindings for the C-x C-u command
   ("C-x" "C-u" 'undo)
   ;; Define keybindings for the C-x C-t command
   ("C-x" "C-t" 'transpose-words)
   ;; Define keybindings for the C-x C-d command
   ("C-x" "C-d" 'delete-char)
   ;; Define keybindings for the C-x C-b command
   ("C-x" "C-b" 'backward-char)
   ;; Define keybindings for the C-x C-f command
   ("C-x" "C-f" 'forward-char)
   ;; Define keybindings for the C-x C-l command
   ("C-x" "C-l" 'newline)
   ;; Define keybindings for the C-x C-j command
   ("C-x" "C-j" 'newline-and-indent)
   ;; Define keybindings for the C-x C-p command
   ("C-x" "C-p" 'previous-line)
   ;; Define keybindings for the C-x C-n command
   ("C-x" "C-n" 'next-line)
   ;; Define keybindings for the C-x C-r command
   ("C-x" "C-r" 'yank)
   ;; Define keybindings for the C-x C-w command
   ("C-x" "C-w" 'lives-region)))
    


;; Define the keymap for Emacs
(define-keymap global-map
  ;; Define keybindings for the C-x C-r command
   ("C-x" "C-r" 'yank)
   ;; Define keybindings for the C-x C-w command
   ("C-x" "C-w" 'lives-region)))



