(require 'subr-x)

(defvar kernc-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; C/C++ style comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    ;; Preprocessor stuff
    (modify-syntax-entry ?# "." table)
    ;; Chars are the same as strings
    (modify-syntax-entry ?' "\"" table)
    ;; Treat <> as punctuation (needed to highlight C++ keywords properly in template syntax)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?% "." table)
    table))

(defun kernc-types ()
  '("char" "int" "long" "short" "void" "bool" "float" "double" "signed" "unsigned"
    "char16_t" "char32_t" "char8_t"
    "int8_t" "uint8_t" "int16_t" "uint16_t" "int32_t" "uint32_t" "int64_t" "uint64_t"
    "uintptr_t"
    "size_t"))

(defun kernc-keywords ()
  '("auto" "break" "case" "const" "continue" "default" "do"
    "else" "enum" "extern" "for" "goto" "if" "register"
    "return"  "sizeof" "static" "struct" "switch" "typedef"
    "union"  "volatile" "while" "alignas" "alignof" "and"
    "and_eq" "asm" "atomic_cancel" "atomic_commit" "atomic_noexcept" "bitand"
    "bitor" "catch"  "class" "co_await"
    "co_return" "co_yield" "compl" "concept" "const_cast" "consteval" "constexpr"
    "constinit" "decltype" "delete" "dynamic_cast" "explicit" "export" "false" 
    "friend" "inline" "mutable" "namespace" "new" "noexcept" "not" "not_eq"
    "nullptr" "operator" "or" "or_eq" "private" "protected" "public" "reflexpr"
    "reinterpret_cast" "requires" "static_assert" "static_cast" "synchronized"
    "template" "this" "thread_local" "throw" "true" "try" "typeid" "typename"
    "using" "virtual" "wchar_t" "xor" "xor_eq"))

(defun kernc-font-lock-keywords ()
  (list
   `("# *[#a-zA-Z0-9_]+" . font-lock-preprocessor-face)
   `("#.*include \\(\\(<\\|\"\\).*\\(>\\|\"\\)\\)" . (1 font-lock-string-face))
   `(,(regexp-opt (kernc-keywords) 'symbols) . font-lock-keyword-face)
   `(,(regexp-opt (kernc-types) 'symbols) . font-lock-type-face)))

(defun kernc--previous-non-empty-line ()
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (string-empty-p
                 (string-trim-right
                  (thing-at-point 'line t))))
      (forward-line -1))
    (thing-at-point 'line t)))

(defun kernc--indentation-of-previous-non-empty-line ()
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (string-empty-p
                 (string-trim-right
                  (thing-at-point 'line t))))
      (forward-line -1))
    (current-indentation)))

(defun kernc--desired-indentation ()
  (let* ((cur-line (string-trim-right (thing-at-point 'line t)))
         (prev-line (string-trim-right (kernc--previous-non-empty-line)))
         (indent-len 8)  ;; Use tabs and 8-space wide indentation
         (prev-indent (kernc--indentation-of-previous-non-empty-line)))
    (cond
     ;; Special handling for switch/case
     ((string-match-p "^\\s-*switch\\s-*(.+)" prev-line)
      prev-indent)
     ((and (string-suffix-p "{" prev-line)
           (string-prefix-p "}" (string-trim-left cur-line)))
      prev-indent)
     ((string-suffix-p "{" prev-line)
      (+ prev-indent indent-len))
     ((string-prefix-p "}" (string-trim-left cur-line))
      (max (- prev-indent indent-len) 0))
     ((string-suffix-p ":" prev-line)
      (if (string-suffix-p ":" cur-line)
          prev-indent
        (+ prev-indent indent-len)))
     ((string-suffix-p ":" cur-line)
      (max (- prev-indent indent-len) 0))
     (t prev-indent))))

(defun kernc-indent-line ()
  "Indent current line according to Linux kernel C style."
  (interactive)
  (when (not (bobp))
    (let* ((desired-indentation
            (kernc--desired-indentation))
           (n (max (- (current-column) (current-indentation)) 0)))
      (indent-line-to desired-indentation)
      (if (zerop desired-indentation)
          (insert-char ?\t))  ;; Insert tab for indentation level 0
      (forward-char n))))

;;; Mode Definition
(define-derived-mode kernc-mode prog-mode "Kernel C"
  "A major mode for editing C files with Linux kernel style."
  :syntax-table kernc-mode-syntax-table
  (setq-local font-lock-defaults '(kernc-font-lock-keywords))
  (setq-local indent-line-function 'kernc-indent-line)
  (setq-local comment-start "// ")
  (setq-local tab-width 8)  ;; Set tab width to 8 for Linux kernel style
  (setq-local indent-tabs-mode t))  ;; Use tabs instead of spaces

(provide 'kernc-mode)
