;;; scl-mode.el --- A mode for SCL
;;;
;;;
;;;
;;;
;;; Commentary:
;;;

;;; Code:

(require 'rx)

(defvar scl-mode-map nil
  "Key map for 'scl-mode`")

(setq scl-mode-map
      (let ((map (make-keymap)))
	(define-key map "\C-j" 'newline-and-indent)
	map))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.st\\'" . scl-mode))


(defun scl-regex-endswith (seq)
  ""
  (let ((words (regexp-opt seq 'words)))
    (concat ".*" words "$")))

(defun scl-regex-startswith (seq)
  ""
  (let ((words (regexp-opt seq 'words)))
    (concat "[ \t]*" words ".*")))


(defvar scl-indent-regex+1 nil "Regex for indentation increment.")
(setq scl-indent-regex+1
      (concat
       (scl-regex-endswith  '("DO" "THEN" "ELSE"))
       "\\|"
       (scl-regex-startswith
	'("FUNCTION_BLOCK" "PROGRAM" "CONFIGURATION" "VAR"
	  "VAR_INPUT" "VAR_OUTPUT"))))

(defvar scl-indent-regex-1 nil "Regex for indentation decrement.")
(setq scl-indent-regex-1
      "[ \t]*\\<\\(END_.*\\|ELSE\\)\\>[ \t]*")





(defvar scl-datatypes nil "Datatypes to be highlighted.")
(setq scl-datatypes
      (list "ANY" "ANY_BIT" "ANY_DATE" "ANY_DERIVED" "ANY_ELEMENTARY"
	    "ANY_INT" "ANY_NUM" "ANY_REAL" "ANY_STRING" "ARRAY"
	    "BOOL" "BYTE"
	    "INT" "ULINT" "USINT" "DINT" "LINT" "SINT" "USINT"
	    "REAL" "LREAL" "REAL"
	    "DOUBLE"
	    "WORD"  "LWORD" "DWORD"
	    "STRING" "WSTRING" "CHAR"
	    "DATE" "DATE_AND_TIME" "TIME" "TIME_OF_DAY" "TOD" "TIMER"
	    "TON_TIME" "TOFF_TIME" "R_TRIG"
	    "REF_TO"))

(defvar scl-keywords nil "Keywords to be highlighted.")
(setq scl-keywords
      (list "VAR_OUTPUT"
	     "BEGIN" "TRUE" "FALSE"
	     "AT" "NOT" "AND"
	     "BY" "CASE" "COLON" "CONFIGURATION"
	     "CONSTANT" "DO" "DT"
	     "ELSE" "ELSEIF" "END_CASE" "END_CONFIGURATION"
	     "END_FOR" "END_FUNCTION" "END_FUNCTION_BLOCK"
	     "END_IF" "END_PROGRAM" "END_REPEAT" "END_RESOURCE"
	     "END_STRUCT" "END_TYPE" "END_VAR" "END_WHILE"
	     "EXIT" "FOR" "FUNCTION" "FUNCTION_BLOCK"
	     "F_EDGE" "IF" "INTERVAL" "NIL"
	     "NON_RETAIN" "OF" "ON" "PRIORITY" "PROGRAM"
	     "READ_ONLY" "READ_WRITE" "REPEAT" "RESOURCE" "REGION" "END_REGION"
	     "RETAIN" "RETURN" "RIGHT_ARROW" "R_EDGE"
	     "SINGLE" "STRUCT" "TASK" "THEN" "TO"
	     "TYPE" "UNTIL" "VAR" "VAR_ACCESS" "VAR_CONFIG"
	     "VAR_EXTERNAL" "VAR_GLOBAL" "VAR_INPUT" "VAR_IN_OUT"
	     "VAR_TEMP" "WHILE" "WITH"))


(defvar scl-multi-line-comment-regex nil
  "Regex for multi-line comments.")
(defvar scl-single-line-comment-regex nil
  "Regex for single-line comments //.")

(setq scl-single-line-comment-regex
      (rx (group "//" (?? not-newline) "\n")))

(setq scl-multi-line-comment-regex
      (rx (or (group "/*" (?? anything) "*/")
	      (group "(*" (?? anything) "*)"))))

;; (defvar scl-string-regex nil
;;   "Regex for string literals.")
;; (setq scl-string-regex
;;       (rx (or (group "\"" (?? anything) "\"")
;; 	      (group ?' (?? anything) ?'))))

(defvar scl-string-regex nil
  "Regex for string literals.")
(setq scl-string-regex "['][a-zA-Z0-9_\\.\\-]+[']")


(defvar scl-time-regex
  nil "\\(TIME\\|T\\)[#]\\([0-9_]+\\(\\.[0-9]+\\)?\\(ms\\|s\\|m\\|h\\|d\\)\\)+.")
(setq scl-time-regex
      (rx word-start (or "TIME" "T") "#" (group (one-or-more (one-or-more digit)
					   (opt ?. (one-or-more digit))
					   (or "ms" ?s ?h ?m ?d))) word-end))

(defvar scl-date-regex nil "")
(setq scl-date-regex
      (rx word-start (or "DATE" "D") "#"
	  (group (repeat 4 digit) ?- (repeat 2 digit) ?- (repeat 2 digit)) word-end))

(defvar scl-datetime-regex nil)
(setq scl-datetime-regex
      (rx word-start (or "DATE_AND_TIME" "DT") ?#
	  (group (repeat 4 digit) ?- (repeat 2 digit) ?- (repeat 2 digit))
	  ?: (repeat 2 digit) ?: (repeat 2 digit) ?: (repeat 2 digit) word-end))

(defvar scl-variable-regex nil)
(setq scl-variable-regex "[#][a-zA-Z0-9_\\.\\-]+")

(defvar scl-bool-regex nil)
(setq scl-bool-regex (rx word-start (or "TRUE" "FALSE" "BIT#1" "BIT#0") word-end))

(defvar scl-font-lock-keywords "" nil)
(setq scl-font-lock-keywords
  `(
    ;(,scl-multi-line-comment-regex . font-lock-comment-face)
    (,scl-string-regex . font-lock-constant-face)
    (,scl-time-regex . font-lock-constant-face)
    (,scl-variable-regex . font-lock-variable-name-face)
    ;(,scl-datetime-regex . font-lock-constant-face)
    ;(,scl-date-regex . font-lock-constant-face)
    ;("\\(TIME_OF_DAY\\|TOD\\)#[012][0-9]:[0-5][0-9]:[0-5][0-9]\\(\\.[0-9]\\{,3\\}\\)"
    ; . font-lock-constant-face)
    ;("\\<.*#.*\\>" . font-lock-constant-face)
    ;("\\<[0-9]+\\(\\.[0-9]+\\)?\\>" . font-lock-constant-face)
    ;(,scl-bool-regex  . font-lock-constant-face)
    (,(concat "\\<" (regexp-opt scl-keywords) "\\>") . font-lock-keyword-face)
    (,(concat "\\<" (regexp-opt scl-datatypes) "\\>") . font-lock-type-face)))


(defun scl-indent-line ()
  "Identation function for scl-mode."
  (interactive)
  (beginning-of-line)
  (let ((not-indented t)
	cur-indent)
    (setq cur-indent
	  (save-excursion
	    (cond
	     ;; Beginning of the buffer => no indentation
	     ((bobp) 0)
	     ;; current line is deindentation
	     ((looking-at-p scl-indent-regex-1)
	      (forward-line -1) ;; do not understand this
	      (- (current-indentation) tab-width))
	     (t
	      (forward-line -1)
	      (if (looking-at-p scl-indent-regex+1)
		  (+ (current-indentation) tab-width)
		(current-indentation))))))
    (indent-line-to cur-indent)))

(defvar scl-mode-syntax-table nil
  "IEC 61131 Syntax Table.")

(setq scl-mode-syntax-table
      (let ((table (make-syntax-table)))
	(modify-syntax-entry ?_ "w" table)
	(modify-syntax-entry ?/ ". 12b" table)
	(modify-syntax-entry 40 ". 1n" table) ;; (
	(modify-syntax-entry 41 ". 4n" table) ;; )
	(modify-syntax-entry ?* ". 23n" table)
	(modify-syntax-entry ?\n "> b" table)
	table))

(defun scl-insert-if (&optional condition then else)
  (interactive)
  (let ((condition (read-string "Condition:" condition))
	(then (read-string "Then:" then))
	(else (read-string "Else:" else)))
    (insert "IF ")
    (insert condition)
    (insert "THEN\n")
    (insert then)
    (insert "\nELSE\n")
    (insert else)
    (insert "END_IF")))


(define-derived-mode
  scl-mode fundamental-mode
  "SIEMENS SCL"
  "A major mode for editing Structured Control Language files"
  :syntax-table scl-mode-syntax-table
  (setq-local comment-start "(*")
  (setq-local comment-end "*)")
  (setq-local font-lock-defaults
              '(scl-font-lock-keywords nil t)) ;set CASE-FOLD t
  (setq-local indent-line-function 'scl-indent-line))

(provide 'scl-mode)

;;; scl-mode.el ends here
