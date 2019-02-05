;;; xr.el --- Convert string regexp to rx notation   -*- lexical-binding: t -*-

;; Copyright (C) 2019 Free Software Foundation, Inc.

;; Author: Mattias Engdegård <mattiase@acm.org>
;; Version: 1.0
;; Keywords: lisp, maint, regexps

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is an inverse companion to the rx package for translating
;; regexps in string form to the rx notation.  Its chief uses are:
;;
;; - Migrating existing code to rx form, for better readability and
;;   maintainability
;; - Understanding complex regexp strings
;;   
;; Please refer to `rx' for more information about the notation.
;;
;; The exported functions are `xr', which simply returns the converted
;; rx expression, and `xr-pp', which pretty-prints the rx expression.
;; Suggested use is from an interactive elisp buffer.
;;
;; Example (regexp found in compile.el):
;;
;;   (xr-pp "\\`\\(?:[^^]\\|\\^\\(?: \\*\\|\\[\\)\\)")
;; =>
;;   (seq bos
;;        (or
;;         (not (any "^"))
;;         (seq "^"
;;              (or " *" "["))))
;;
;; The rx notation admits many synonyms; the xr functions mostly
;; prefer brief variants, such as `seq' to `sequence' and `nonl' to
;; `not-newline'.  The user is encouraged to edit the result for
;; maximum readability, consistency and personal preference when
;; replacing existing regexps in elisp code.

;; Similar functionality is provided by the `lex' package in the form of the
;; `lex-parse-re' function, but `xr' does not depend on `lex' and does
;; a more thorough job of handling all corner cases of Elisp's regexp syntax.

;;; Code:

(require 'rx)

(defun xr--parse-char-alt (negated)
  (let ((set nil))
    (cond
     ;; Initial ]-x range
     ((looking-at (rx "]-" (not (any "]"))))
      (push (match-string 0) set)
      (goto-char (match-end 0)))
     ;; Initial ]
     ((looking-at "]")
      (push "]" set)
      (forward-char 1)))
    (while (not (looking-at "]"))
      (cond
       ;; character class
       ((looking-at (rx "[:" (group (one-or-more letter)) ":]"))
        (let ((sym (intern (match-string 1))))
          (when (not (memq sym
                           '(ascii alnum alpha blank cntrl digit graph
                             lower multibyte nonascii print punct space
                             unibyte upper word xdigit)))
            (error "No such character class: %s" sym))
          (push sym set)
          (goto-char (match-end 0))))
       ;; character range
       ((looking-at (rx (not (any "]")) "-" (not (any "]"))))
        (let ((range (match-string 0)))
          ;; We render [a-z] as (any "a-z") instead of (any (?a . ?z))
          ;; for readability and brevity, and because the latter would
          ;; become (97 . 122) when printed.
          ;; TODO: Possibly convert "[0-9]" to digit, and
          ;; "[0-9a-fA-F]" (and permutations) to hex-digit.
          (goto-char (match-end 0))
          (let ((prev (car set)))
            ;; Merge with preceding range if any.
            (if (and (stringp prev)
                     (>= (length prev) 3)
                     (eq (aref prev 1) ?-))
                (setq set (cons (concat prev range) (cdr set)))
              (push range set)))))
       ((looking-at (rx eos))
        (error "Unterminated character alternative"))
       ;; plain character (including ^ or -)
       (t
        (let* ((ch (following-char))
               (ch-str (char-to-string ch)))
          ;; Merge with the previous string if neither contains "-".
          (if (and (stringp (car set))
                   (not (eq ch ?-))
                   (not (string-match "-" (car set))))
              (setq set (cons (concat (car set) ch-str) (cdr set)))
            (push ch-str set)))
        (forward-char 1))))

    (forward-char 1)                    ; eat the ]
    (cond
     ;; Non-negated single-char set, like [$]
     ((and (not negated)
           (= (length set) 1)
           (stringp (car set))
           (= (length (car set)) 1))
      (car set))
     ;; Single named class set, like [[:space:]]
     ((and (= (length set) 1)
           (symbolp (car set)))
      (if negated
          (list 'not (car set))
        (car set)))
     ;; Anything else.
     (negated
      (list 'not (cons 'any (reverse set))))
     (t
      (cons 'any (reverse set))))))

;; Reverse a sequence, flatten any (seq ...) inside, and concatenate
;; adjacent strings.
(defun xr--rev-join-seq (sequence)
  (let ((result nil))
    (while sequence
      (let ((elem (car sequence))
            (rest (cdr sequence)))
        (cond ((and (consp elem) (eq (car elem) 'seq))
               (setq sequence (append (reverse (cdr elem)) rest)))
              ((and (stringp elem) (stringp (car result)))
               (setq result (cons (concat elem (car result)) (cdr result)))
               (setq sequence rest))
              (t
               (setq result (cons elem result))
               (setq sequence rest)))))
    result))

(defun xr--char-category (negated category-code)
  (let ((sym (assq category-code
                   '((?0 . consonant)
                     (?1 . base-vowel)                        
                     (?2 . upper-diacritical-mark)            
                     (?3 . lower-diacritical-mark)            
                     (?4 . tone-mark)                 
                     (?5 . symbol)                            
                     (?6 . digit)                             
                     (?7 . vowel-modifying-diacritical-mark)  
                     (?8 . vowel-sign)                        
                     (?9 . semivowel-lower)                   
                     (?< . not-at-end-of-line)                
                     (?> . not-at-beginning-of-line)          
                     (?A . alpha-numeric-two-byte)            
                     (?C . chinese-two-byte)                  
                     (?G . greek-two-byte)                    
                     (?H . japanese-hiragana-two-byte)        
                     (?I . indian-two-byte)                   
                     (?K . japanese-katakana-two-byte)        
                     (?N . korean-hangul-two-byte)            
                     (?Y . cyrillic-two-byte)         
                     (?^ . combining-diacritic)               
                     (?a . ascii)                             
                     (?b . arabic)                            
                     (?c . chinese)                           
                     (?e . ethiopic)                          
                     (?g . greek)                             
                     (?h . korean)                            
                     (?i . indian)                            
                     (?j . japanese)                          
                     (?k . japanese-katakana)         
                     (?l . latin)                             
                     (?o . lao)                               
                     (?q . tibetan)                           
                     (?r . japanese-roman)                    
                     (?t . thai)                              
                     (?v . vietnamese)                        
                     (?w . hebrew)                            
                     (?y . cyrillic)                          
                     (?| . can-break)))))
    (when (not sym)
      (error "Unknown category code: %s" category-code))
    (let ((item (list 'category (cdr sym))))
      (if negated (list 'not item) item))))

(defun xr--char-syntax (negated syntax-code)
  (let ((sym (assq syntax-code
                   '((?-  . whitespace)
                     (?\s . whitespace)
                     (?.  . punctuation)
                     (?w  . word)
                     (?_  . symbol)
                     (?\( . open-parenthesis)
                     (?\) . close-parenthesis)
                     (?'  . expression-prefix)
                     (?\" . string-quote)
                     (?$  . paired-delimiter)
                     (?\\ . escape)
                     (?/  . character-quote)
                     (?<  . comment-start)
                     (?>  . comment-end)
                     (?|  . string-delimiter)
                     (?!  . comment-delimiter)))))
    (when (not sym)
      (error "Unknown syntax code: %s" syntax-code))
    (let ((item (list 'syntax (cdr sym))))
      (if negated (list 'not item) item))))

(defun xr--postfix (operator operand)
  ;; We use verbose names for the common *, + and ? operators for readability,
  ;; even though these names are affected by the rx-greedy-flag.
  ;; For the (less common) non-greedy operators we might want to
  ;; consider using minimal-match/maximal-match instead, but
  ;; this would complicate the implementation.
  (let* ((sym (cdr (assoc operator '(("*"  . zero-or-more)
                                     ("+"  . one-or-more)
                                     ("?"  . opt)
                                     ("*?" . *?)
                                     ("+?" . +?)
                                     ("??" . ??)))))
         ;; Simplify when the operand is (seq ...)
         (body (if (and (listp operand) (eq (car operand) 'seq))
                   (cdr operand)
                 (list operand))))
    (cons sym body)))

;; Apply a repetition of {LOWER,UPPER} to OPERAND.
;; UPPER may be nil, meaning infinity.
(defun xr--repeat (lower upper operand)
  ;; rx does not accept (= 0 ...) or (>= 0 ...), so we use 
  ;; (repeat 0 0 ...) and (zero-or-more ...), respectively.
  ;; Note that we cannot just delete the operand if LOWER=UPPER=0,
  ;; since doing so may upset the group numbering.
  (let* ((operator (cond ((null upper)
                          (if (zerop lower)
                              '(zero-or-more)
                            (list '>= lower)))
                         ((and (= lower upper) (> lower 0))
                          (list '= lower))
                         (t
                          (list 'repeat lower upper))))
         ;; Simplify when the operand is (seq ...).
         (body (if (and (listp operand) (eq (car operand) 'seq))
                   (cdr operand)
                 (list operand))))
    (append operator body)))
  
(defun xr--parse-seq ()
  (let ((sequence nil))                 ; reversed
    (while (not (looking-at (rx (or "\\|" "\\)" eos))))
      (cond
       ;; ^ - only special at beginning of sequence
       ((and (looking-at (rx "^")) (null sequence))
        (forward-char 1)
        (push 'bol sequence))

       ;; $ - only special at end of sequence
       ((looking-at (rx "$" (or "\\|" "\\)" eos)))
        (forward-char 1)
        (push 'eol sequence))

       ;; * ? + (and non-greedy variants)
       ;; - not special at beginning of sequence or after ^
       ((and (looking-at (rx (any "*?+") (opt "?")))
             sequence (not (eq (car sequence) 'bol)))
        (let ((operator (match-string 0)))
          (goto-char (match-end 0))
          (setq sequence (cons (xr--postfix operator (car sequence))
                               (cdr sequence)))))

       ;; \{..\} - not special at beginning of sequence or after ^
       ((and (looking-at (rx "\\{"))
             sequence (not (eq (car sequence) 'bol)))
        (forward-char 2)
        (if (looking-at (rx (opt (group (one-or-more digit)))
                            (opt (group ",")
                                 (opt (group (one-or-more digit))))
                            "\\}"))
            (let ((lower (if (match-string 1)
                             (string-to-number (match-string 1))
                           0))
                  (comma (match-string 2))
                  (upper (and (match-string 3)
                              (string-to-number (match-string 3)))))
              (goto-char (match-end 0))
              (setq sequence (cons (xr--repeat
                                    lower
                                    (if comma upper lower)
                                    (car sequence))
                                   (cdr sequence))))
          (error "Invalid \\{\\} syntax")))

       ;; nonspecial character
       ((looking-at (rx (not (any "\\.["))))
        (forward-char 1)
        (push (match-string 0) sequence))

       ;; character alternative
       ((looking-at (rx "[" (opt (group "^"))))
        (goto-char (match-end 0))
        (let ((negated (match-string 1)))
          (push (xr--parse-char-alt negated) sequence)))

       ;; group
       ((looking-at (rx "\\("
                        (opt (group "?" (group (zero-or-more digit)) ":"))))
        (let ((question (match-string 1))
              (number (match-string 2))
              (end (match-end 0)))
          (goto-char end)
          (let* ((group (xr--parse-alt))
                 ;; simplify - group has an implicit seq
                 (operand (if (and (listp group) (eq (car group) 'seq))
                              (cdr group)
                            (list group))))
            (when (not (looking-at (rx "\\)")))
              (error "Missing \\)"))
            (forward-char 2)
            (let ((item (cond ((not question)           ; plain subgroup
                               (cons 'group operand))
                              ((zerop (length number))  ; shy group
                               group)
                              (t
                               (append (list 'group-n (string-to-number number))
                                       operand)))))
              (push item sequence)))))

       ;; back-reference
       ((looking-at (rx "\\" (group digit)))
        (forward-char 2)
        (push (list 'backref (string-to-number (match-string 1)))
              sequence))

       ;; various simple substitutions
       ((looking-at (rx (or "." "\\w" "\\W" "\\`" "\\'" "\\="
                            "\\b" "\\B" "\\<" "\\>" "\\_<" "\\_>")))
        (goto-char (match-end 0))
        (let ((sym (cdr (assoc
                         (match-string 0)
                         '(("." . nonl)
                           ("\\w" . wordchar) ("\\W" . not-wordchar)
                           ("\\`" . bos) ("\\'" . eos)
                           ("\\=" . point)
                           ("\\b" . word-boundary) ("\\B" . not-word-boundary)
                           ("\\<" . bow) ("\\>" . eow)
                           ("\\_<" . symbol-start) ("\\_>" . symbol-end))))))
          (push sym sequence)))

       ;; character syntax
       ((looking-at (rx "\\" (group (any "sS")) (group anything)))
        (let ((negated (string-equal (match-string 1) "S"))
              (syntax-code (string-to-char (match-string 2))))
          (goto-char (match-end 0))
          (push (xr--char-syntax negated syntax-code) sequence)))

       ;; character categories
       ((looking-at (rx "\\" (group (any "cC")) (group anything)))
        (let ((negated (string-equal (match-string 1) "C"))
              (category-code (string-to-char (match-string 2))))
          (goto-char (match-end 0))
          (push (xr--char-category negated category-code) sequence)))

       ;; Escaped character. Only \*+?.^$[ really need escaping, but we accept
       ;; any not otherwise handled character after the backslash since
       ;; such sequences are found in the wild.
       ((looking-at (rx "\\" (group anything)))
        (forward-char 2)
        (push (match-string 1) sequence))

       (t (error "Backslash at end of regexp"))))

    (let ((item-seq (xr--rev-join-seq sequence)))
      (cond ((null item-seq)
             "")
            ((null (cdr item-seq))
             (car item-seq))
            (t 
             (cons 'seq item-seq))))))

(defun xr--parse-alt ()
  (let ((alternatives nil))             ; reversed
    (push (xr--parse-seq) alternatives)
    (while (not (looking-at (rx (or "\\)" eos))))
      (forward-char 2)                  ; skip \|
      (push (xr--parse-seq) alternatives))
    (if (cdr alternatives)
        ;; Simplify (or nonl "\n") to anything
        (if (or (equal alternatives '(nonl "\n"))
                (equal alternatives '("\n" nonl)))
            'anything
          (cons 'or (reverse alternatives)))
      (car alternatives))))

;;;###autoload
(defun xr (re-string)
  "Convert a regexp string to rx notation; the inverse of `rx'.
Passing the returned value to `rx' (or `rx-to-string') yields a regexp string
equivalent to RE-STRING."
  (with-temp-buffer
    (insert re-string)
    (goto-char (point-min))
    (let ((rx (xr--parse-alt)))
      (when (looking-at (rx "\\)"))
        (error "Unbalanced \\)"))
      rx)))

;; Print a rx expression to a string, unformatted.
(defun xr--rx-to-string (rx)
  (cond
   ((eq rx '*?) "*?")                   ; Avoid unnecessary \ in symbol.
   ((eq rx '+?) "+?")
   ((consp rx)
    ;; Render character ? as ?? when first in a list.
    ;; Elsewhere, it's just an integer.
    (let ((first (if (eq (car rx) ??)
                     "??"
                   (xr--rx-to-string (car rx))))
          (rest (mapcar #'xr--rx-to-string (cdr rx))))
      (concat "(" (mapconcat #'identity (cons first rest) " ") ")")))
   ((stringp rx)
    ;; Translate control and raw chars to escape sequences for readability.
    ;; We prefer hex escapes (\xHH) since that is usually what the user wants,
    ;; but use octal (\OOO) if a legitimate hex digit follows, as
    ;; hex escapes are not limited to two digits.
    (concat "\""
            (replace-regexp-in-string
             "[\x00-\x1f\"\\\x7f\x80-\xff][[:xdigit:]]?"
             (lambda (s)
               (let* ((c (logand (string-to-char s) #xff))
                      (xdigit (substring s 1))
                      (transl (assq c
                                    '((?\" . "\\\"")
                                      (?\\ . "\\\\")
                                      (?\a . "\\a")
                                      (?\b . "\\b")
                                      (?\t . "\\t")
                                      (?\n . "\\n")
                                      (?\v . "\\v")
                                      (?\f . "\\f")
                                      (?\r . "\\r")
                                      (?\e . "\\e")))))
                 (concat
                  (if transl
                      (cdr transl)
                    (format (if (zerop (length xdigit)) "\\x%02x" "\\%03o")
                            c))
                  xdigit)))
             rx 'fixedcase 'literal)
            "\""))
   (t (prin1-to-string rx))))

;; Pretty-print a regexp (in rx notation) to a string.
;; It does a slightly better job than standard `pp' for rx purposes.
(defun xr--pp-rx-to-str (rx)
  (with-temp-buffer
    (insert (xr--rx-to-string rx) "\n")
    (pp-buffer)

    ;; Remove the line break after "(not" for readability and compactness.
    (goto-char (point-min))
    (while (re-search-forward
            (rx bol
                (zero-or-more (any space)) "(not"
                (group "\n" (zero-or-more (any space)))
                (one-or-more nonl) "))"
                eol)
            nil t)
      (replace-match " " t t nil 1))
    
    (buffer-string)))

;;;###autoload
(defun xr-pp (re-string)
  "Convert to `rx' notation and pretty-print.
This basically does `(pp (rx RE-STRING))', but in a slightly more readable
way.  It is intended for use from an interactive elisp session.
Returns nil."
  (insert (xr--pp-rx-to-str (xr re-string))))


(defun xr--expect-result (fun input expected)
  "Verify (FUN INPUT) against EXPECTED."
  (let ((got (funcall fun input)))
    (unless (equal got expected)
      (error "Failure in (%s %S):\ngot      %S\nexpected %S"
             fun input got expected))))

(defun xr--expect (regexp-str expected-rx)
  "Verify (xr REGEXP-STR) against EXPECTED-RX."
  (xr--expect-result 'xr regexp-str expected-rx))

(defun xr--expect-pp (rx expected-str)
  "Verify (xr--pp-rx-to-str RX) against EXPECTED-STR."
  (xr--expect-result 'xr--pp-rx-to-str rx expected-str))

(provide 'xr)

(eval-when-compile
  ;; FIXME: When byte-compiling the file, this `eval-when-compile' block
  ;; will be executed at a time where the above functions have been compiled
  ;; but they're not necessarily known by the current Emacs session yet
  ;; (because the neither `xr.el' nor `xr.elc' has been loaded yet).
  ;; As a quick fix, we (require 'xr) here to load the `xr' file (and fail
  ;; silently if the file is not in `load-path').
  ;; Maybe a better fix is to move those tests to a separate file, and/or
  ;; to wrap them in an `ert-deftest'.
  (when (require 'xr nil 'noerror)
  (xr--expect "a\\$b\\\\c\\[\\]\\q"
              "a$b\\c[]q")
  (xr--expect "\\(?:ab\\|c*d\\)?"
              '(opt (or "ab" (seq (zero-or-more "c") "d"))))
  (xr--expect ".+"
              '(one-or-more nonl))
  (xr--expect "\\(?:x?y\\)\\{3\\}"
              '(= 3 (opt "x") "y"))
  (xr--expect "\\(?:x?y\\)\\{3,8\\}"
              '(repeat 3 8 (opt "x") "y"))
  (xr--expect "\\(?:x?y\\)\\{3,\\}"
              '(>= 3 (opt "x") "y"))
  (xr--expect "\\(?:x?y\\)\\{,8\\}"
              '(repeat 0 8 (opt "x") "y"))
  (xr--expect "\\(?:xy\\)\\{4,4\\}"
              '(= 4 "xy"))
  (xr--expect "a\\{,\\}"
              '(zero-or-more "a"))
  (xr--expect "a\\{0\\}"
              '(repeat 0 0 "a"))
  (xr--expect "a\\{0,\\}"
              '(zero-or-more "a"))
  (xr--expect "a\\{0,0\\}"
              '(repeat 0 0 "a"))
  (xr--expect "a\\{\\}"
              '(repeat 0 0 "a"))
  (xr--expect "a\\{,1\\}"
              '(repeat 0 1 "a"))
  (xr--expect "a\\{1,\\}"
              '(>= 1 "a"))
  (xr--expect "\\(ab\\)\\(?3:cd\\)\\1\\3"
              '(seq (group "ab") (group-n 3 "cd") (backref 1) (backref 3)))
  (xr--expect "^.\\w\\W\\`\\'\\=\\b\\B\\<\\>\\_<\\_>$"
              '(seq bol nonl wordchar not-wordchar bos eos point
                    word-boundary not-word-boundary bow eow
                    symbol-start symbol-end eol))
  (xr--expect "\\s-\\s \\sw\\s_\\s.\\s(\\s)\\s\""
              '(seq (syntax whitespace) (syntax whitespace) (syntax word)
                    (syntax symbol) (syntax punctuation)
                    (syntax open-parenthesis) (syntax close-parenthesis)
                    (syntax string-quote)))
  (xr--expect "\\s\\\\s/\\s$\\s'\\s<\\s>\\s!\\s|"
              '(seq (syntax escape) (syntax character-quote)
                    (syntax paired-delimiter) (syntax expression-prefix)
                    (syntax comment-start) (syntax comment-end)
                    (syntax comment-delimiter) (syntax string-delimiter)))
  (xr--expect "\\S-\\S<"
              '(seq (not (syntax whitespace))
                    (not (syntax comment-start))))
  (xr--expect "\\c0\\c1\\c2\\c3\\c4\\c5\\c6\\c7\\c8\\c9\\c<\\c>"
              '(seq (category consonant) (category base-vowel)
                    (category upper-diacritical-mark)
                    (category lower-diacritical-mark)
                    (category tone-mark) (category symbol) (category digit)
                    (category vowel-modifying-diacritical-mark)
                    (category vowel-sign) (category semivowel-lower)
                    (category not-at-end-of-line)
                    (category not-at-beginning-of-line)))
  (xr--expect "\\cA\\cC\\cG\\cH\\cI\\cK\\cN\\cY\\c^"
          '(seq (category alpha-numeric-two-byte) (category chinese-two-byte)
                (category greek-two-byte) (category japanese-hiragana-two-byte)
                (category indian-two-byte)
                (category japanese-katakana-two-byte)
                (category korean-hangul-two-byte) (category cyrillic-two-byte)
                (category combining-diacritic)))
  (xr--expect "\\ca\\cb\\cc\\ce\\cg\\ch\\ci\\cj\\ck\\cl\\co\\cq\\cr"
          '(seq (category ascii) (category arabic) (category chinese)
                (category ethiopic) (category greek) (category korean)
                (category indian)  (category japanese)
                (category japanese-katakana) (category latin) (category lao)
                (category tibetan) (category japanese-roman)))
  (xr--expect "\\ct\\cv\\cw\\cy\\c|"
              '(seq (category thai) (category vietnamese) (category hebrew)
                    (category cyrillic) (category can-break)))
  (xr--expect "\\C2\\C^"
              '(seq (not (category upper-diacritical-mark))
                    (not (category combining-diacritic))))
  (xr--expect "\\(?:a.\\)*?"
              '(*? "a" nonl))
  (xr--expect "\\(?:a.\\)+?"
              '(+? "a" nonl))
  (xr--expect "\\(?:a.\\)??"
              '(?? "a" nonl))
  (xr--expect "\\(?:.\\(a+\\(?:b+?c*\\)?\\)??\\)*"
              '(zero-or-more
                nonl
                (?? (group (one-or-more "a")
                           (opt (+? "b")
                                (zero-or-more "c"))))))
  (xr--expect "[[:alnum:][:blank:]][[:alpha:]][[:cntrl:][:digit:]]"
              '(seq (any alnum blank) alpha (any cntrl digit)))
  (xr--expect "[^[:lower:][:punct:]][^[:space:]]"
              '(seq (not (any lower punct)) (not space)))
  (xr--expect "^[a-z]*"
              '(seq bol (zero-or-more (any "a-z"))))
  (xr--expect "some[.]thing"
              "some.thing")
  (xr--expect "[^]-c]"
              '(not (any "]-c")))
  (xr--expect "[-^]"
              '(any "-" "^"))
  (xr--expect "[a-z-+/*%0-4[:xdigit:]]"
              '(any "a-z" "-" "+/*%" "0-4" xdigit))
  (xr--expect "[^]A-Za-z-]*"
              '(zero-or-more (not (any "]" "A-Za-z" "-"))))
  (xr--expect "[+*%A-Ka-k0-3${-}]"
              '(any "+*%" "A-Ka-k0-3" "$" "{-}"))
  (xr--expect ""
              "")
  (xr--expect "a\\|"
              '(or "a" ""))
  (xr--expect "\\|a"
              '(or "" "a"))
  (xr--expect "a\\|\\|b"
              '(or "a" "" "b"))
  (xr--expect "\\(?:.\\|\n\\)?\\(\n\\|.\\)*"
              '(seq (opt anything) (zero-or-more (group anything))))
  (xr--expect "\\*\\*\\* EOOH \\*\\*\\*\n"
              "*** EOOH ***\n")
  (xr--expect "\\<\\(catch\\|finally\\)\\>[^_]"
              '(seq bow (group (or "catch" "finally")) eow
                    (not (any "_"))))
  (xr--expect "[ \t\n]*:\\([^:]+\\|$\\)"
              '(seq (zero-or-more (any " \t\n")) ":"
                    (group (or (one-or-more (not (any ":")))
                               eol))))
  (xr--expect "^a^b\\(?:^c^\\|^d^\\|e^\\)^"
              '(seq bol "a^b" (or (seq bol "c^") (seq bol "d^") "e^") "^"))
  (xr--expect "$a$b\\(?:$c$\\|$d$\\|$e$\\)$"
              '(seq "$a$b" (or (seq "$c" eol) (seq "$d" eol) (seq "$e" eol))
                    eol))
  (xr--expect "*a\\|*b\\(*c\\)"
              '(or "*a" (seq "*b" (group "*c"))))
  (xr--expect "+a\\|+b\\(+c\\)"
              '(or "+a" (seq "+b" (group "+c"))))
  (xr--expect "?a\\|?b\\(?c\\)"
              '(or "?a" (seq "?b" (group "?c"))))
  (xr--expect "^**"
              '(seq bol (zero-or-more "*")))
  (xr--expect "^+"
              '(seq bol "+"))
  (xr--expect "^?"
              '(seq bol "?"))
  (xr--expect "*?a\\|^??b"
              '(or (seq (opt "*") "a") (seq bol (opt "?") "b")))
  (xr--expect "^\\{xy"
              '(seq bol "{xy"))
  (xr--expect "\\{2,3\\}"
              "{2,3}")
  (xr--expect "a\\(?:b?\\(?:c.\\)d*\\)e"
              '(seq "a" (opt "b") "c" nonl (zero-or-more "d") "e"))
  (xr--expect "a\\(?:b\\(?:c.d\\)e\\)f"
              '(seq "abc" nonl "def"))
  (xr--expect-pp "A\e\r\n\t\0 \x7f\x80\ B\xff\x02"
                 "\"A\\e\\r\\n\\t\\x00 \\x7f\\200B\\xff\\x02\"\n")
  (xr--expect-pp '(?? nonl)
                 "(?? nonl)\n")
  (xr--expect-pp '(repeat 1 63 "a")
                 "(repeat 1 63 \"a\")\n")
  ))

;;; xr.el ends here
