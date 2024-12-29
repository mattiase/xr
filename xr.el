;;; xr.el --- Convert string regexp to rx notation   -*- lexical-binding: t -*-

;; Copyright (C) 2019-2024 Free Software Foundation, Inc.

;; Author: Mattias Engdegård <mattiase@acm.org>
;; Version: 2.1
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/mattiase/xr
;; Keywords: lisp, regexps

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

;; This package translates regexps in string form to the rx notation.
;; It can also find mistakes and questionable constructs in regexps
;; and related expressions. See the README file for more information.

;;; Code:

(require 'rx)
(require 'cl-lib)

(defun xr--add-diag-group (warnings group)
  (push group (car warnings)))

(defun xr--warn (warnings beg end message &rest info)
  "Add the warning MESSAGE at BEG..END to WARNINGS.
BEG and END are inclusive char indices.  END is nil if only start is known.
More BEG END MESSAGE argument triples for info-level messages can follow."
  (when warnings
    (let ((more nil))
      (while info
        (unless (cddr info)
          (error "bad xr--warn info args"))
        (push (list (nth 0 info) (nth 1 info) (nth 2 info) 'info)
              more)
        (setq info (cdddr info)))
      (xr--add-diag-group warnings (cons (list beg end message 'warning)
                                         (nreverse more))))))

(defun xr--add-error (warnings beg end message)
  (when warnings
    (xr--add-diag-group warnings (list (list beg end message 'error)))))

(define-error 'xr-parse-error "xr parsing error")

(defun xr--error (beg end message &rest args)
  "Format MESSAGE with ARGS at BEG..END as an error and abort the parse.
END is nil if unknown."
  (signal 'xr-parse-error
          (list (apply #'format-message message args) beg end)))

;; House versions of `cl-some' and `cl-every', but faster.

(defmacro xr--some (pred list)
  "Whether PRED is true for at least one element in LIST."
  `(let ((list ,list))
     (while (and list (not (funcall ,pred (car list))))
       (setq list (cdr list)))
     list))

(defmacro xr--every (pred list)
  "Whether PRED is true for all elements in LIST."
  `(let ((list ,list))
     (while (and list (funcall ,pred (car list)))
       (setq list (cdr list)))
     (not list)))

(eval-when-compile
  (defconst xr--char-classes '( ascii alnum alpha blank cntrl digit graph
                                lower multibyte nonascii print punct space
                                unibyte upper word xdigit)))

;; FIXME: `eval-and-compile' around `pcase-defmacro' only necessary
;; for compatibility with Emacs 27.
(eval-and-compile
  (pcase-defmacro xr--char-class ()
    "Match any standard regexp char class as a symbol."
    `(or ,@(mapcar (lambda (x) `(quote ,x)) xr--char-classes)))
  )

(defvar xr--string)
(defvar xr--len)
(defvar xr--idx)

(defmacro xr--substring-p (string idx substring)
  "Whether SUBSTRING is in STRING at IDX."
  (let ((i (make-symbol "i"))
        (sub (make-symbol "sub")))
    `(let ((,i ,idx)
           (,sub ,substring))
       (eq (compare-strings ,string ,i (+ ,i (length ,sub)) ,sub nil nil)
           t))))

;; `string-search' added in Emacs 28
(defalias 'xr--string-search
  (if (fboundp 'string-search)
      #'string-search
    (lambda (needle haystack start-pos)
      "Index of the string NEEDLE in the string HAYSTACK, or nil."
      (string-match-p (regexp-quote needle) haystack start-pos))))

(defun xr--parse-char-alt (negated warnings checks)
  (let* ((intervals nil)
         (classes nil)
         (start-pos xr--idx)
         (string xr--string)
         (len xr--len)
         (idx start-pos)
         ch)
    (while (and (< idx len)
                (or (not (eq (setq ch (aref string idx)) ?\]))
                    (= idx start-pos)))
      (cond
       ;; character class
       ((and (eq ch ?\[)
             (< (+ idx 3) len)
             (eq (aref string (1+ idx)) ?:)
             (let ((i (xr--string-search ":]" string (+ 2 idx))))
               (and i
                    (let ((sym (intern (substring string (+ idx 2) i))))
                      (unless (memq sym (eval-when-compile xr--char-classes))
                        (xr--error idx (1+ i)
                                   "No character class `[:%s:]'"
                                   (symbol-name sym)))
                      (let ((prev (assq sym classes)))
                        (if prev
                            (let* ((prev-beg (cdr prev))
                                   (prev-end (1+ (xr--string-search
                                                  ":]" string prev-beg))))
                              (xr--warn
                               warnings idx (1+ i)
                               (format-message
                                "Duplicated character class `[:%s:]'" sym)
                               prev-beg prev-end "Previous occurrence here"))
                          (push (cons sym idx) classes)))
                      (setq idx (+ i 2))
                      t)))))
       ;; character range
       ((and (< (+ idx 3) len)
             (eq (aref string (1+ idx)) ?-)
             (not (memq (aref string (+ idx 2)) '(?\] nil))))
        (let ((start ch)
              (end (aref string (+ idx 2))))
          (cond
           ((<= start #x7f #x3fff80 end)
            ;; Intervals that go from ASCII (0-7f) to raw bytes
            ;; (3fff80-3fffff) always exclude the intervening (Unicode) points.
            (push (vector start #x7f idx) intervals)
            (push (vector #x3fff80 end idx) intervals))
           ((<= start end)
            (push (vector start end idx) intervals))
           ;; It's unlikely that anyone writes z-a by mistake; don't complain.
           ((and (eq start ?z) (eq end ?a)))
           (t
            (xr--warn
             warnings idx (+ idx 2)
             (xr--escape-string
              (format-message "Reversed range `%c-%c' matches nothing"
                              start end)))))
          (cond
           ;; Suppress warnings about ranges between adjacent digits,
           ;; like [0-1], as they are common and harmless.
           ((and (= end (1+ start)) (not (<= ?0 start end ?9)))
            (xr--warn warnings idx (+ idx 2)
                          (xr--escape-string
                           (format-message "Two-character range `%c-%c'"
                                           start end))))
           ;; This warning is not necessarily free of false positives,
           ;; although they are unlikely. Maybe it should be off by default?
           ((and (<= ?A start ?Z) (<= ?a end ?z))
            (xr--warn
             warnings idx (+ idx 2)
             (format-message
              "Range `%c-%c' between upper and lower case includes symbols"
              start end)))
           ;; Ranges on the form +-X and X-+ are likely to be a
           ;; mistake because matching both + and - is common.
           ((and (eq checks 'all)
                 (or (eq start ?+) (eq end ?+)))
            (xr--warn
             warnings idx (+ idx 2)
             (xr--escape-string
              (format-message
               "Suspect character range `%c-%c': should `-' be literal?"
               start end)))))

          (setq idx (+ idx 3))))
       ;; single character (including ], ^ and -)
       (t
        (when (and
               warnings
               (eq ch ?\[)
               ;; Ad-hoc pattern attempting to catch mistakes
               ;; on the form [...[...]...]
               ;; where we are    ^here
               (let ((i (1+ idx)))
                 (while (and (< i len)
                             (not (memq (aref string i) '(?\[ ?\]))))
                   (setq i (1+ i)))
                 (and (< i len)
                      (eq (aref string i) ?\])
                      (let ((j (1+ i)))
                        (while (and (< j len)
                                    (not (memq (aref string j) '(?\[ ?\]))))
                          (setq j (1+ j)))
                        (and
                         (< (+ i 1) j len)
                         (not (memq (aref string (- j 1)) '(?\[ ?\\)))
                         (eq (aref string j) ?\])))))
               ;; Only if the alternative didn't start with ]
               (not (and intervals
                         (eq (aref (car (last intervals)) 0) ?\]))))
          (xr--warn warnings idx idx
                        (format-message "Suspect `[' in char alternative")))
        (when (and (eq ch ?-)
                   (< start-pos idx (1- len))
                   (not (eq (aref string (1+ idx)) ?\])))
          (xr--warn
           warnings idx idx
           (format-message
            "Literal `-' not first or last in character alternative")))
        (when (eq checks 'all)
          (let ((last (car-safe intervals)))
            (when (and last
                       (eq (aref last 1) ?\\)
                       (or (memq ch '( ?t ?n ?r ?f ?x ?e ?b  ; char escapes
                                       ?s ?S ?d ?D ?w ?W))   ; PCRE sequences
                           (<= ?0 ch ?7))                    ; octal escapes
                       ;; Suppress some common false positives, eg [\\nrt]
                       (not (and (memq ch '(?t ?n ?r ?f ?x ?e ?b))
                                 (< (1+ idx) len)
                                 (memq (aref string (1+ idx))
                                       '(?t ?n ?r ?f ?x ?e ?b)))))
              (xr--warn
               warnings (- idx 1) idx
               (format-message
                "Possibly erroneous `\\%c' in character alternative" ch)))))
        (push (vector ch ch idx) intervals)
        (setq idx (1+ idx)))))

    (unless (< idx len)
      (xr--error (- start-pos (if negated 2 1)) (- len 1)
                 "Unterminated character alternative"))

    (setq xr--idx (1+ idx))             ; eat the ] and write back

    ;; Detect duplicates and overlapping intervals.
    (let* ((sorted
            (sort (nreverse intervals)
                  (lambda (a b) (< (aref a 0) (aref b 0)))))
           (s sorted))
      (while (cdr s)
        (let* ((this (car s))
               (next (cadr s)))
          (if (>= (aref this 1) (aref next 0))
              ;; Overlap.
              (let* ((a (if (< (aref this 2) (aref next 2)) this next))
                     (b (if (< (aref this 2) (aref next 2)) next this)))
                (cond
                 ;; Duplicate character: drop it and warn.
                 ((and (eq (aref a 0) (aref a 1))
                       (eq (aref b 0) (aref b 1)))
                  (xr--warn warnings
                            (aref b 2) (aref b 2)
                            (xr--escape-string
                             (format-message
                              "Duplicated `%c' inside character alternative"
                              (aref this 0)))
                            (aref a 2) (aref a 2) "Previous occurrence here"))
                 ;; Duplicate range: drop it and warn.
                 ((and (eq (aref a 0) (aref b 0))
                       (eq (aref a 1) (aref b 1)))
                  (xr--warn
                   warnings (aref b 2) (+ (aref b 2) 2)
                   (xr--escape-string
                    (format-message
                     "Duplicated `%c-%c' inside character alternative"
                     (aref b 0) (aref b 1)))
                   (aref a 2) (+ (aref a 2) 2) "Previous occurrence here"))
                 ;; Character in range: drop it and warn.
                 ((eq (aref a 0) (aref a 1))
                  (when (eq a this)
                    (setcar s next))
                  (xr--warn
                   warnings (aref b 2) (+ (aref b 2) 2)
                   (xr--escape-string
                    (format-message
                     "Range `%c-%c' includes character `%c'"
                     (aref b 0) (aref b 1) (aref a 0)))
                   (aref a 2) (aref a 2) "Previous occurrence here"))
                 ;; Same but other way around.
                 ((eq (aref b 0) (aref b 1))
                  (when (eq b this)
                    (setcar s next))
                  (xr--warn
                   warnings (aref b 2) (aref b 2)
                   (xr--escape-string
                    (format-message
                     "Character `%c' included in range `%c-%c'"
                     (aref b 0) (aref a 0) (aref a 1)))
                   (aref a 2) (+ (aref a 2) 2) "Previous occurrence here"))
                 ;; Overlapping ranges: merge and warn.
                 (t
                  (let ((this-end (aref this 1)))
                    (aset this 1 (max (aref this 1) (aref next 1)))
                    (xr--warn
                     warnings (aref b 2) (+ (aref b 2) 2)
                     (xr--escape-string
                      (format-message "Ranges `%c-%c' and `%c-%c' overlap"
                                      (aref this 0) this-end
                                      (aref next 0) (aref next 1)))
                     (aref a 2) (+ (aref a 2) 2) "Previous occurrence here"))))
                (setcdr s (cddr s)))
            ;; No overlap.
            (setq s (cdr s)))))
            
      ;; Gather ranges and single characters separately.
      ;; We make no attempts at merging adjacent intervals/characters,
      ;; nor at splitting short intervals such as "a-b"; if the user
      ;; wrote it that way, there was probably a reason for it.
      (let ((ranges nil)
            (chars nil))
        (dolist (interv sorted)
          (if (eq (aref interv 0) (aref interv 1))
              (push (aref interv 0) chars)
            (push (string (aref interv 0) ?- (aref interv 1))
                  ranges)))
        
        ;; We return (any) for non-negated empty sets, such as [z-a].
        ;; `unmatchable' would perhaps be better; both require Emacs 27.1
        ;; or newer for use in rx.
        (cond
         ;; Negated empty set, like [^z-a]: anything.
         ((and negated
               (null chars)
               (null ranges)
               (null classes))
          'anything)
         ;; Non-negated single-char set, like [$]: make a string.
         ;; FIXME: Translate [^a] to (not "a") instead of (not (any "a")),
         ;; the latter only required for use in Emacs 26 and older.
         ((and (= (length chars) 1)
               (not negated)
               (null ranges)
               (null classes))
          (string (car chars)))
         ;; Single named class, like [[:space:]]: use the symbol.
         ((and (= (length classes) 1)
               (null chars)
               (null ranges))
          (if negated
              (list 'not (caar classes))
            (caar classes)))
         ;; [^\n]: nonl.
         ((and negated
               (equal chars '(?\n))
               (null ranges)
               (null classes))
          'nonl)
         ;; Anything else: produce (any ...)
         (t
          ;; Put dash last of all single characters.
          (when (memq ?- chars)
            (setq chars (cons ?- (delq ?- chars))))
          (let* ((set (cons 'any
                            (nconc
                             (and ranges
                                  (list (apply #'concat (nreverse ranges))))
                             (and chars
                                  (list (apply #'string (nreverse chars))))
                             (nreverse (mapcar #'car classes))))))
            (if negated
                (list 'not set)
              set))))))))

(defun xr--rev-join-seq (sequence)
  "Reverse SEQUENCE, flatten any (seq ...) inside, and concatenate
adjacent strings. SEQUENCE is used destructively."
  (let ((strings nil)
        (result nil))
    (while sequence
      (let ((elem (car sequence))
            (rest (cdr sequence)))
        (setq sequence
              (cond ((stringp elem)
                     (unless (equal elem "")
                       (push elem strings))
                     rest)
                    ((eq (car-safe elem) 'seq)
                     (nconc (nreverse (cdr elem)) rest))
                    (strings
                     (push (if (cdr strings)
                               (mapconcat #'identity strings nil)
                             (car strings))
                           result)
                     (setq strings nil)
                     (push elem result)
                     rest)
                    (t
                     (push elem result)
                     rest)))))
    (if strings
        (cons (if (cdr strings)
                  (mapconcat #'identity strings nil)
                (car strings))
              result)
      result)))

(defun xr--char-category (negated category-code)
  (let* ((sym (assq category-code
                    '((?\s . space-for-indent)
                      (?. . base)
                      (?0 . consonant)
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
                      (?L . strong-left-to-right)
                      (?N . korean-hangul-two-byte)            
                      (?R . strong-right-to-left)
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
                      (?| . can-break))))
         (item (list 'category (if sym (cdr sym) category-code))))
    (if negated (list 'not item) item)))

(defconst xr--char-syntax-alist
  '((?-  . whitespace)
    (?\s . whitespace)
    (?.  . punctuation)
    (?w  . word)
    (?W  . word)       ; undocumented
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
    (?!  . comment-delimiter)))

(defun xr--postfix (operator-char lazy operand)
  ;; We use verbose names for the common *, + and ? operators for readability
  ;; even though these names are affected by the rx-greedy-flag, since nobody
  ;; uses minimal-match in practice.
  (let* ((sym (cdr (assq operator-char
                         (if lazy
                             ;; What a pretty symmetry!
                             '((?* . *?)
                               (?+ . +?)
                               (?? . ??))
                           '((?*  . zero-or-more)
                             (?+  . one-or-more)
                             (??  . opt))))))
         ;; Simplify when the operand is (seq ...)
         (body (if (and (listp operand) (eq (car operand) 'seq))
                   (cdr operand)
                 (list operand))))
    (cons sym body)))

(defun xr--repeat (lower upper operand)
  "Apply a repetition of {LOWER,UPPER} to OPERAND.
UPPER may be nil, meaning infinity."
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
  
(defconst xr--zero-width-assertions
  '(bol eol bos eos bow eow word-boundary not-word-boundary
    symbol-start symbol-end point))

(defun xr--matches-empty-p (rx)
  "Whether RX can match the empty string regardless of context."
  (pcase rx
    (`(,(or 'seq 'one-or-more '+? 'group) . ,body)
     (xr--every #'xr--matches-empty-p body))
    (`(or . ,body)
     (xr--some #'xr--matches-empty-p body))
    (`(group-n ,_ . ,body)
     (xr--every #'xr--matches-empty-p body))
    (`(,(or 'opt 'zero-or-more ?? '*?) . ,_)
     t)
    (`(repeat ,from ,_ . ,body)
     (or (= from 0)
         (xr--every #'xr--matches-empty-p body)))
    (`(,(or '= '>=) ,_ . ,body)
     (xr--every #'xr--matches-empty-p body))
    ("" t)))

(defun xr--adjacent-subsumption (a b)
  "Check if A subsumes B, or vice versa, or not, assuming they are adjacent.
Return `a-subsumes-b', `b-subsumes-a' or nil."
  ;; Check for subsuming repetitions in sequence: (Ra A) (Rb B)
  ;; where Ra and Rb are repetition operators, and A and B are operands.
  ;; We conclude that (Ra A) subsumes (Rb B), in the sense that the
  ;; sequence is equivalent to just (Ra A), if:
  ;;       A matches a superset of B
  ;;   and Ra can match infinitely many times
  ;;   and Rb can match zero times
  ;;   and Rb is non-greedy if Ra is non-greedy.
  ;; Example: [cd]+c?
  (let ((a-expr (and (consp a)
                     (memq (car a)
                           '(zero-or-more one-or-more opt *? +? ??))
                     (xr--make-seq (cdr a)))))
    (when a-expr
      (let ((b-expr (and (consp b)
                         (memq (car b)
                               '(zero-or-more one-or-more opt *? +? ??))
                         (xr--make-seq (cdr b)))))
        (when b-expr
          (let ((a-op (car a))
                (b-op (car b)))
            ;; Test the same condition twice, but mirrored.
            (cond
             ((and (memq b-op '(zero-or-more opt *? ??))
                   (memq a-op '(zero-or-more one-or-more *? +?))
                   (not (and (memq a-op '(*? +?))
                             (memq b-op '(zero-or-more opt))))
                   (xr--superset-p a-expr b-expr))
              'a-subsumes-b)
             ((and (memq a-op '(zero-or-more opt *? ??))
                   (memq b-op '(zero-or-more one-or-more *? +?))
                   (not (and (memq b-op '(*? +?))
                             (memq a-op '(zero-or-more opt))))
                   (xr--superset-p b-expr a-expr))
              'b-subsumes-a))))))))
  
(defun xr--check-wrap-around-repetition (operand beg end warnings)
  "Whether OPERAND has a wrap-around repetition subsumption case,
like (* (* X) ... (* X))."
  (when (and (consp operand)
             (memq (car operand) '(seq group group-n)))
    (let* ((operands
            (if (eq (car operand) 'group-n)
                (cddr operand)
              (cdr operand))))
      (when (cddr operands)
        (let* ((first (car operands))
               (last (car (last operands)))
               (subsumption (xr--adjacent-subsumption last first)))
          (when subsumption
            ;; FIXME: add info about first and last item.
            ;; How do we get their locations?
            (xr--warn
             warnings beg end
             (if (eq subsumption 'b-subsumes-a)
                 "First item in repetition subsumes last item (wrapped)"
               "Last item in repetition subsumes first item (wrapped)"))))))))

(defun xr--parse-seq (warnings purpose checks)
  (let ((locations nil)            ; starting idx for each item in sequence
        (sequence nil)             ; parsed items, reversed
        (string xr--string)
        (len xr--len)
        (idx xr--idx)
        (at-end nil))
    (while (and (< idx len) (not at-end))
      (let ((item-start idx)
            (next-char (aref string idx)))
        (push item-start locations)
        (cond
         ;; ^ - only special at beginning of sequence
         ((eq next-char ?^)
          (setq idx (1+ idx))
          (if (null sequence)
              (progn
                (when (eq purpose 'file)
                  (xr--warn warnings item-start item-start
                                "Use \\` instead of ^ in file-matching regexp"))
                (push 'bol sequence))
            (xr--warn warnings item-start item-start
                          (format-message "Unescaped literal `^'"))
            (push "^" sequence)))

         ;; $ - only special at end of sequence
         ((eq next-char ?$)
          (setq idx (1+ idx))
          (if (or (>= idx len)
                  (and (< (1+ idx) len)
                       (eq (aref string idx) ?\\)
                       (memq (aref string (1+ idx)) '(?| ?\)))))
              (progn
                (when (eq purpose 'file)
                  (xr--warn warnings item-start item-start
                                "Use \\' instead of $ in file-matching regexp"))
                
                (push 'eol sequence))
            (xr--warn warnings item-start item-start
                          (format-message "Unescaped literal `$'"))
            (push "$" sequence)))

         ;; not-newline
         ((eq next-char ?.)
          (setq idx (1+ idx))
          ;; Assume that .* etc is intended.
          (when (and (eq purpose 'file)
                     (not (and (< idx len)
                               (memq (aref string idx) '(?? ?* ?+)))))
            (xr--warn warnings item-start item-start
                          (format-message
                           "Possibly unescaped `.' in file-matching regexp")))
          (push 'nonl sequence))

          ;; character alternative
         ((eq next-char ?\[)
          (setq idx (1+ idx))
          (let ((negated (and (< idx len) (eq (aref string idx) ?^))))
            (when negated (setq idx (1+ idx)))
            ;; FIXME: ugly spill and fill around call
            (setq xr--idx idx)
            (push (xr--parse-char-alt negated warnings checks) sequence)
            (setq idx xr--idx)))

         ;; * ? + (and non-greedy variants)
         ((memq next-char '(?* ?? ?+))
          ;; - not special at beginning of sequence or after ^ or \`
          (if (and sequence
                   (not (and (memq (car sequence) '(bol bos))
                             (memq (aref string (1- idx)) '(?^ ?`)))))
              (let* ((operator-char next-char)
                     (lazy (and (< (1+ item-start) len)
                                (eq (aref string (1+ item-start)) ??)))
                     (end-idx (if lazy (1+ item-start) item-start))
                     (operand (car sequence)))
                (when warnings
                  ;; Check both (OP (OP X)) and (OP (group (OP X))).
                  (let ((inner-op
                         (and (consp operand)
                              (if (eq (car operand) 'group)
                                  (and (null (cddr operand))
                                       (let ((inner (cadr operand)))
                                         (and (consp inner)
                                              (car inner))))
                                (car operand)))))
                    (cond
                     ((and
                       ;; (OP1 (OP2 X)), for any repetitions OP1, OP2
                       (memq inner-op '(opt zero-or-more one-or-more *? +? ??))
                       ;; Except (? (+ X)) which may be legitimate.
                       (not (and (eq operator-char ??)
                                 (consp operand)
                                 (memq inner-op '(one-or-more +?)))))
                      (let ((outer-opt (eq operator-char ??))
                            (inner-opt (memq inner-op '(opt ??))))
                        (xr--warn warnings
                                  idx end-idx
                                  (if outer-opt
                                      (if inner-opt
                                          "Optional option"
                                        "Optional repetition")
                                    (if inner-opt
                                        "Repetition of option"
                                      "Repetition of repetition"))
                                  (cadr locations) (1- idx)
                                  "This is the inner expression")))
                     ((memq operand xr--zero-width-assertions)
                      (xr--warn warnings
                                idx end-idx
                                (if (eq operator-char ??)
                                    "Optional zero-width assertion"
                                  "Repetition of zero-width assertion")
                                (cadr locations) (1- idx)
                                "Zero-width assertion here"))
                     ((and (xr--matches-empty-p operand)
                           ;; Rejecting repetition of the empty string
                           ;; suppresses some false positives.
                           (not (equal operand "")))
                      (xr--warn warnings
                                idx end-idx
                                (concat
                                 (if (eq operator-char ??)
                                     "Optional expression"
                                   "Repetition of expression")
                                 " matching an empty string")
                                (cadr locations) (1- idx)
                                "This expression matches an empty string"))
                     ((and (memq operator-char '(?* ?+))
                           (consp operand)
                           (memq (car operand) '(seq group))
                           (let ((nonzero-items
                                  (mapcan
                                   (lambda (item)
                                     (and (not (xr--matches-empty-p item))
                                          (list item)))
                                   (cdr operand))))
                             (and (= (length nonzero-items) 1)
                                  (consp (car nonzero-items))
                                  (memq (caar nonzero-items)
                                        '( opt zero-or-more one-or-more
                                           +? *? ?? >=)))))
                      (xr--warn warnings
                                idx end-idx
                                "Repetition of effective repetition"
                                (cadr locations) (1- idx)
                                "This expression contains a repetition"))))
                  ;; (* (* X) ... (* X)) etc: wrap-around subsumption
                  (unless (eq operator-char ??)
                    (xr--check-wrap-around-repetition
                     operand (cadr locations) end-idx warnings)))
                (setq idx (1+ end-idx))
                (setq sequence (cons (xr--postfix operator-char lazy operand)
                                     (cdr sequence)))
                (pop locations))
            (setq idx (1+ idx))
            (xr--warn warnings item-start item-start
                          (format-message "Unescaped literal `%c'" next-char))
            (push (char-to-string next-char) sequence)))

         ;; Anything starting with backslash
         ((eq next-char ?\\)
          (setq idx (1+ idx))
          (unless (< idx len)
            (xr--error (1- len) (1- len) "Backslash at end of regexp"))
          (setq next-char (aref string idx))
          (cond
           ;; end of sequence: \) or \|
           ((memq next-char '(?\) ?|))
            (setq idx (1- idx))         ; regurgitate the backslash
            (setq at-end t))
            
           ;; group
           ((eq next-char ?\()
            (setq idx (1+ idx))
            (let* ((group-start idx)
                   (submatch
                    (if (and (< idx len) (eq (aref string idx) ??))
                        (progn
                          (setq idx (1+ idx))
                          (unless (< idx len)
                            (xr--error (- idx 3) (1- idx)
                                       "Invalid \\(? syntax"))
                          (let ((c (aref string idx)))
                            (cond
                             ((eq c ?:)
                              (setq idx (1+ idx))
                              nil)
                             ((and (<= ?1 c ?9)
                                   (let ((i (1+ idx)))
                                     (while
                                         (and (< i len)
                                              (<= ?0 (aref string i) ?9))
                                       (setq i (1+ i)))
                                     (and (< i len)
                                          (eq (aref string i) ?:)
                                          (prog1
                                              (string-to-number
                                               (substring string idx i))
                                            (setq idx (1+ i)))))))
                             (t (xr--error (- idx 3) (1- idx)
                                           "Invalid \\(? syntax")))))
                      (when (and (eq checks 'all)
                                 (< (1+ idx) len)
                                 (eq (aref string idx) ?:)
                                 (eq (aref string (1+ idx)) ??)
                                 ;; suppress if the group ends after the :?
                                 (not (xr--substring-p string (+ idx 2) "\\)")))
                        (xr--warn
                         warnings idx (1+ idx)
                         (format-message
                          "Possibly mistyped `:?' at start of group")))
                      'unnumbered))
                   (group (progn
                            ;; FIXME: ugly spill and fill around call
                            (setq xr--idx idx)
                            (prog1
                                (xr--parse-alt warnings purpose checks)
                              (setq idx xr--idx))))
                   ;; simplify - group has an implicit seq
                   (operand (if (and (listp group) (eq (car group) 'seq))
                                (cdr group)
                              (list group))))
              (unless (and (< (1+ idx) len)
                           (eq (aref string idx) ?\\)
                           (eq (aref string (1+ idx)) ?\)))
                (xr--error (- group-start 2) (1- (min len idx))
                           "Missing \\)"))
              (setq idx (+ 2 idx))
              (let ((item (cond ((eq submatch 'unnumbered)
                                 (cons 'group operand))
                                (submatch
                                 (append (list 'group-n submatch) operand))
                                (t group))))
                (push item sequence))))

           ;; \{..\} - not special at beginning of sequence or after ^ or \`
           ((eq next-char ?\{)
            (if (or (not sequence)
                    (and (memq (car sequence) '(bol bos))
                         (memq (aref string (1- item-start)) '(?^ ?`))))
                ;; Literal {
                (xr--warn warnings item-start (1+ item-start)
                              (format-message
                               "Escaped non-special character `{'"))

              (setq idx (1+ idx))
              (let ((operand (car sequence)))
                ;; parse bounds
                (let* ((start idx)
                       (i start))
                  (while (and (< i len)
                              (<= ?0 (aref string i) ?9))
                    (setq i (1+ i)))
                  (let ((lower (and (> i start)
                                    (string-to-number
                                     (substring string start i))))
                        (comma nil)
                        (upper nil))
                    (when (and (< i len)
                               (eq (aref string i) ?,))
                      (setq comma t)
                      (setq i (1+ i))
                      (let ((start-u i))
                        (while (and (< i len)
                                    (<= ?0 (aref string i) ?9))
                          (setq i (1+ i)))
                        (setq upper
                              (and (> i start-u)
                                   (string-to-number
                                    (substring string start-u i))))))
                    (setq idx i)
                    (unless (xr--substring-p string idx "\\}")
                      (xr--error (- start 2) (1- idx) "Missing \\}"))
                    (unless (or lower upper)
                      (xr--warn warnings (- start 2) (+ idx 1)
                                    (if comma
                                        "Uncounted repetition"
                                      "Implicit zero repetition")))
                    (setq idx (+ i 2))
                    (setq lower (or lower 0))

                    (unless comma
                      (setq upper lower))
                    (when (and upper (> lower upper))
                      (xr--error start (1- i)
                                 "Invalid repetition interval"))

                    (when warnings
                      (when (or (not upper) (>= upper 2))
                        (xr--check-wrap-around-repetition
                         operand (cadr locations) (1- idx) warnings))

                      (cond
                       ((and (consp operand)
                             (or
                              ;; (** N M (* X)), for any repetition *
                              (memq (car operand)
                                    '(opt zero-or-more one-or-more +? *? ??))
                              ;; (** N M (group (* X))), for any repetition *
                              (and
                               (eq (car operand) 'group)
                               (null (cddr operand))
                               (let ((inner (cadr operand)))
                                 (and (consp inner)
                                      (memq (car inner)
                                            '(opt zero-or-more one-or-more
                                                  +? *? ??)))))))
                        (let ((inner-opt (or (memq (car operand) '(opt ??))
                                             (and (eq (car operand) 'group)
                                                  (memq (caadr operand)
                                                        '(opt ??))))))
                          (xr--warn warnings
                                    item-start (1- idx)
                                    (if inner-opt
                                        "Repetition of option"
                                      "Repetition of repetition")
                                    (cadr locations) (1- item-start)
                                    "This is the inner expression")))
                       ((memq operand xr--zero-width-assertions)
                        (xr--warn warnings
                                  item-start (1- idx)
                                  "Repetition of zero-width assertion"
                                  (cadr locations) (1- item-start)
                                  "Zero-width assertion here"))
                       ((and (xr--matches-empty-p operand)
                             ;; Rejecting repetition of the empty string
                             ;; suppresses some false positives.
                             (not (equal operand "")))
                        (xr--warn
                         warnings item-start (1- idx)
                         "Repetition of expression matching an empty string"
                         (cadr locations) (1- item-start)
                         "This expression matches an empty string"))))
                    (setq sequence (cons (xr--repeat lower upper operand)
                                         (cdr sequence)))
                    (pop locations))))))

           ;; back-reference
           ((memq next-char (eval-when-compile (number-sequence ?1 ?9)))
            (setq idx (1+ idx))
            (push (list 'backref (- next-char ?0))
                  sequence))

           ;; various simple substitutions
           ((memq next-char '(?w ?W ?` ?\' ?= ?b ?B ?< ?>))
            (setq idx (1+ idx))
            (let ((sym (cdr (assq
                             next-char
                             ;; Note that translating \w to wordchar isn't
                             ;; right, since `wordchar' yields [[:word:]] which
                             ;; does not respect syntax properties.
                             ;; We translate \W to (not (syntax word)) for
                             ;; consistency, rather than the confusingly
                             ;; named legacy `not-wordchar'.
                             '((?w . (syntax word)) (?W . (not (syntax word)))
                               (?` . bos) (?\' . eos)
                               (?= . point)
                               (?b . word-boundary) (?B . not-word-boundary)
                               (?< . bow) (?> . eow))))))
              (push sym sequence)))

           ;; symbol-start, symbol-end
           ((eq next-char ?_)
            (setq idx (1+ idx))
            (let* ((c (and (< idx len) (aref string idx)))
                   (sym (cond ((eq c ?<) 'symbol-start)
                              ((eq c ?>) 'symbol-end)
                              (t
                               (xr--error (- idx 2) idx
                                          "Invalid \\_ sequence")))))
              (setq idx (1+ idx))
              (push sym sequence)))

           ;; character syntax
           ((memq next-char '(?s ?S))
            (setq idx (1+ idx))
            (unless (< idx len)
              (xr--error (- idx 2) (1- len)
                         "Incomplete \\%c sequence" next-char))
            (let* ((negated (eq next-char ?S))
                   (syntax-code (aref string idx)))
              (setq idx (1+ idx))
              (let ((sym (assq syntax-code xr--char-syntax-alist)))
                (unless sym
                  (xr--error (- idx 3) (- idx 1)
                             "Unknown syntax code `%s'"
                             (xr--escape-string
                              (char-to-string syntax-code))))
                (push (let ((item (list 'syntax (cdr sym))))
                        (if negated (list 'not item) item))
                      sequence))))

           ;; character categories
           ((memq next-char '(?c ?C))
            (setq idx (1+ idx))
            (unless (< idx len)
              (xr--error (- idx 2) (1- len)
                         "Incomplete \\%c sequence" next-char))
            (let ((negated (eq next-char ?C))
                  (category-code (aref string idx)))
              (setq idx (1+ idx))
              (push (xr--char-category negated category-code)
                    sequence)))


           ;; Escaped character. Only \*+?.^$[ really need escaping.
           (t
            (setq idx (1+ idx))
            (push (char-to-string next-char) sequence)
            (unless (memq next-char '(?\\ ?* ?+ ?? ?. ?^ ?$ ?\[ ?\]))
              ;; Note that we do not warn about \], since the symmetry with \[
              ;; makes it unlikely to be a serious error.
              (xr--warn warnings item-start (1+ item-start)
                        (format-message "Escaped non-special character `%s'"
                                        (xr--escape-string
                                         (char-to-string next-char))))))))

         ;; nonspecial character
         (t
          (setq idx (1+ idx))
          (push (char-to-string next-char) sequence)))

        (when (and (not at-end) warnings (cdr sequence)
                   (not (or (and (< idx len)
                                 (memq (aref string idx) '(?? ?* ?+)))
                            (and (< (1+ idx) len)
                                 (eq (aref string idx) ?\\)
                                 (eq (aref string (1+ idx)) ?\{)))))
          (let* ((item (car sequence))
                 (prev-item (cadr sequence))
                 (subsumption (xr--adjacent-subsumption prev-item item)))
            (when subsumption
              (xr--warn warnings
                        (car locations) (1- idx)
                        (if (eq subsumption 'a-subsumes-b)
                            "Repetition subsumed by preceding repetition"
                          "Repetition subsumes preceding repetition")
                        (cadr locations) (1- (car locations))
                        (if (eq subsumption 'a-subsumes-b)
                            "Subsuming repetition here"
                          "Subsumed repetition here")))

            ;; Check for anchors conflicting with previous/next character.
            ;; To avoid false positives, we require that at least one
            ;; of the items is present in all paths.
            (let ((prev-eol (xr--ends-with-sym 'eol prev-item)))
              (when prev-eol
                (let ((this-nonl (xr--starts-with-nonl item)))
                  (when (and this-nonl
                             (or (eq prev-eol 'always)
                                 (eq this-nonl 'always)))
                    (xr--warn
                     warnings
                     (car locations) (1- idx)
                     "Non-newline follows end-of-line anchor"
                     (cadr locations) (1- (car locations))
                     "This matches at the end of a line")))))
            (let ((this-bol (xr--starts-with-sym 'bol item)))
              (when this-bol
                (let ((prev-nonl (xr--ends-with-nonl prev-item)))
                  (when (and prev-nonl
                             (or (eq prev-nonl 'always)
                                 (eq this-bol 'always)))
                    (xr--warn warnings
                              (car locations) (1- idx)
                              "Line-start anchor follows non-newline"
                              (cadr locations) (1- (car locations))
                              "This matches a non-newline at the end")))))
            (let ((prev-eos (xr--ends-with-sym 'eos prev-item)))
              (when prev-eos
                (let ((this-nonempty (xr--matches-nonempty item)))
                  (when (and this-nonempty
                             (or (eq prev-eos 'always)
                                 (eq this-nonempty 'always)))
                    (xr--warn warnings
                              (car locations) (1- idx)
                              "Non-empty pattern follows end-of-text anchor"
                              (cadr locations) (1- (car locations))
                              "This matches at the end of the text")))))

            ;; FIXME: We don't complain about non-empty followed by
            ;; bos because it may be the start of unmatchable.
            ;; We should really do these checks in a later pass,
            ;; and maintain location information.
            ))))

    (setq xr--idx idx)
    (let ((item-seq (xr--rev-join-seq sequence)))
      (cond ((null item-seq)
             "")
            ((null (cdr item-seq))
             (car item-seq))
            (t 
             (cons 'seq item-seq))))))

;; Our tristate logic: {nil, sometimes, always}
;; ┌─────────┬─────────┬─────────┬─────────┐
;; │A        │B        │A OR B   │A AND* B │
;; ├─────────┼─────────┼─────────┼─────────┤
;; │nil      │nil      │nil      │nil      │
;; │sometimes│nil      │sometimes│sometimes│ <- not nil!
;; │sometimes│sometimes│sometimes│sometimes│
;; │always   │nil      │always   │sometimes│ <- not nil!
;; │always   │sometimes│always   │sometimes│
;; │always   │always   │always   │always   │
;; └─────────┴─────────┴─────────┴─────────┘

(defun xr--tristate-some (f list)
  "Whether F is true for some element in LIST.
Return `always' if F returns `always' for at least one element,
nil if F returns nil for all elements,
`sometimes' otherwise."
  ;; This is the n-ary OR operator in the table above.
  (let ((ret nil))
    (while (and list
                (let ((val (funcall f (car list))))
                  (when val
                    (setq ret val))
                  (not (eq val 'always))))
      (setq list (cdr list)))
    ret))

(defun xr--tristate-all (f list)
  "Whether F is true for all elements in LIST.
Return `always' if F returns `always' for all elements,
otherwise nil if F returns nil for all elements,
`sometimes' otherwise."
  ;; This is the n-ary AND* operator in the table above.
  (if list
      (let ((ret (funcall f (car list))))
        (unless (eq ret 'sometimes)
          (setq list (cdr list))
          (while (and list
                      (or (eq (funcall f (car list)) ret)
                          (progn
                            (setq ret 'sometimes)
                            nil)))
            (setq list (cdr list))))
        ret)
    'always))

(defun xr--matches-nonempty (rx)
  "Whether RX matches non-empty strings. Return `always', `sometimes' or nil.
`always' if RX only matches non-empty strings,
`sometimes' if RX may match a non-empty string,
nil if RX only matches the empty string."
  (pcase rx
    ((pred stringp) (and (> (length rx) 0) 'always))
    (`(,(or 'seq 'one-or-more '+? 'group) . ,body)
     (xr--tristate-some #'xr--matches-nonempty body))
    (`(,(or 'opt 'zero-or-more ?? '*?) . ,body)
     (and (xr--tristate-some #'xr--matches-nonempty body) 'sometimes))
    (`(or . ,body)
     (xr--tristate-all #'xr--matches-nonempty body))
    (`(group-n ,_ . ,body)
     (xr--tristate-some #'xr--matches-nonempty body))
    (`(repeat ,from ,_ . ,body)
     (if (= from 0)
         (and (xr--some #'xr--matches-nonempty body) 'sometimes)
       (xr--tristate-some #'xr--matches-nonempty body)))
    (`(,(or '= '>=) ,n . ,body)
     (if (= n 0)
         (and (xr--some #'xr--matches-nonempty body) 'sometimes)
       (xr--tristate-some #'xr--matches-nonempty body)))
    (`(,(or 'any 'not 'intersection 'syntax 'category) . ,_) 'always)
    ((or 'nonl 'anything (xr--char-class))
     'always)))

(defun xr--starts-with-sym (symbol item)
  "Whether ITEM starts with SYMBOL. Return `always', `sometimes' or nil."
  (cond ((eq item symbol) 'always)
        ((atom item) nil)
        ((memq (car item) '(seq one-or-more +? group))
         (xr--starts-with-sym symbol (cadr item)))
        ((memq (car item) '(seq opt zero-or-more ?? *?))
         (and (xr--starts-with-sym symbol (cadr item)) 'sometimes))
        ((eq (car item) 'group-n)
         (xr--starts-with-sym symbol (caddr item)))
        ((eq (car item) 'or)
         (xr--tristate-all (lambda (x) (xr--starts-with-sym symbol x))
                           (cdr item)))))

(defun xr--ends-with-sym (symbol item)
  "Whether ITEM ends with SYMBOL. Return `always', `sometimes' or nil."
  (cond ((eq item symbol) 'always)
        ((atom item) nil)
        ((memq (car item) '(seq one-or-more +? group group-n))
         (xr--ends-with-sym symbol (car (last item))))
        ((memq (car item) '(seq opt zero-or-more ?? *?))
         (and (xr--ends-with-sym symbol (car (last item))) 'sometimes))
        ((eq (car item) 'or)
         (xr--tristate-all (lambda (x) (xr--ends-with-sym symbol x))
                           (cdr item)))))

(defun xr--starts-with-nonl (item)
  "Whether ITEM starts with a non-newline. Return `always', `sometimes' or nil."
  (pcase item
    ((pred stringp)
     (and (> (length item) 0) (not (eq (aref item 0) ?\n)) 'always))
    (`(,(or 'seq 'one-or-more '+? 'group) ,first . ,_)
     (xr--starts-with-nonl first))
    (`(,(or 'opt 'zero-or-more ?? '*?) ,first . ,_)
     (and (xr--starts-with-nonl first) 'sometimes))
    (`(or . ,items)
     (xr--tristate-all #'xr--starts-with-nonl items))
    (`(group-n ,_ ,first . ,_)
     (xr--starts-with-nonl first))
    (`(,(or '= '>=) ,n ,first . ,_)
     (and (> n 0) (xr--starts-with-nonl first)))
    (`(repeat ,n ,_ ,first . ,_)
     (and (> n 0) (xr--starts-with-nonl first)))
    (`(,(or 'any 'not 'intersection) . ,_)
     (and (xr--superset-p 'nonl item) 'always))
    ((or 'alnum 'alpha 'blank 'digit 'graph
         'lower 'multibyte 'nonascii 'print 'punct
         'upper 'word 'xdigit
         'nonl)
     'always)))

(defun xr--ends-with-nonl (item)
  "Whether ITEM ends with a non-newline. Return `always', `sometimes' or nil."
  (pcase item
    ((pred stringp)
     (and (> (length item) 0) (not (eq (aref item (1- (length item))) ?\n))
          'always))
    (`(,(or 'seq 'one-or-more '+? 'group 'group-n) . ,items)
     (xr--ends-with-nonl (car (last items))))
    (`(,(or 'opt 'zero-or-more ?? '*?) . ,items)
     (and (xr--ends-with-nonl (car (last items))) 'sometimes))
    (`(or . ,items)
     (xr--tristate-all #'xr--starts-with-nonl items))
    (`(,(or '= '>=) ,n . ,items)
     (and (> n 0) (xr--ends-with-nonl (car (last items)))))
    (`(repeat ,n ,_ . ,items)
     (and (> n 0) (xr--ends-with-nonl (car (last items)))))
    (`(,(or 'any 'not 'intersection) . ,_)
     (and (xr--superset-p 'nonl item) 'always))
    ((or 'alnum 'alpha 'blank 'digit 'graph
         'lower 'multibyte 'nonascii 'print 'punct
         'upper 'word 'xdigit
         'nonl)
     'always)))

(defun xr--range-string-to-items (str)
  "Convert a string of ranges to a list of pairs of their endpoints."
  (let ((len (length str))
        (ranges nil)
        (i 0))
    (while (< i len)
      (push (cons (aref str i) (aref str (+ i 2)))
            ranges)
      (setq i (+ i 3)))
    ranges))

(defun xr--any-arg-to-items (arg)
  "Convert an `any' argument to a list of characters, ranges (as pairs),
and classes (symbols)."
  ;; We know (since we built it) that x is either a symbol, string or char,
  ;; and that the string does not mix ranges and chars.
  (cond ((symbolp arg)
         ;; unibyte and multibyte are aliases of ascii and nonascii in
         ;; practice; simplify.
         (list (cond ((eq arg 'unibyte) 'ascii)
                     ((eq arg 'multibyte) 'nonascii)
                     (t arg))))
        ((characterp arg) (list arg))
        ((and (>= (length arg) 3)
              (eq (aref arg 1) ?-))
         (xr--range-string-to-items arg))
        (t (string-to-list arg))))

;; Character class relation matrix
;; Legend:  = same
;;          ⊂ row subset of column
;;          ⊃ row superset of column
;;          x overlapping
;;          ∅ disjoint
;;          ? not certain but very likely
;;          * assuming `case-fold-search' is nil
;;
;;         alp aln dig xdi cnt asc non bla gra pri pun upp low spa wor
;; alpha    =   ⊂   ∅   x   ∅   x   x   ∅   ⊂   ⊂   ∅?  ⊃?  ⊃?  ∅?  ⊂?
;; alnum    ⊃   =   ⊃   ⊃   ∅   x   x   ∅   ⊂   ⊂   ∅?  ⊃?  ⊃?  ∅?  ⊂?
;; digit    ∅   ⊂   =   ⊂   ∅   ⊂   ∅   ∅   ⊂   ⊂   ∅   ∅?  ∅?  ∅?  ⊂?
;; xdigit   x   ⊂   ⊃   =   ∅   ⊂   ∅   ∅   ⊂   ⊂   ∅   x?  x?  ∅?  ⊂?
;; cntrl    ∅   ∅   ∅   ∅   =   ⊂   ∅   x   ∅   ∅   ∅   ∅?  ∅?  x?  ∅?
;; ascii    x   x   ⊃   ⊃   ⊃   =   ∅   x   x   x   x   x?  x?  x?  x?
;; nonascii x   x   ∅   ∅   ∅   ∅   =   x   x   x   x?  x?  x?  x?  x?
;; blank    ∅   ∅   ∅   ∅   x   x   x   =   ∅   x   x?  ∅?  ∅?  x?  ∅?
;; graph    ⊃   ⊃   ⊃   ⊃   ∅   x   x   ∅   =   ⊂   ⊃?  ⊃?  ⊃?  ∅?  ⊃?
;; print    ⊃   ⊃   ⊃   ⊃   ∅   x   x   x   ⊃   =   ⊃?  ⊃?  ⊃?  x?  ⊃?
;; punct    ∅?  ∅?  ∅   ∅   ∅   x   x?  x?  ⊂?  ⊂?  =   ∅?  ∅?  ∅?  x?
;; upper    ⊂?  ⊂?  ∅?  x?  ∅?  x?  x?  ∅?  ⊂?  ⊂?  ∅?  =   ∅*  ∅?  ⊂?
;; lower    ⊂?  ⊂?  ∅?  x?  ∅?  x?  x?  ∅?  ⊂?  ⊂?  ∅?  ∅*  =   ∅?  ⊂?
;; space    ∅?  ∅?  ∅?  ∅?  x?  x?  x?  x?  ∅?  x?  ∅?  ∅?  ∅?  =   ∅
;; word     ⊃?  ⊃?  ⊃?  ⊃?  ∅?  x?  x?  ∅?  ⊂?  ⊂?  x?  ⊃?  ⊃?  ∅   =

(defun xr--any-item-superset-p (a b)
  "Whether A is a superset of B, both being `any' items: a character,
a range (pair of chars), or a class (symbol)."
  (cond
   ((symbolp a)
    (cond
     ((symbolp b)
      (or (eq a b)
          (memq
           b
           (cdr (assq
                 a
                 ;; Class superset matrix: first class in each row is
                 ;; a superset of all the rest in that row.
                 ;; It is slightly approximative, since upper, lower
                 ;; and (partially) punct can be modified through case
                 ;; and syntax tables.
                 '((alpha upper lower)
                   (alnum alpha digit xdigit upper lower)
                   (xdigit digit)
                   (ascii digit xdigit cntrl)
                   (graph alpha alnum digit xdigit punct upper lower word)
                   (print alpha alnum digit xdigit graph punct
                          upper lower word)
                   (word alpha alnum digit xdigit upper lower)))))))

     ((characterp b)
      (cond
       ;; Some reasonable subsets of `space' and `word'.
       ((eq a 'space) (memq b '(?\s ?\t ?\f)))
       ((eq a 'word)
        (string-match-p (rx (any "0-9A-Za-z")) (char-to-string b)))
       ;; Test for invariant classes only. `punct' is invariant for ASCII.
       ;; `upper' and `lower' are not really invariant but mostly.
       ((or (memq a '(digit xdigit cntrl ascii nonascii alpha alnum blank
                            graph print upper lower))
            (and (eq a 'punct) (<= b 127)))
        (string-match-p (format "[[:%s:]]" a) (char-to-string b)))))

     (t   ; b is a range.
      ;; For simplicity, only check ASCII ranges.
      (and (<= (cdr b) 127)
           (xr--some
            (lambda (a-range) (and (<= (car a-range) (car b))
                                   (<= (cdr b) (cdr a-range))))
            (cdr (assq a '((alpha (?A . ?Z) (?a . ?z))
                           (alnum (?0 . ?9) (?A . ?Z) (?a . ?z))
                           (digit (?0 . ?9))
                           (xdigit (?0 . ?9) (?A . ?F) (?a . ?f))
                           (cntrl (0 . 31))
                           (ascii (0 . 127))
                           (graph (33 . 126))
                           (print (32 . 126))
                           (punct (33 . 47) (58 . 64) (91 . 96) (123 . 126))
                           ;; Not-so-wild assumptions.
                           (upper (?A . ?Z))
                           (lower (?a . ?z))
                           (word (?0 . ?9) (?A . ?Z) (?a . ?z))
                           (space (?\s . ?\s) (?\t . ?\t) (?\f . ?\f))))))))))
   
   ((consp a)
    (cond
     ((characterp b) (<= (car a) b (cdr a)))
     ((consp b) (<= (car a) (car b) (cdr b) (cdr a)))
     (t   ; b is a class.
      ;; Only consider classes with simple definitions.
      (let ((b-hull (cdr (assq b '((digit . (?0 . ?9))
                                   (xdigit . (?0 . ?f))
                                   (cntrl . (0 . 31))
                                   (ascii . (0 . 127))
                                   (nonascii . (#x80 . #x10ffff)))))))
        (and b-hull
             (<= (car a) (car b-hull))
             (<= (cdr b-hull) (cdr a)))))))
   (t   ; a is a character.
    (and (characterp b) (eq a b)))))

(defun xr--any-item-may-intersect-p (a b)
  "Whether A intersects B, both being `any' items: a character,
a range (pair of chars), or a class (symbol). If in doubt, return t."
  (cond
   ((symbolp a)
    (cond
     ((symbolp b)
      (or (eq a b)
          (memq
           b
           (cdr (assq
                 a
                 ;; Class intersection matrix: first class in each row
                 ;; intersects all the rest in that row.
                 ;; Again slightly approximate, since upper, lower,
                 ;; space, word and (partially) punct can be modified
                 ;; through syntax and case tables.
                 '((alpha alnum xdigit ascii nonascii graph print
                          upper lower word)
                   (alnum alpha digit xdigit ascii nonascii graph print
                          upper lower word)
                   (digit alnum xdigit ascii graph print word)
                   (xdigit alpha alnum digit ascii graph print
                           upper lower word)
                   (cntrl ascii blank space)
                   (ascii alpha alnum digit xdigit cntrl ascii blank
                          graph print punct upper lower space word)
                   (nonascii alpha alnum blank graph print punct
                             upper lower space word)
                   (blank cntrl ascii nonascii print punct space)
                   (graph alpha alnum digit xdigit ascii nonascii print punct
                          upper lower word)
                   (print alpha alnum digit xdigit ascii nonascii blank graph
                          punct upper lower space word)
                   (punct ascii nonascii blank graph print upper lower word)
                   (upper alpha alnum xdigit ascii nonascii graph print word)
                   (lower alpha alnum xdigit ascii nonascii graph print word)
                   (space cntrl ascii nonascii blank print)
                   (word alpha alnum digit xdigit ascii nonascii graph print
                         punct upper lower)))))))

     ((characterp b)
      (cond
       ;; Some reasonably conservative subsets of `space' and `word'.
       ((eq a 'space)
        (not (<= 33 b 126)))
       ((eq a 'word)
        (not (memq b '(?\s ?\t ?\f ?\r))))
       (t
        ;; Only some classes are invariant. `punct' is invariant for ASCII.
        ;; `upper' and `lower' are not really invariant but mostly.
        (or (and (eq a 'punct) (> b 127))
            ;; This may be a tad slow.
            (string-match-p (format "[[:%s:]]" a) (char-to-string b))))))

     (t   ; b is a range.
      ;; For simplicity, only check ASCII ranges.
      (cond
       ((and (> (cdr b) 127)
             (not (memq a '(cntrl ascii digit xdigit)))))
       ((eq a 'space)
        (not (xr--some (lambda (a-range) (and (<= (car a-range) (cdr b))
                                              (<= (car b) (cdr a-range))))
                       '((?0 . ?9) (?A . ?Z) (?a . ?z)))))
       ((eq a 'word))
       (t
        (xr--some
         (lambda (a-range) (and (<= (car a-range) (cdr b))
                                (<= (car b) (cdr a-range))))
         (cdr (assq a '((alpha (?A . ?Z) (?a . ?z))
                        (alnum (?0 . ?9) (?A . ?Z) (?a . ?z))
                        (digit (?0 . ?9))
                        (xdigit (?0 . ?9) (?A . ?F) (?a . ?f))
                        (cntrl (0 . 31))
                        (ascii (0 . 127))
                        (graph (33 . 126))
                        (print (32 . 126))
                        (punct (33 . 47) (58 . 64) (91 . 96) (123 . 126))
                        ;; Not-so-wild assumptions.
                        (upper (?A . ?Z))
                        (lower (?a . ?z)))))))))))

   ((consp a)
    (cond ((characterp b) (<= (car a) b (cdr a)))
          ((consp b) (and (<= (car a) (cdr b))
                          (<= (car b) (cdr a))))
          (t  ; b is a class
           (xr--any-item-may-intersect-p b a))))
   ;; Now a must be a character.
   ((characterp b) (eq a b))
   (t (xr--any-item-may-intersect-p b a))))

(defun xr--char-superset-of-char-set-p (a-sets negated b-sets)
  "Whether A-SETS, possibly NEGATED, is a superset of B-SETS.
A-SETS and B-SETS are arguments to `any'."
  (let ((a-items (mapcan #'xr--any-arg-to-items a-sets))
        (b-items (mapcan #'xr--any-arg-to-items b-sets)))
    (xr--every (lambda (b-item)
                 (if negated
                     (not (xr--some
                           (lambda (a-item)
                             (xr--any-item-may-intersect-p b-item a-item))
                           a-items))
                   (xr--some (lambda (a-item)
                               (xr--any-item-superset-p a-item b-item))
                             a-items)))
               b-items)))

(defun xr--char-superset-of-rx-p (sets negated rx)
  "Whether SETS, possibly NEGATED, is a superset of RX."
  (pcase rx
    (`(any . ,b-sets)
     (xr--char-superset-of-char-set-p sets negated b-sets))
    (`(not (any . ,b-sets))
     (and negated
          (xr--char-superset-of-char-set-p b-sets nil sets)))
    ((xr--char-class)
     (xr--char-superset-of-char-set-p sets negated (list rx)))
    (`(not ,(and sym (xr--char-class)))
     (and negated
          (xr--char-superset-of-char-set-p (list sym) nil sets)))
    ((pred characterp)
     (xr--char-superset-of-char-set-p sets negated (list rx)))))

(defun xr--single-non-newline-char-p (rx)
  "Whether RX only matches single characters none of which is newline."
  (pcase rx
    ((or 'nonl 'wordchar) t)
    (`(category ,_) t)
    (`(syntax ,s) (not (eq s ?>)))      ; comment-end often matches newline
    (_ (xr--char-superset-of-rx-p '("\n") t rx))))

(defun xr--single-char-p (rx)
  "Whether RX only matches single characters."
  (or (memq rx (eval-when-compile
                 (append '(nonl anything wordchar not-wordchar)
                         xr--char-classes)))
      (characterp rx)
      (and (consp rx)
           (or (memq (car rx) '(any category syntax))
               (and (eq (car rx) 'not)
                    (xr--single-char-p (cadr rx)))))))

(defun xr--syntax-superset-of-rx-p (syntax negated rx)
  "Whether SYNTAX, possibly NEGATED, is a superset of RX."
  (cond
   ((eq syntax 'whitespace) (xr--char-superset-of-rx-p '(space) negated rx))
   ((eq syntax 'word)       (xr--char-superset-of-rx-p '(word) negated rx))
   (t
    ;; Syntax tables vary, but we make a fairly conservative guess.
    (let* ((always-set
            ;; Characters we think always will be in the syntax set.
            '((open-parenthesis "([")
              (close-parenthesis "])")))
           (never-set
            ;; Characters we think never will be in the syntax set.
            '((punctuation "A-Za-z0-9")   ; NOT the same as [:punct:]!
              (open-parenthesis "\000-\037A-Za-z0-9" " \177")
              (close-parenthesis "\000-\037A-Za-z0-9" " \177")))
           (set (assq syntax (if negated never-set always-set))))
      (and set
           (xr--char-superset-of-rx-p (cdr set) nil rx))))))

(defun xr--expand-strings (rx)
  "Expand strings to characters or seqs of characters.
`seq' forms are expanded non-recursively."
  (cond ((consp rx)
         (if (eq (car rx) 'seq)
             (cons 'seq (mapcan (lambda (x)
                                  (if (stringp x)
                                      (string-to-list x)
                                    (list x)))
                                (cdr rx)))
           rx))
        ((stringp rx)
         (if (= (length rx) 1)
             (string-to-char rx)
           (cons 'seq (string-to-list rx))))
        (t rx)))

(defun xr--superset-seq-p (a b)
  "Whether A matches all that B matches, both lists of expressions."
  (while (and a b (xr--superset-p (car a) (car b)))
    (setq a (cdr a))
    (setq b (cdr b)))
  (and (not b)
       (or (not a)
           (xr--matches-empty-p (cons 'seq a)))))

(defun xr--make-seq (body)
  (if (> (length body) 1)
      (cons 'seq body)
    (car body)))

(defun xr--superset-p (a b)
  "Whether A matches all that B matches."
  ;; simple hack that speeds up a fairly common case
  (if (and (stringp a) (stringp b))
      (equal a b)

    (setq a (xr--expand-strings a))
    (setq b (xr--expand-strings b))

    (cond
     ((eq (car-safe b) 'or)
      (xr--every (lambda (b-expr) (xr--superset-p a b-expr)) (cdr b)))
     ((consp a)
      (let ((a-op (car a))
            (a-body (cdr a)))
        (cond
         ((eq a-op 'any)
          (xr--char-superset-of-rx-p a-body nil b))
         ((eq a-op 'not)
          (let ((a-not-arg (nth 1 a)))
            (cond ((eq (car-safe a-not-arg) 'any)
                   (xr--char-superset-of-rx-p (cdr a-not-arg) t b))
                  ((eq (car-safe a-not-arg) 'syntax)
                   (or (equal a b)
                       (xr--syntax-superset-of-rx-p (nth 1 a-not-arg) t b)))
                  ((eq (car-safe a-not-arg) 'category)
                   (or (equal a b)
                       (and (characterp b)
                            (string-match-p (rx-to-string a)
                                            (char-to-string b)))))
                  ((memq a-not-arg (eval-when-compile xr--char-classes))
                   (xr--char-superset-of-rx-p (list a-not-arg) t b))
                  (t (equal a b)))))

         ((eq a-op 'category)
          (or (equal a b)
              (and (characterp b)
                   (string-match-p (rx-to-string a) (char-to-string b)))))

         ((eq a-op 'seq)
          (if (eq (car-safe b) 'seq)
              (let ((b-body (cdr b)))
                (xr--superset-seq-p a-body b-body))
            (xr--superset-seq-p a-body (list b))))

         ((eq a-op 'or)
          (xr--some (lambda (a-expr) (xr--superset-p a-expr b)) a-body))

         ((memq a-op '(zero-or-more *?))
          (if (memq (car-safe b) '(opt zero-or-more one-or-more ?? *? +?))
            (let ((b-body (cdr b)))
              (xr--superset-p (xr--make-seq a-body) (xr--make-seq b-body)))
            (or (equal b '(seq))
                (xr--superset-p (xr--make-seq a-body) b))))
         ((memq a-op '(one-or-more +?))
          (if (memq (car-safe b) '(one-or-more +?))
              (let ((b-body (cdr b)))
                (xr--superset-p (xr--make-seq a-body) (xr--make-seq b-body)))
            (xr--superset-p (xr--make-seq a-body) b)))
         ((memq a-op '(opt ??))
          (if (memq (car-safe b) '(opt ??))
            (let ((b-body (cdr b)))
              (xr--superset-p (xr--make-seq a-body) (xr--make-seq b-body)))
            (or (equal b '(seq))
                (xr--superset-p (xr--make-seq a-body) b))))
         ((eq a-op 'repeat)
          (let ((lo (car a-body))
                (a-body (cddr a-body)))
            (if (<= lo 1)
                (or (and (= lo 0) (equal b '(seq)))
                    (xr--superset-p (xr--make-seq a-body) b))
              (equal a b))))
         
         ;; We do not expand through groups on the subset (b) side to
         ;; avoid false positives; "\\(a\\)\\|." should be without warning.
         ((eq a-op 'group)
          (xr--superset-p (xr--make-seq a-body) b))
         ((eq a-op 'group-n)
          (let ((a-body (cdr a-body)))
            (xr--superset-p (xr--make-seq a-body) b)))

         ((eq a-op 'syntax)
          (or (equal a b) (xr--syntax-superset-of-rx-p (car a-body) nil b)))

         (t (equal a b)))))

     ((memq a (eval-when-compile xr--char-classes))
      (xr--char-superset-of-rx-p (list a) nil b))

     ((eq a 'nonl) (xr--single-non-newline-char-p b))
     ((eq a 'anything) (xr--single-char-p b))

     ((eq a 'wordchar)
      (or (equal a b) (xr--syntax-superset-of-rx-p 'word nil b)))
     ((eq a 'not-wordchar)
      (or (equal a b) (xr--syntax-superset-of-rx-p 'word t b)))

     (t (equal a b)))))

(defun xr--char-alt-equivalent-p (x)
  "Whether X could be expressed as a combinable character alternative."
  ;; We exclude `nonl' because it is either something we warn about anyway
  ;; because of subsumption or patterns like (or nonl "\n") which is just
  ;; a way of expressing `anychar' in a slightly less efficient way.
  ;; We also exclude `not'-forms because they usually don't combine in an
  ;; `or'-expressions to make an `any' form.
  (pcase x
    ((pred stringp) (= (length x) 1))
    ((or (xr--char-class) 'anything)
     t)
    (`(any . ,_) t)
    ;; Assume for this purpose that \sw and \s- are equivalent to
    ;; [[:word:]] and [[:space:]] even though they differ in whether syntax
    ;; properties are respected, because for most uses this doesn't matter.
    (`(syntax ,(or 'word 'whitespace)) t)
    (`(or . ,ys) (xr--every #'xr--char-alt-equivalent-p ys))))

(defun xr--parse-alt (warnings purpose checks)
  (let ((locations (list xr--idx))
        (alternatives nil))             ; reversed
    (push (xr--parse-seq warnings purpose checks) alternatives)
    (while (not (or (>= xr--idx xr--len)
                    (and (eq (aref xr--string xr--idx) ?\\)
                         (< (1+ xr--idx) xr--len)
                         (eq (aref xr--string (1+ xr--idx)) ?\)))))
      (setq xr--idx (+ xr--idx 2))      ; skip \|
      (push xr--idx locations)
      (let ((pos xr--idx)
            (seq (xr--parse-seq warnings purpose checks)))
        (when warnings
          (let ((alts alternatives)
                (locs locations))
            (while
                (and alts
                     (let ((branch (car alts)))
                       (cond
                        ((xr--superset-p seq branch)
                         (let ((duplicate (equal seq branch))
                               (prev-beg (cadr locs))
                               (prev-end (- (car locs) 3)))
                           (xr--warn
                            warnings
                            pos (and (< pos xr--idx) (1- xr--idx))
                            (if duplicate
                                "Duplicated alternative branch"
                              "Branch matches superset of a previous branch")
                            prev-beg (and (>= prev-end prev-beg) prev-end)
                            (if duplicate
                                "Previous occurrence here"
                              "This is the subset branch"))
                           nil))
                        ((xr--superset-p branch seq)
                         (let ((prev-beg (cadr locs))
                               (prev-end (- (car locs) 3)))
                           (xr--warn
                            warnings
                            pos (and (< pos xr--idx) (1- xr--idx))
                            "Branch matches subset of a previous branch"
                            prev-beg (and (>= prev-end prev-beg) prev-end)
                            "This is the superset branch")
                           nil))
                         (t t))))
              (setq locs (cdr locs))
              (setq alts (cdr alts))))
          (when (and (eq checks 'all)
                     (xr--char-alt-equivalent-p (car alternatives))
                     (xr--char-alt-equivalent-p seq))
            (xr--warn
             warnings (nth 1 locations) (1- xr--idx)
             "Or-pattern more efficiently expressed as character alternative"))
           )
        (push seq alternatives)))
    (if (cdr alternatives)
        ;; Simplify (or nonl "\n") to anything
        (if (member alternatives '((nonl "\n") ("\n" nonl)))
            'anything
          (cons 'or (nreverse alternatives)))
      (car alternatives))))

(defun xr--parse (re-string warnings purpose checks)
  (let* ((s (string-to-multibyte re-string))
         (xr--string s)
         (xr--len (length s))
         (xr--idx 0)
         (rx (xr--parse-alt warnings purpose checks)))
    (when (xr--substring-p s xr--idx "\\)")
      (xr--error xr--idx (1+ xr--idx) "Unbalanced \\)"))
    rx))

;; Grammar for skip-set strings:
;;
;; skip-set ::= negated? item* dangling?
;; item     ::= range | single
;; range    ::= single `-' endpoint
;; single   ::= {any char but `\'}
;;            | `\' {any char}
;; endpoint ::= single | `\'
;; dangling ::= `\'
;; negated  ::= `^'
;;
;; Ambiguities in the above are resolved greedily left-to-right.

(defun xr--skip-set-warn (warnings string start is-range format &rest args)
  (let* ((beg start)
         (end (if is-range (+ beg 2) beg)))
    (when (eq (aref string beg) ?\\)
      (setq end (1+ end)))
    (when (and is-range (eq (aref string end) ?\\))
      (setq end (1+ end)))
    (xr--warn warnings beg end
              (xr--escape-string (apply #'format-message format args)))))

(defun xr--parse-skip-set (string warnings)

  ;; An ad-hoc check, but one that catches lots of mistakes.
  (when (and (string-match (rx bos (group "[" (one-or-more anything) "]")
                               (opt (any "+" "*" "?")
                                    (opt "?"))
                               eos)
                           string)
             (not (string-match-p
                   (rx bos "[:" (one-or-more anything) ":]" eos)
                   string)))
    (xr--warn warnings 0 (1- (match-end 1))
                (format-message "Suspect skip set framed in `[...]'")))

  (let* ((intervals nil)
         (classes nil)
         (escaped nil)
         (len (length string))
         (negated (and (> len 0) (eq (aref string 0) ?^)))
         (idx 0)
         (start-pos 0))
    (when negated
      (setq idx (1+ idx))
      (setq start-pos idx))
    (while (< idx len)
      (let ((ch (aref string idx)))
        (cond
         ((and
           (eq ch ?\[)
           (not escaped)
           (< (1+ idx) len)
           (eq (aref string (1+ idx)) ?:)
           (let ((i (xr--string-search ":]" string (+ 2 idx))))
             (and i
                  (let ((sym (intern (substring string (+ idx 2) i))))
                    (unless (memq sym (eval-when-compile xr--char-classes))
                      (xr--error idx (1+ i)
                                 "No character class `[:%s:]'"
                                 (symbol-name sym)))
                    ;; Another useful ad-hoc check.
                    (when (and (> idx 0)
                               (eq (aref string (1- idx)) ?\[)
                               (< (+ i 2) len)
                               (eq (aref string (+ i 2)) ?\]))
                      (xr--warn
                       warnings (1- idx) (+ i 2)
                       (format-message
                        "Suspect character class framed in `[...]'")))
                    (when (memq sym classes)
                      (xr--warn warnings idx (1+ i)
                                    (format-message
                                     "Duplicated character class `[:%s:]'"
                                     sym)))
                    (push sym classes)
                    (setq idx (+ 2 i))
                    t)))))

         ((and (eq ch ?\\) (not escaped))
          (setq idx (1+ idx))
          (if (= idx len)
              (xr--warn warnings (1- idx) (1- idx)
                            (format-message "Stray `\\' at end of string"))
            (setq escaped t)))

         (t 
          (let ((pos (if escaped (1- idx) idx))
                (start ch)
                (end nil))
            (when (and escaped (not (memq start '(?^ ?- ?\\))))
              (xr--warn warnings pos (1+ pos)
                            (xr--escape-string
                             (format-message
                              "Unnecessarily escaped `%c'" start))))

            (setq idx (1+ idx))
            (when (and (< (1+ idx) len) (eq (aref string idx) ?-))
              (setq idx (1+ idx))
              (setq end (aref string idx))
              (setq idx (1+ idx))
              (when (and (eq end ?\\) (< idx len))
                (setq end (aref string idx))
                (setq idx (1+ idx))
                (when (not (memq end '(?^ ?- ?\\)))
                  (xr--warn warnings (- idx 2) (- idx 1)
                                (xr--escape-string
                                 (format-message
                                  "Unnecessarily escaped `%c'" end))))))

            (when (and (eq start ?-)
                       (not end)
                       (not escaped)
                       (< start-pos pos (1- len)))
              (xr--warn warnings pos pos
                            (format-message "Literal `-' not first or last")))
            (if (and end (> start end))
                (xr--warn warnings pos (1- idx)
                              (xr--escape-string
                               (format-message
                                "Reversed range `%c-%c'" start end)))
              (cond
               ((eq start end)
                (xr--warn warnings pos (1- idx)
                              (xr--escape-string
                               (format-message "Single-element range `%c-%c'"
                                               start end))))
               ((eq (1+ start) end)
                (xr--warn warnings pos (1- idx)
                              (xr--escape-string
                               (format-message "Two-element range `%c-%c'"
                                               start end)))))
              (cond
               ((not end)
                (push (vector start start pos) intervals))
               ((<= start #x7f #x3fff80 end)
                ;; Intervals that go from ASCII (0-7f) to raw bytes
                ;; (3fff80-3fffff) always exclude the intervening
                ;; (Unicode) points.
                (push (vector start #x7f pos) intervals)
                (push (vector #x3fff80 end pos) intervals))
               (t
                (push (vector start end pos) intervals))))
            (setq escaped nil))))))

    (when (and (null intervals) (null classes))
      (xr--warn warnings 0 nil
                    (if negated
                        "Negated empty set matches anything"
                      "Empty set matches nothing")))

    (let* ((sorted (sort (nreverse intervals)
                         (lambda (a b) (< (aref a 0) (aref b 0)))))
           (s sorted))
      (while (cdr s)
        (let ((this (car s))
              (next (cadr s)))
          (if (>= (aref this 1) (aref next 0))
              ;; Overlap.
              (let* ((a (if (< (aref this 2) (aref next 2)) this next))
                     (b (if (< (aref this 2) (aref next 2)) next this)))
                (cond
                 ;; Duplicate character: drop it and warn.
                 ((and (eq (aref a 0) (aref a 1))
                       (eq (aref b 0) (aref b 1)))
                  (xr--skip-set-warn warnings string (aref b 2) nil
                                     "Duplicated character `%c'" (aref this 0)))
                 ;; Duplicate range: drop it and warn.
                 ((and (eq (aref a 0) (aref b 0))
                       (eq (aref a 1) (aref b 1)))
                  (xr--skip-set-warn warnings string (aref b 2) t
                                     "Duplicated range `%c-%c'"
                                     (aref b 0) (aref b 1)))
                 ;; Character in range: drop it and warn.
                 ((eq (aref a 0) (aref a 1))
                  (when (eq a this)
                    (setcar s next))
                  (xr--skip-set-warn warnings string (aref b 2) t
                                     "Range `%c-%c' includes character `%c'"
                                     (aref b 0) (aref b 1) (aref a 0)))
                 ;; Same but other way around.
                 ((eq (aref b 0) (aref b 1))
                  (when (eq b this)
                    (setcar s next))
                  (xr--skip-set-warn warnings string (aref b 2) nil
                                     "Character `%c' included in range `%c-%c'"
                                     (aref b 0) (aref a 0) (aref a 1)))
                 ;; Overlapping ranges: merge and warn.
                 (t
                  (let ((this-end (aref this 1)))
                    (aset this 1 (max (aref this 1) (aref next 1)))
                    (xr--skip-set-warn warnings string (aref b 2) t
                                       "Ranges `%c-%c' and `%c-%c' overlap"
                                       (aref this 0) this-end
                                       (aref next 0) (aref next 1)))))
                (setcdr s (cddr s)))
            ;; No overlap.
            (setq s (cdr s)))))

      (let ((ranges nil)
            (chars nil))
        (dolist (interv sorted)
          (if (eq (aref interv 0) (aref interv 1))
              (push (aref interv 0) chars)
            (push (string (aref interv 0) ?- (aref interv 1))
                  ranges)))

        (cond
         ;; Single non-negated character, like "-": make a string.
         ((and (not negated)
               (null classes)
               (null ranges)
               (= (length chars) 1))
          (regexp-quote (char-to-string (car chars))))
         ;; Negated empty set, like "^": anything.
         ((and negated
               (null classes)
               (null intervals))
          'anything)
         ;; Single named class, like "[:nonascii:]": use the symbol.
         ((and (= (length classes) 1)
               (null intervals))
          (if negated
              (list 'not (car classes))
            (car classes)))
         ;; Anything else: produce (any ...)
         (t
          ;; Put a single `-' last.
          (when (memq ?- chars)
            (setq chars (cons ?- (delq ?- chars))))
          (let ((set (cons 'any
                           (append
                            (and ranges
                                 (list (apply #'concat (nreverse ranges))))
                            (and chars
                                 (list (apply #'string (nreverse chars))))
                            (nreverse classes)))))
            (if negated
                (list 'not set)
              set))))))))

(defun xr--substitute-keywords (head-alist body-alist rx)
  "Substitute keywords in RX using HEAD-ALIST and BODY-ALIST in the
head and body positions, respectively."
  (cond
   ((symbolp rx)
    (or (cdr (assq rx body-alist)) rx))
   ((consp rx)
    (cons (or (cdr (assq (car rx) head-alist))
              (car rx))
          (mapcar (lambda (elem) (xr--substitute-keywords
                                  head-alist body-alist elem))
                  (cdr rx))))
   (t rx)))

(defconst xr--keywords
  '((medium . nil)
    (brief . (((zero-or-more . 0+)
               (one-or-more  . 1+))
              . nil))
    (terse . (((seq          . :)
               (or           . |)
               (any          . in)
               (zero-or-more . *)
               (one-or-more  . +)
               (opt          . ? )
               (repeat       . **))
              . nil))
    (verbose . (((opt . zero-or-one))
                .
                ((nonl . not-newline)
                 (bol  . line-start)
                 (eol  . line-end)
                 (bos  . string-start)
                 (eos  . string-end)
                 (bow  . word-start)
                 (eow  . word-end)))))
  "Alist mapping keyword dialect to (HEAD-ALIST . BODY-ALIST),
or to nil if no translation should take place.
The alists are mapping from the default choice.")

(defun xr--in-dialect (rx dialect)
  (let ((keywords (assq (or dialect 'medium) xr--keywords)))
    (unless keywords
      (error "Unknown dialect `%S'" dialect))
    (if (cdr keywords)
        (xr--substitute-keywords (cadr keywords) (cddr keywords) rx)
      rx)))
  
(defmacro xr--error-to-warnings (warnings form)
  "Run FORM, converting parse errors to warnings."
  `(condition-case err
       ,form
     (xr-parse-error
      ;; Add the error to the diagnostics.
      (let ((msg (nth 1 err))
            (beg (nth 2 err))
            (end (nth 3 err)))
        (xr--add-error ,warnings beg end msg)))))

(defalias 'xr--sort-diags
  (if (>= emacs-major-version 30)
      (lambda (diags)
        (with-suppressed-warnings ((callargs sort))  ; hush emacs <30
          (sort diags :key #'caar :in-place t)))
    (lambda (diags) (sort diags (lambda (a b) (< (caar a) (caar b)))))))

;;;###autoload
(defun xr (re-string &optional dialect)
  "Convert a regexp string to rx notation; the inverse of `rx'.
Passing the returned value to `rx' (or `rx-to-string') yields a regexp string
equivalent to RE-STRING.  DIALECT controls the choice of keywords,
and is one of:
`verbose'       -- verbose keywords
`medium' or nil -- somewhat verbose keywords (the default)
`brief'         -- short keywords
`terse'         -- very short keywords"
  (xr--in-dialect (xr--parse re-string nil nil nil) dialect))

;;;###autoload
(defun xr-skip-set (skip-set-string &optional dialect)
  "Convert a skip set string argument to rx notation.
SKIP-SET-STRING is interpreted according to the syntax of
`skip-chars-forward' and `skip-chars-backward' and converted to
a character class on `rx' form.
If desired, `rx' can then be used to convert the result to an
ordinary regexp.
See `xr' for a description of the DIALECT argument."
  (xr--in-dialect (xr--parse-skip-set
                   (string-to-multibyte skip-set-string) nil)
                  dialect))

;;;###autoload
(defun xr-lint (re-string &optional purpose checks)
  "Detect dubious practices and possible mistakes in RE-STRING.
This includes uses of tolerated but discouraged constructs, as well
as outright syntax errors.

If PURPOSE is `file', perform additional checks assuming that RE-STRING
is used to match a file name.

If CHECKS is absent or nil, only perform checks that are very
likely to indicate mistakes; if `all', include all checks,
including ones more likely to generate false alarms.

Return a list of lists of (BEG END COMMENT SEVERITY), where COMMENT
applies at offsets BEG..END inclusive in RE-STRING, and SEVERITY is
`error', `warning' or `info'. The middle list level groups diagnostics
about the same problem."
  (unless (memq purpose '(nil file))
    (error "Bad xr-lint PURPOSE argument: %S" purpose))
  (unless (memq checks '(nil all))
    (error "Bad xr-lint CHECKS argument: %S" checks))
  (let ((warnings (list nil)))
    (xr--error-to-warnings
     warnings (xr--parse re-string warnings purpose checks))
    (xr--sort-diags (car warnings))))

;;;###autoload
(defun xr-skip-set-lint (skip-set-string)
  "Detect dubious practices and possible mistakes in SKIP-SET-STRING.
This includes uses of tolerated but discouraged constructs, as well
as outright syntax errors.
The argument is interpreted according to the syntax of
`skip-chars-forward' and `skip-chars-backward'.

Return a list of lists of (BEG END COMMENT SEVERITY), where COMMENT
applies at offsets BEG..END inclusive in SKIP-SET-STRING, and SEVERITY is
`error', `warning' or `info'. The middle list level groups diagnostics
about the same problem."
  (let ((warnings (list nil)))
    (xr--error-to-warnings
     warnings (xr--parse-skip-set skip-set-string warnings))
    (xr--sort-diags (car warnings))))

(defun xr--escape-string (string &optional escape-printable)
  "Escape non-printing characters in a string for maximum readability.
If ESCAPE-PRINTABLE, also escape \\ and \", otherwise don't."
  (replace-regexp-in-string
   (rx (in "\x00-\x1f" "\x80-\xff" ?\" ?\\ #x7f) (? xdigit))
   (lambda (s)
     (let* ((c (logand (string-to-char s) #xff))
            (xdigit (substring s 1))
            (transl (assq c
                          '((?\b . "\\b")
                            (?\t . "\\t")
                            (?\n . "\\n")
                            (?\v . "\\v")
                            (?\f . "\\f")
                            (?\r . "\\r")
                            (?\e . "\\e")))))
       ;; We prefer hex escapes (\xHH) because that is what most users
       ;; want today, but use octal (\OOO) if the following character
       ;; is a legitimate hex digit.
       (concat
        (cond (transl (cdr transl))
              ((memq c '(?\\ ?\"))
               (if escape-printable (string ?\\ c) (string c)))
              ((zerop (length xdigit)) (format "\\x%02x" c))
              (t (format (format "\\%03o" c))))
        xdigit)))
   string 'fixedcase 'literal))

;; `take' added in Emacs 29
(defalias 'xr--take
  (if (fboundp 'take)
      #'take
    (lambda (n list)
      "The N first elements of LIST."
      (cl-loop repeat n for x in list collect x))))

(defun xr--rx-list-to-string (rx plain-prefix)
  "Print the list `rx' to a string, unformatted.
The first PLAIN-PREFIX elements are formatted using `prin1-to-string';
the rest with `xr--rx-to-string'."
  (concat "("
          (mapconcat #'identity
                     (append
                      (mapcar #'prin1-to-string (xr--take plain-prefix rx))
                      (mapcar #'xr--rx-to-string (nthcdr plain-prefix rx)))
                     " ")
          ")"))

(defun xr--rx-to-string (rx)
  "Print an rx expression to a string, unformatted."
  (cond
   ((eq rx '*?) "*?")                   ; Avoid unnecessary \ in symbol.
   ((eq rx '+?) "+?")
   ((eq rx '\??) "\\??")
   ((stringp rx) (concat "\"" (xr--escape-string rx t) "\""))
   ((characterp rx)
    (let ((esc (assq rx '((?\( . ?\()
                          (?\) . ?\))
                          (?\[ . ?\[)
                          (?\] . ?\])
                          (?\\ . ?\\)
                          (?\; . ?\;)
                          (?\" . ?\")
                          (?\s . ?s)
                          (?\n . ?n)
                          (?\r . ?r)
                          (?\t . ?t)
                          (?\e . ?e)
                          (?\b . ?b)
                          (?\f . ?f)
                          (?\v . ?v)))))
      (cond (esc (format "?\\%c" (cdr esc)))
            ;; Only base characters are displayed as ?char; this excludes
            ;; controls, combining, surrogates, noncharacters etc.
            ((aref (char-category-set rx) ?.) (format "?%c" rx))
            (t (format "#x%02x" rx)))))
   ((atom rx) (prin1-to-string rx))
   ((nlistp (cdr rx))
    (format "(%s . %s)"
            (xr--rx-to-string (car rx))
            (xr--rx-to-string (cdr rx))))
   ((or (eq (car rx) '**)
        (and (eq (car rx) 'repeat) (> (length rx) 3)))
    ;; First 2 args are integers.
    (xr--rx-list-to-string rx 3))
   ((memq (car rx) '(= >= repeat group-n backref))
    ;; First arg is integer.
    (xr--rx-list-to-string rx 2))
   (t
    ;; Render the space character as ? when first in a list.
    ;; Elsewhere, it's a character or integer.
    (let ((first (if (eq (car rx) ?\s)
                     "?"
                   (xr--rx-to-string (car rx))))
          (rest (mapcar #'xr--rx-to-string (cdr rx))))
      (concat "(" (mapconcat #'identity (cons first rest) " ") ")")))))

(defun xr-pp-rx-to-str (rx)
  "Pretty-print the regexp RX (in rx notation) to a string.
It does a slightly better job than standard `pp' for rx purposes."
  (with-temp-buffer
    (insert (xr--rx-to-string rx) "\n")
    (pp-buffer)

    ;; Remove the line break after short operator names for
    ;; readability and compactness.
    (goto-char (point-min))
    (while (re-search-forward
            (rx "(" (** 1 4 (any "a-z0-9" "+?:|*=>"))
                (group "\n" (zero-or-more blank)))
            nil t)
      (replace-match " " t t nil 1))
    
    ;; Reindent the buffer in case line breaks have been removed.
    (goto-char (point-min))
    (indent-sexp)

    (buffer-string)))

;;;###autoload
(defun xr-pp (re-string &optional dialect)
  "Convert to `rx' notation and output the pretty-printed result.
This function uses `xr' to translate RE-STRING into DIALECT.
It is intended for use from an interactive elisp session.
See `xr' for a description of the DIALECT argument."
  (insert (xr-pp-rx-to-str (xr re-string dialect))))

;;;###autoload
(defun xr-skip-set-pp (skip-set-string &optional dialect)
  "Convert a skip set string to `rx' notation and pretty-print.
This function uses `xr-skip-set' to translate SKIP-SET-STRING
into DIALECT.
It is intended for use from an interactive elisp session.
See `xr' for a description of the DIALECT argument."
  (insert (xr-pp-rx-to-str (xr-skip-set skip-set-string dialect))))

(provide 'xr)

;;; xr.el ends here
