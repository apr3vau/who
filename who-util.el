(require 'cl-macs)

(defmacro who-with-unique-names (bindings &rest body)
  "Syntax: WITH-UNIQUE-NAMES ( { var | (var x) }* ) declaration* form*

Executes a series of forms with each VAR bound to a fresh,
uninterned symbol. The uninterned symbol is as if returned by a call
to GENSYM with the string denoted by X - or, if X is not supplied, the
string denoted by VAR - as argument.

The variable bindings created are lexical unless special declarations
are specified. The scopes of the name bindings and declarations do not
include the Xs.

The forms are evaluated in order, and the values of all but the last
are discarded \(that is, the body is an implicit PROGN)."
  ;; reference implementation posted to comp.lang.lisp as
  ;; <cy3bshuf30f.fsf@ljosa.com> by Vebjorn Ljosa - see also
  ;; <http://www.cliki.net/Common%20Lisp%20Utilities>
  `(let ,(mapcar #'(lambda (binding)
                     (cl-check-type binding (or cons symbol))
                     (if (consp binding)
			 (cl-destructuring-bind (var x) binding
			   (cl-check-type var symbol)
			   `(,var (cl-gensym ,(cl-etypecase x
						(symbol (symbol-name x))
						(character (string x))
						(string x)))))
                       `(,binding (cl-gensym ,(symbol-name binding)))))
                 bindings)
     ,@body))

;; TODO...
(defun who-apply-to-tree (function test tree)
  "Applies FUNCTION recursively to all elements of the tree TREE \(not
only leaves) which pass TEST."
  (cl-declare (optimize speed space))
  (cl-declare (type function function test))
  (cond
   ((funcall test tree)
    (funcall function tree))
   ((consp tree)
    (cons
     (apply-to-tree function test (car tree))
     (apply-to-tree function test (cdr tree))))
   (t tree)))

(defmacro who-n-spaces (n)
  "A string with N spaces - used by indentation."
  `(make-string ,n ?\ ))

(cl-declaim (inline escape-char))
(defun who-escape-char (char &key test)
  "Returns an escaped version of the character CHAR if CHAR satisfies
the predicate TEST.  Always returns a string."
  (cl-declare (optimize speed))
  (unless test (setq test who--escape-char-p))
  (if (funcall test char)
      (cl-case char
	(?\< "&lt;")
	(?\> "&gt;")
	(?\& "&amp;")
	(?\' "&#039;")
	(?\" "&quot;")
	(t (format (if (eq who--html-mode :xml) "&#x~x;" "&#~d;")
		   char)))
    (make-string 1 :initial-element char)))

(defun who-escape-string (string &key test)
  "Escape all characters in STRING which pass TEST. This function is
not guaranteed to return a fresh string.  Note that you can pass NIL
for STRING which'll just be returned."
  (cl-declare (optimize speed))
  (unless test (setq test who--escape-char-p))
  (let ((first-pos (cl-position-if test string))
        (format-string (if (eq who--html-mode :xml) "&#x~x;" "&#~d;")))
    (if (not first-pos)
	;; nothing to do, just return STRING
	string
      (with-output-to-string
	(cl-loop with len = (length string)
		 for old-pos = 0 then (1+ pos)
		 for pos = first-pos
		 then (cl-position-if test string :start old-pos)
		 ;; now the characters from OLD-POS to (excluding) POS
		 ;; don't have to be escaped while the next character has to
		 for char = (and pos (char string pos))
		 while pos
		 do (print (substring string old-pos pos))
		 (case char
		       ((?\<)
			(print "&lt;"))
		       ((?\>)
			(print "&gt;"))
		       ((?\&)
			(print "&amp;"))
		       ((?\')
			(print "&#039;"))
		       ((?\")
			(print "&quot;"))
		       (otherwise
			(format format-string char)))
		 while (< (1+ pos) len)
		 finally (unless pos
			   (print (substring string old-pos))))))))

(defun who-minimal-escape-char-p (char)
  "Helper function for the ESCAPE-FOO-MINIMAL functions to determine
whether CHAR must be escaped."
  (cl-find char "<>&"))

(defun who-escape-char-minimal (char)
  "Escapes only #\<, #\>, and #\& characters."
  (who-escape-char char :test #'who-minimal-escape-char-p))

(defun who-escape-string-minimal (string)
  "Escapes only #\<, #\>, and #\& in STRING."
  (who-escape-string string :test #'who-minimal-escape-char-p))

(defun who-minimal-plus-quotes-escape-char-p (char)
  "Helper function for the ESCAPE-FOO-MINIMAL-PLUS-QUOTES functions to
determine whether CHAR must be escaped."
  (cl-find char "<>&'\""))

(defun who-escape-char-minimal-plus-quotes (char)
  "Like ESCAPE-CHAR-MINIMAL but also escapes quotes."
  (who-escape-char char :test #'who-minimal-plus-quotes-escape-char-p))

(defun who-escape-string-minimal-plus-quotes (string)
  "Like ESCAPE-STRING-MINIMAL but also escapes quotes."
  (who-escape-string string :test #'who-minimal-plus-quotes-escape-char-p))

(defun who-iso-8859-1-escape-char-p (char)
  "Helper function for the ESCAPE-FOO-ISO-8859-1 functions to
determine whether CHAR must be escaped."
  (or (cl-find char "<>&'\"")
      (> char 255)))

(defun who-escape-char-iso-8859-1 (char)
  "Escapes characters that aren't defined in ISO-8859-9."
  (who-escape-char char :test #'who-iso-8859-1-escape-char-p))

(defun who-escape-string-iso-8859-1 (string)
  "Escapes all characters in STRING which aren't defined in ISO-8859-1."
  (who-escape-string string :test #'who-iso-8859-1-escape-char-p))

(defun who-non-7bit-ascii-escape-char-p (char)
  "Helper function for the ESCAPE-FOO-ISO-8859-1 functions to
determine whether CHAR must be escaped."
  (or (cl-find char "<>&'\"")
      (> char 127)))

(defun who-escape-char-all (char)
  "Escapes characters which aren't in the 7-bit ASCII character set."
  (escape-char char :test #'who-non-7bit-ascii-escape-char-p))

(defun who-escape-string-all (string)
  "Escapes all characters in STRING which aren't in the 7-bit ASCII
character set."
  (escape-string string :test #'who-non-7bit-ascii-escape-char-p))

;; We don't support declarations here.

;; (defun who-extract-declarations (forms)
;;   "Given a FORM, the declarations - if any - will be extracted
;;    from the head of the FORM, and will return two values the declarations,
;;    and the remaining of FORM"
;;   (cl-loop with declarations
;; 	   for forms on forms
;; 	   for form = (first forms)
;; 	   while (and (consp form)
;; 		      (eql (first form) 'declare))
;; 	   do (push form declarations)
;; 	   finally (return (values (nreverse declarations) forms))))

(defun who-alpha-char-p (char)
  "The argument must be a character object. ALPHA-CHAR-P returns T if the
argument is an alphabetic character, A-Z or a-z; otherwise NIL."
  (string-match-p "[a-zA-Z]" (string char)))

(defun who-upper-case-p (char)
  "The argument must be a character object; UPPER-CASE-P returns T if the
argument is an upper-case character, NIL otherwise."
  (string-match-p "[A-Z]" (string char)))

(defun who-lower-case-p (char)
  "The argument must be a character object; LOWER-CASE-P returns T if the
argument is a lower-case character, NIL otherwise."
  (string-match-p "[a-z]" (string char)))

(defun who-same-case-p (string)
  "Test if all characters of a string are in the same case."
  (or (cl-every #'(lambda (c) (or (not (who-alpha-char-p c)) (who-lower-case-p c))) string)
      (cl-every #'(lambda (c) (or (not (who-alpha-char-p c)) (who-upper-case-p c))) string)))

(defun who-maybe-downcase (symbol)
  (let* ((origin (symbol-name symbol))
	 (str (if (keywordp symbol) (substring origin 1) origin)))
    (if (and who--downcase-tokens-p (who-same-case-p str))
        (downcase str)
      string)))

(provide 'who-util)
