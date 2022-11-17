(require 'who-specials)
(require 'who-util)

(require 'cl-macs)

(defun who-html-mode ()
  "Returns the current HTML mode. :SGML for (SGML-)HTML, :XML for
XHTML and :HTML5 for HTML5 (HTML syntax).

Using (setf (who-html-mode) MODE) to set the output mode, to
XHTML or (SGML-)HTML.
MODE can be :SGML for HTML, :XML for XHTML or :HTML5 for
HTML5 (HTML syntax)."
  who--html-mode)

(gv-define-setter who-html-mode (mode)
  ;; gv-setter of setf macro for who-html-mode
  `(cl-ecase ,mode
     ((:sgml)
      (setq who--empty-attribute-syntax t
            who--empty-tag-end ">"
            who--prologue "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">"
	    who--html-mode :sgml))
     ((:xml)
      (setq who--empty-attribute-syntax nil
            who--empty-tag-end " />"
            who--prologue "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
	    who--html-mode :xml))
     ((:html5)
      (setq who--empty-attribute-syntax t
            who--empty-tag-end ">"
            who--prologue "<!DOCTYPE html>"
	    who--html-mode :html5))))

(defun who-process-tag (sexp body-fn)
  "Returns a string list corresponding to the `HTML' (in CL-WHO syntax) in SEXP.
Uses the generic function WHO-CONVERT-TO-STRING-LIST internally.
Utility function used by WHO-TREE-TO-TEMPLATE."
  (cl-declare (optimize speed space))
  (let (tag attr-list body)
    (cond
      ((keywordp sexp)
       (setq tag sexp))
      ((atom (cl-first sexp))
       (setq tag (cl-first sexp))
       ;; collect attribute/value pairs into ATTR-LIST and tag body (if
       ;; any) into BODY
       (cl-loop for rest on (cdr sexp) by #'cddr
		if (keywordp (cl-first rest))
		collect (cons (cl-first rest) (cl-second rest)) into attr
		else
		do (progn (setq attr-list attr)
			  (setq body rest)
			  (cl-return))
		finally (setq attr-list attr)))
      ((listp (cl-first sexp))
       (setq tag (cl-first (cl-first sexp)))
       (cl-loop for rest on (cdr (cl-first sexp)) by #'cddr
		if (keywordp (cl-first rest))
		collect (cons (cl-first rest) (cl-second rest)) into attr
		finally (setq attr-list attr))
       (setq body (cdr sexp))))
    (who-convert-tag-to-string-list tag attr-list body body-fn)))

(defun constantp (form &optional environment)
  "A simple simulation of 'constantp' in Common Lisp.

True of any FORM that has a constant value: self-evaluating objects,
keywords, defined constants, quote forms. Additionally the
constant-foldability of some function calls and special forms is recognized.
ENVIRONMENT is ignored."
  (ignore-errors (if (equal form (eval form)))))

(defun who-convert-attributes (attr-list)
  "Helper function for WHO-CONVERT-TAG-TO-STRING-LIST which converts the
alist ATTR-LIST of attributes into a list of strings and/or Lisp
forms."
  (cl-declare (optimize speed space))
  (cl-loop with =var= = (gensym)
	   for cons in attr-list
	   for attr = (who-maybe-downcase (car cons))
	   unless (null (cdr cons)) ;; no attribute at all if (CDR CONS) is NIL
	   if (constantp (cdr cons))
	   if (and who--empty-attribute-syntax (eq (cdr cons) t)) ; special case for SGML and HTML5
	   nconc (list " " attr)
	   else
	   nconc (list " "
		       ;; name of attribute
		       attr
		       (format "=%c" who--attribute-quote-char)
		       ;; value of attribute
		       (cond ((eq (cdr cons) t)
			      ;; (CDR CONS) is T, use attribute's name
			      attr)
			     (t
			      ;; constant form, PRINC it -
			      ;; EVAL is OK here because of CONSTANTP
			      (format "%s" (eval (cdr cons)))))
		       (string who--attribute-quote-char))
	   end
	   else
	   ;; do the same things as above but at runtime
	   nconc (list `(let ((,=var= ,(cdr cons)))
			  (cond ((null ,=var=))
				((eq ,=var= t)
				 ,(if who--empty-attribute-syntax
				      `(fmt " %s" ,attr)
				    `(fmt " %s=%c%s%c"
					  ,attr
					  who--attribute-quote-char
					  ,attr
					  who--attribute-quote-char)))
				(t
				 (fmt " %s=%c%s%c"
				      ,attr
				      who--attribute-quote-char
				      ,=var=
				      who--attribute-quote-char)))))))

(defgeneric who-convert-tag-to-string-list (tag attr-list body body-fn)
  (:documentation "Used by WHO-PROCESS-TAG to convert `HTML' into a list
of strings.  TAG is a keyword symbol naming the outer tag, ATTR-LIST
is an alist of its attributes (the car is the attribute's name as a
keyword, the cdr is its value), BODY is the tag's body, and BODY-FN is
a function which should be applied to BODY.  The function must return
a list of strings or Lisp forms."))

(defmethod who-convert-tag-to-string-list (tag attr-list body body-fn)
  "The standard method which is not specialized.  The idea is that you
can use EQL specializers on the first argument."
  (cl-declare (optimize speed space))
  (let ((tag (who-maybe-downcase tag))
        (body-indent
          ;; increase WHO--INDENT by 2 for body -- or disable it
         (when (and who--indent
		    (not (cl-member tag who--html-no-indent-tags
				    :test #'string-equal)))
           (+ 2 who--indent))))
    (nconc
     (if who--indent
       ;; indent by WHO--INDENT spaces
       (list who--newline (who-n-spaces who--indent)))
     ;; tag name
     (list "<" tag)
     ;; attributes
     (who-convert-attributes attr-list)
     ;; body
     (if body
       (append
        (list ">")
        ;; now hand over the tag's body to TREE-TO-TEMPLATE
        (let ((who--indent body-indent))
          (funcall body-fn body))
        (when body-indent
          ;; indentation
          (list who--newline (who-n-spaces who--indent)))
        ;; closing tag
        (list "</" tag ">"))
       ;; no body, so no closing tag unless defined in WHO--HTML-EMPTY-TAGS
       (if (or (not who--html-empty-tag-aware-p)
               (cl-member tag who--html-empty-tags :test #'string-equal))
         (list who--empty-tag-end)
         (list ">" "</" tag ">"))))))

(defun who-tree-to-template (tree)
  "Transforms an HTML tree into an intermediate format - mainly a
flattened list of strings. Utility function used by TREE-TO-COMMANDS-AUX."
  (cl-loop for element in tree
	   if (or (keywordp element)
		  (and (listp element)
		       (keywordp (cl-first element)))
		  (and (listp element)
		       (listp (cl-first element))
		       (keywordp (cl-first (cl-first element)))))
	   ;; the syntax for a tag - process it
	   nconc (who-process-tag element #'who-tree-to-template)
	   ;; list - insert as sexp
	   else if (consp element)
	   collect `(let ((who--indent ,who--indent))
		      nil ;; If the element is (cl-declare ...) it
		      ;; won't be interpreted as a declaration and an
		      ;; appropriate error could be signaled
		      ,element)
	   ;; something else - insert verbatim
	   else
	   collect element))

(defun who-string-list-to-string (string-list)
  "Concatenates a list of strings to one string."
  ;; We can use 'concat' here in Emacs because Emacs has no CALL-ARGUMENTS-LIMIT
  (apply #'concat string-list))

(defun who-conc (&rest string-list)
  "Concatenates all arguments which should be string into one string."
  (funcall #'string-list-to-string string-list))

(defun who-tree-to-commands (tree printcharfun &rest keys)
  (cl-declare (optimize speed space))
  (let ((prologue (plist-get keys :prologue))
	(indent (plist-get keys :indent)))
    (setq who--indent indent)
    (when (and who--indent
               (not (integerp who--indent)))
      (setq who--indent 0))
    (let ((in-string-p t)
          collector
          string-collector
          (template (who-tree-to-template tree)))
      (when prologue
	(push who--newline template)
	(when (eq prologue t)
          (setq prologue who--prologue))
	(push prologue template))
      (cl-flet ((emit-string-collector
		 ()
		 "Generate a PRINC statement for what is currently in
STRING-COLLECTOR."
		 (list 'princ
		       (who-string-list-to-string (nreverse string-collector))
		       printcharfun)))
	(dolist (element template)
          (cond ((and in-string-p (stringp element))
		 ;; this element is a string and the last one
		 ;; also was (or this is the first element) -
		 ;; collect into STRING-COLLECTOR
		 (push element string-collector))
		((stringp element)
		 ;; the last one wasn't a string so we start
		 ;; with an empty STRING-COLLECTOR
		 (setq string-collector (list element)
                       in-string-p t))
		(string-collector
		 ;; not a string but STRING-COLLECTOR isn't
		 ;; empty so we have to emit the collected
		 ;; strings first
		 (push (emit-string-collector) collector)
		 (setq in-string-p nil
                       string-collector '())
		 (push element collector))
		(t
		 ;; not a string and empty STRING-COLLECTOR
		 (push element collector))))
	(if string-collector
            ;; finally empty STRING-COLLECTOR if
            ;; there's something in it
            (nreverse (cons (emit-string-collector)
                            collector))
          (nreverse collector))))))

(defmacro who-with-html-output (var &rest body)
  "Transform the enclosed BODY consisting of HTML as s-expressions
into Lisp code to write the corresponding HTML as strings using
PRINC.

Using PRINTCHARFUN to specify output. For more information, see
'princ'.

\(fn (PRINTCHARFUN &KEY PROLOGUE INDENT) BODY...)"
  (cl-declare (ignore prologue))
  (let ((printcharfun (car var))
	(prologue (plist-get (cdr var) :prologue))
	(indent (plist-get (cdr var) :indent)))
    `(cl-macrolet ((htm (&body body)
			`(who-with-html-output
			  (,',printcharfun :indent ,,indent)
			  ,@body))
		   (fmt (&rest args)
			`(princ (format ,@args) ,',printcharfun))
		   (esc (thing)
			(who-with-unique-names
			 (list result)
			 `(let ((,result ,thing))
			    (when ,result (princ (who-escape-string ,result)
						 ,',printcharfun)))))
		   (str (thing)
			(who-with-unique-names
			 (list result)
			 `(let ((,result ,thing))
			    (when ,result (princ ,result ,',printcharfun))))))
       ,@(apply 'who-tree-to-commands body var))))

(defmacro who-with-html-output-to-string (var &rest body)
  "Transform the enclosed BODY consisting of HTML as s-expressions
into Lisp code which creates the corresponding HTML as a string.

\(fn (&KEY PROLOGUE INDENT) BODY...)"
  `(with-output-to-string
     (who-with-html-output (nil ,@var) ,@body)))

;; stuff for Nikodemus Siivola's HYPERDOC
;; see <http://common-lisp.net/project/hyperdoc/>
;; and <http://www.cliki.net/hyperdoc>
;; also used by LW-ADD-ONS

(defvar who-hyperdoc-base-uri "http://weitz.de/cl-who/")

(provide 'who)
