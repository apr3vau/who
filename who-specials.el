;;; who-specials.el --- Special variables for WHO. -*- lexical-binding: t -*-
;; See https://github.com/apr3vau/who for more infomations.

;; Thank Edicl for his effort.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(defvar who--prologue
  "<!DOCTYPE html>"
  "This is the first line that'll be printed if the :PROLOGUE keyword
argument is T")

(defvar who--escape-char-p
  (lambda (char)
    (or (cl-find char "<>&'\"")
        (> char 127)))
  "Used by ESCAPE-STRING to test whether a character should be escaped.")

(defvar who--indent nil
  "Whether to insert line breaks and indent. Also controls amount of
indentation dynamically.")

(defvar who--html-mode :html5
  ":SGML for \(SGML-)HTML, :XML \(default) for XHTML, :HTML5 for HTML5.")

(defvar who--empty-attribute-syntax t
  "Set this to t to enable attribute minimization (also called
'boolean attributes', or 'empty attribute syntax' according to the w3
html standard). In XHTML attribute minimization is forbidden, and all
attributes must have a value. Thus in XHTML boolean attributes must be
defined as <input disabled='disabled' />. In HTML5 boolean attributes
can be defined as <input disabled>")

(defvar who--downcase-tokens-p t
  "If NIL, a keyword symbol representing a tag or attribute name will
not be automatically converted to lowercase.  If T, the tag and
attribute name will be converted to lowercase only if it is in the
same case. This is useful when one needs to output case sensitive
XML.")

(defvar who--attribute-quote-char ?\'
  "Quote character for attributes.")

(defvar who--empty-tag-end ">"
  "End of an empty tag.  Default is XML style.")

(defvar who--html-no-indent-tags
  '(:pre :textarea)
  "The list of HTML tags that should disable indentation inside them. The initial
value is a list containing only :PRE and :TEXTAREA.")

(defvar who--html-empty-tags
  '(:area
    :atop
    :audioscope
    :base
    :basefont
    :br
    :choose
    :col
    :command
    :embed
    :frame
    :hr
    :img
    :input
    :isindex
    :keygen
    :left
    :limittext
    :link
    :meta
    :nextid
    :of
    :over
    :param
    :range
    :right
    :source
    :spacer
    :spot
    :tab
    :track
    :wbr)
  "The list of HTML tags that should be output as empty tags.
See *HTML-EMPTY-TAG-AWARE-P*.")

(defvar who--html-empty-tag-aware-p t
  "Set this to NIL to if you want to use CL-WHO as a strict XML
generator.  Otherwise, CL-WHO will only write empty tags listed
in WHO--HTML-EMPTY-TAGS as <tag/> \(XHTML mode) or <tag> \(SGML
mode and HTML5 mode).  For all other tags, it will always generate
<tag></tag>.")

(defconst who--newline (make-string 1 ?\C-j)
  "Used for indentation.")

(provide 'who-specials)

;;; who-specials.el ends here.
