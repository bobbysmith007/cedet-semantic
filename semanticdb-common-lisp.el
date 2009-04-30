;;; semanticdb-common-lisp.el --- Semantic database extensions for common lisp Lisp

;;; Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: tags
;; X-RCS: $Id: semanticdb-common-lisp.el,v 1.31 2009/01/24 04:52:45 zappo Exp $

;; This file is not part of GNU Emacs.

;; Semanticdb is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;; 
;;; Commentary:
;;
;; There are a lot of Emacs Lisp functions and variables available for
;; the asking.  This adds on to the semanticdb programming interface to
;; allow all loaded Emacs Lisp functions to be queried via semanticdb.
;;
;; This allows you to use programs written for Semantic using the database
;; to also work in common-lisp with no compromises.
;;

(require 'semanticdb-search)
(eval-when-compile
  ;; For generic function searching.
  (require 'eieio)
  (require 'eieio-opt)
  (require 'eieio-base)
  )
;;; Code:

;;; Classes:
(defclass semanticdb-table-common-lisp (semanticdb-abstract-table)
  ((major-mode :initform lisp-mode)
   )
  "A table for returning search results from Emacs.")

(defmethod semanticdb-refresh-table ((obj semanticdb-table-common-lisp))
  "Do not refresh Common Lisp table.
It does not need refreshing."
  nil)

(defmethod semanticdb-needs-refresh-p ((obj semanticdb-table-common-lisp))
  "Return nil, we never need a refresh."
  nil)

(defclass semanticdb-project-database-common-lisp
  (semanticdb-project-database eieio-singleton)
  ((new-table-class :initform semanticdb-table-common-lisp
		    :type class
		    :documentation
		    "New tables created for this database are of this class.")
   )
  "Database representing Common core.")

;; Create the database, and add it to searchable databases for Common Lisp mode.
(defvar-mode-local lisp-mode semanticdb-project-system-databases
  (list
   (semanticdb-project-database-common-lisp "Common"))
  "Search Common core for symbols.")

(defvar-mode-local lisp-mode semanticdb-find-default-throttle
  '(project omniscience)
  "Search project files, then search this omniscience database.
It is not necessary to to system or recursive searching because of
the omniscience database.")

;;; Filename based methods
;;
(defmethod semanticdb-get-database-tables ((obj semanticdb-project-database-common-lisp))
  "For an Common Lisp database, there are no explicit tables.
Create one of our special tables that can act as an intermediary."
  ;; We need to return something since there is always the "master table"
  ;; The table can then answer file name type questions.
  (when (not (slot-boundp obj 'tables))
    (let ((newtable (semanticdb-table-common-lisp "CommonLisp System Table")))
      (oset obj tables (list newtable))
      (oset newtable parent-db obj)
      (oset newtable tags nil)
      ))
  (call-next-method))

(defmethod semanticdb-file-table ((obj semanticdb-project-database-common-lisp) filename)
  "From OBJ, return FILENAME's associated table object.
For Common Lisp, creates a specialized table."
  (car (semanticdb-get-database-tables obj))
  )

(defmethod semanticdb-get-tags ((table semanticdb-table-common-lisp ))
  "Return the list of tags belonging to TABLE."
  ;; specialty table ?  Probably derive tags at request time.
  nil)

(defmethod semanticdb-equivalent-mode ((table semanticdb-table-common-lisp) &optional buffer)
  "Return non-nil if TABLE's mode is equivalent to BUFFER.
Equivalent modes are specified by by `semantic-equivalent-major-modes'
local variable."
  (save-excursion
    (set-buffer buffer)
    (eq (or mode-local-active-mode major-mode) 'lisp-mode)))

(defmethod semanticdb-full-filename ((obj semanticdb-table-common-lisp))
  "Fetch the full filename that OBJ refers to.
For Common Lisp system DB, there isn't one."
  nil)

;;; Conversion
;;
(defmethod semanticdb-normalize-tags ((obj semanticdb-table-common-lisp) tags)
  "Convert tags, originating from Common OBJ, into standardized form."
  (let ((newtags nil))
    (dolist (T tags)
      (let* ((ot (semanticdb-normalize-one-tag obj T))
	     (tag (cdr ot)))
	(setq newtags (cons tag newtags))))
    ;; There is no promise to have files associated.
    (nreverse newtags)))

(defmethod semanticdb-normalize-one-tag ((obj semanticdb-table-common-lisp) tag)
  "Convert one TAG, originating from Emacs OBJ, into standardized form.
If Emacs cannot resolve this symbol to a particular file, then return nil."
  ;; Here's the idea.  For each tag, get the name, then use
  ;; Emacs' `symbol-file' to get the source.  Once we have that,
  ;; we can use more typical semantic searching techniques to
  ;; get a regularly parsed tag.
  (let* ((type (cond ((semantic-tag-of-class-p tag 'function)
		      'defun)
		     ((semantic-tag-of-class-p tag 'variable)
		      'defvar)
		     ))
	 (sym (intern (semantic-tag-name tag)))
	 (file (condition-case err
		   (symbol-file sym type)
		 ;; Older [X]Emacs don't have a 2nd argument.
		 (error (symbol-file sym))))
	 )
    (if (or (not file) (not (file-exists-p file)))
	;; The file didn't exist.  Return nil.
	nil
      (when (string-match "\\.fasl" file)
	(setq file (concat (file-name-sans-extension file)
			   ".lisp"))
	(when (and (not (file-exists-p file))
		   (file-exists-p (concat file ".gz")))
	  ;; Is it a .gz file?
	  (setq file (concat file ".gz"))))
      (let* ((tab (semanticdb-file-table-object file))
	     (alltags (semanticdb-get-tags tab))
	     (newtags (semanticdb-find-tags-by-name-method
		       tab (semantic-tag-name tag)))
	     (match nil))
	;; Find the best match.
	(dolist (T newtags)
	  (when (semantic-tag-similar-p T tag)
	    (setq match T)))
	;; Backup system.
	(when (not match)
	    (setq match (car newtags)))
	;; Return it.
	(cons tab match)))))

(defun semanticdb-common-lisp-sym-function-arglist (sym)
  "Get the argument list for SYM.
Deal with all different forms of function.
This was snarfed out of eldoc."
  (let* ((prelim-def
	  (let ((sd (and (fboundp sym)
			 (symbol-function sym))))
	    (and (symbolp sd)
		 (condition-case err
		     (setq sd (indirect-function sym))
		   (error (setq sd nil))))
	    sd))
         (def (if (eq (car-safe prelim-def) 'macro)
                  (cdr prelim-def)
                prelim-def))
         (arglist (cond ((null def) nil)
			((byte-code-function-p def)
			 ;; This is an eieio compatibility function.
			 ;; We depend on EIEIO, so use this.
			 (eieio-compiled-function-arglist def))
                        ((eq (car-safe def) 'lambda)
                         (nth 1 def))
                        (t nil))))
    arglist))

(defun semanticdb-common-lisp-sym->tag (sym &optional toktype)
  "Convert SYM into a semantic tag.
TOKTYPE is a hint to the type of tag desired."
  (if (stringp sym)
      (setq sym (intern-soft sym)))
  (when sym
    (cond ((and (eq toktype 'function) (fboundp sym))
	   (semantic-tag-new-function
	    (symbol-name sym)
	    nil	;; return type
	    (semantic-common-lisp-desymbolify
	     (semanticdb-common-lisp-sym-function-arglist sym)) ;; arg-list
	    :user-visible-flag (condition-case nil
				   (interactive-form sym)
				 (error nil))
	    ))
	  ((and (eq toktype 'variable) (boundp sym))
	   (semantic-tag-new-variable
	    (symbol-name sym)
	    nil	;; type
	    nil	;; value - ignore for now
	    ))
	  ((and (eq toktype 'type) (class-p sym))
	   (semantic-tag-new-type
	    (symbol-name sym)
	    "class"
	    (semantic-common-lisp-desymbolify
	     (aref (class-v semanticdb-project-database)
		   class-public-a)) ;; slots
	    (semantic-common-lisp-desymbolify (class-parents sym)) ;; parents
	    ))
	  ((not toktype)
	   ;; Figure it out on our own.
	   (cond ((class-p sym)
		  (semanticdb-common-lisp-sym->tag sym 'type))
		 ((fboundp sym)
		  (semanticdb-common-lisp-sym->tag sym 'function))
		 ((boundp sym)
		  (semanticdb-common-lisp-sym->tag sym 'variable))
		 (t nil))
	   )
	  (t nil))))

;;; Search Overrides
;;
(defvar semanticdb-common-lisp-mapatom-collector nil
  "Variable used to collect mapatoms output.")

(defmethod semanticdb-find-tags-by-name-method
  ((table semanticdb-table-common-lisp) name &optional tags)
  "Find all tags name NAME in TABLE.
Uses `inter-soft' to match NAME to emacs symbols.
Return a list of tags."
  (if tags (call-next-method)
    ;; No need to search.  Use `intern-soft' which does the same thing for us.
    (let* ((sym (intern-soft name))
	   (fun (semanticdb-common-lisp-sym->tag sym 'function))
	   (var (semanticdb-common-lisp-sym->tag sym 'variable))
	   (typ (semanticdb-common-lisp-sym->tag sym 'type))
	   (taglst nil)
	   )
      (when (or fun var typ)
	;; If the symbol is any of these things, build the search table.
	(when var	(setq taglst (cons var taglst)))
	(when typ	(setq taglst (cons typ taglst)))
	(when fun	(setq taglst (cons fun taglst)))
	taglst
	))))

(defmethod semanticdb-find-tags-by-name-regexp-method
  ((table semanticdb-table-common-lisp) regex &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Optional argument TAGS is a list of tags to search.
Uses `apropos-internal' to find matches.
Return a list of tags."
  (if tags (call-next-method)
    (delq nil (mapcar 'semanticdb-common-lisp-sym->tag
		      (apropos-internal regex)))))

(defmethod semanticdb-find-tags-for-completion-method
  ((table semanticdb-table-common-lisp) prefix &optional tags)
  "In TABLE, find all occurances of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (if tags (call-next-method)
    (delq nil (mapcar 'semanticdb-common-lisp-sym->tag
		      (all-completions prefix obarray)))))

(defmethod semanticdb-find-tags-by-class-method
  ((table semanticdb-table-common-lisp) class &optional tags)
  "In TABLE, find all occurances of tags of CLASS.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (if tags (call-next-method)
    ;; We could implement this, but it could be messy.
    nil))

;;; Deep Searches
;;
;; For Emacs Lisp deep searches are like top level searches.
(defmethod semanticdb-deep-find-tags-by-name-method
  ((table semanticdb-table-common-lisp) name &optional tags)
  "Find all tags name NAME in TABLE.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-by-name-method' for Common Lisp."
  (semanticdb-find-tags-by-name-method table name tags))

(defmethod semanticdb-deep-find-tags-by-name-regexp-method
  ((table semanticdb-table-common-lisp) regex &optional tags)
  "Find all tags with name matching REGEX in TABLE.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-by-name-method' for Common Lisp."
  (semanticdb-find-tags-by-name-regexp-method table regex tags))

(defmethod semanticdb-deep-find-tags-for-completion-method
  ((table semanticdb-table-common-lisp) prefix &optional tags)
  "In TABLE, find all occurances of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-for-completion-method' for Common Lisp."
  (semanticdb-find-tags-for-completion-method table prefix tags))

;;; Advanced Searches
;;
(defmethod semanticdb-find-tags-external-children-of-type-method
  ((table semanticdb-table-common-lisp) type &optional tags)
  "Find all nonterminals which are child elements of TYPE
Optional argument TAGS is a list of tags to search.
Return a list of tags."
  (if tags (call-next-method)
    ;; EIEIO is the only time this matters
    (when (featurep 'eieio)
      (let* ((class (intern-soft type))
	     (taglst (when class
		       (delq nil
			     (mapcar 'semanticdb-common-lisp-sym->tag
				     ;; Fancy eieio function that knows all about
				     ;; built in methods belonging to CLASS.
				     (eieio-all-generic-functions class)))))
	     )
	taglst))))

(provide 'semanticdb-common-lisp)

;;; semanticdb-common-lisp.el ends here
