;;; yap-ts-mode.el --- tree-sitter support for Prolog  -*- lexical-binding: t; -*-


;; Copyright (C) 2022-2023 Free Software Foundation, Inc.

;; Author     : Randy Taylor <dev@rjt.dev>
;; Maintainer : Randy Taylor <dev@rjt.dev>
;; Created    : December 2022
;; Keywords   : prolog languages tree-sitter

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:

(require 'treesit)
(eval-when-compile (require 'rx))

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-induce-sparse-tree "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-type "treesit.c")

(defcustom prolog-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `prolog-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'prolog)



(defvar prolog-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?` "\"" table)

    ;; Any better way to handle the 0'<char> construct?!?
    (modify-syntax-entry ?% "<" table)
    (modify-syntax-entry ?\n ">" table)
      ;; Emacs wants to see this it seems:
      (modify-syntax-entry ?* ". 23b" table)
      (modify-syntax-entry ?/ ". 14" table)
    table)
  "Syntax table for `prolog-ts-mode'.")


(defvar prolog-ts-mode--operators
  '("=" "-" "*" "/" "+" "%" "~" "|" "&" "^" "<<" ">>" "->"
    "." "<" "<=" ">=" ">" "==" "!=" "!" "&&" "||" "-="
    "+=" "*=" "/=" "%=" "|=" "&=" "^=" ">>=" "<<=" "--" "++")
  "Prolog operators for tree-sitter font-locking.")

(defvar prolog-ts-mode--brackets
  '("{" "}" "," ";" "[" "]" "(" ")")
  "Prolog operators for tree-sitter font-locking.")


(defvar prolog-ts-mode--indent-rules
  `((prolog
     ((node-is "predicate_definition") column-0 0)
     ((node-is "directive") column-0 0)
     ((parent-is "predicate_definition") parent-bol prolog-ts-mode-indent-offset)
     ((parent-is "inner_goal") parent-bol 2)
     ;;     ((grandparent-is "inner_goal") parent-bol 2)
     ((node-is "inner_goal") parent-bol 2)
     ((parent-is "body") parent-bol 0)

    ;; ((node-is "goal") prev-line 0)
    ;;  ((node-is "semic")  parent 0)
    ;;  ((node-is "rightarrow")   parent 0)
    ;;  ((node-is "close_b")   parent 0)
    ;;  ((parent-is "disj")  parent 2)
    ;;  ((node-is "bracketed_term")  parent 2)
    ;;  ((node-is "body")  parent-bol prolog-ts-mode-indent-offset)
    ;;  ((parent-is "list")  parent-bol 2)
    ;;  ((parent-is "arg")  parent-bol 0)
    ;;  ((node-is "close_list")  first-sibling 1)
     ((catch-all)   prolog-ts-mode-indent-offset 0)
     ))
    "Tree-sitter indent rules for `prolog-ts-mode'.")

(defvar prolog-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'prolog
   :feature 'bracket
   '((["(" "{"  "}" ")"]) @font-lock-bracket-face)

   ;; :language 'prolog
   ;; :feature 'builtin
   ;; `(((foreach_command
   ;;     ((argument) @font-lock-constant-face
   ;;      (:match ,(rx-to-string
   ;;                `(seq bol
   ;;                      (or ,@prolog-ts-mode--foreach-options)
   ;;                      eol))
   ;;              @font-lock-constant-face))))
   ;;   ((if_command
   ;;     ((argument) @font-lock-constant-face
   ;;      (:match ,(rx-to-string
   ;;                `(seq bol
   ;;                      (or ,@prolog-ts-mode--if-conditions)
   ;;                      eol))
   ;;              @font-lock-constant-face)))))

   :language 'prolog
   :feature 'comment
   '((comment) @font-lock-comment-face)

   ;; :language 'prolog
   ;; :feature 'operator
   ;; `([,@prolog-ts-mode--operators] @font-lock-bracket-face)

   ;; :language 'prolog
   ;; :feature 'bracket
   ;; `([,@prolog-ts-mode--brackets] @font-lock-bracket-face)


   ;; :language 'prolog
   ;; :feature 'constant
   ;; `(((argument) @font-lock-constant-face
   ;;    (:match ,(rx-to-string
   ;;              `(seq bol
   ;;                    (or ,@prolog-ts-mode--constants)
   ;;                    eol))
   ;;            @font-lock-constant-face)))

   ;; :language 'prolog
   ;; :feature 'functor
   ;; '((functor) @font-lock-constant-face)

   
   :language 'prolog
   :feature 'function-call
   '((pred_name) @font-lock-function-call-face)
  
   ;; :language 'prolog
   ;; :feature 'function
   ;; '((head_atom) @font-lock-function-name-face)

   :language 'prolog
   :feature 'eot
   `((eot) @font-lock-warning-face)

   :language 'prolog
   :feature 'builtin
   `((builtin) @font-lock-builtin-face)

   :language  'prolog
   :feature 'number
   '((number) @font-lock-number-face)

   :language 'prolog
   :feature 'string
   '([(quoted_atom) (string) (codes)] @font-lock-string-face)

   ;; :language 'prolog
   ;; :feature 'escape-sequence
   ;; :override t
   ;; '((escape_sequence) @font-lock-escape-face)

   :language 'prolog
   :feature 'function
   ;; Don't override strings.
   :override t
   '((head (goal (pred-name))) @font-lock-keyword-face)

   :language 'prolog
   :feature 'variable
   '((variable) @font-lock-variable-face)

   :language 'prolog
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face)
   )
  "Tree-sitter font-lock settings for `prolog-ts-mode'."
  )


;;;###autoload
(define-derived-mode prolog-ts-mode prog-mode "Prolog"
  "Major mode for editing Prolog files, powered by tree-sitter."
  :group 'prolog
  :syntax-table prolog-ts-mode--syntax-table

  (when (treesit-ready-p 'prolog)
    (treesit-parser-create 'prolog)


;; Imenu.
;;  (setq-local treesit-defun-type-regexp
    ;;  (regexp-opt '("head")))

(defun prolog-ts-mode--imenu ()
  "Return Imenu alist for the current buffer."
  (let* ((node (treesit-buffer-root-node))
         (pred-tree (treesit-induce-sparse-tree
                     node "head_atom" nil 1000))
         (pred-index (prolog-ts-mode--imenu-1 pred-tree)))
    (append
     (when pred-index `(("Predicates" . ,pred-index))))))

(defun prolog-ts-mode--imenu-1 (node)
  "Helper for `prolog-ts-mode--imenu'.
Find string representation for NODE and set marker, then recurse
the subtrees."
  (let* ((ts-node (car node))
         (children (cdr node))
         (subtrees (mapcan #'prolog-ts-mode--imenu-1
                           children))
         (name (when ts-node
                    (treesit-node-text ts-node)
		    )
		   )
         (marker (when ts-node
                   (set-marker (make-marker)
                               (treesit-node-start ts-node)))))
    (cond
     ((or (null ts-node) (null name)) subtrees)
     (subtrees
      `((,name ,(cons name marker) ,@subtrees)))
     (t
      `((,name . ,marker))))))


  ;; Comments.f
    (setq-local comment-start "% ")
    (setq-local comment-end "")
    (setq-local comment-start-skip (rx "%" (* (syntax whitespace))))

    ;; Indent.
    ;; Indent.
    (setq-local treesit-simple-indent-rules prolog-ts-mode--indent-rules)


    ;; Imenu.
    (setq-local imenu-create-index-function #'prolog-ts-mode--imenu)
    (setq-local which-func-functions nil)


    ;; Font-lock.
    (setq-local treesit-font-lock-settings prolog-ts-mode--font-lock-settings)
     (setq-local treesit-font-lock-feature-list
                '((comment function string error eot)
                  ;; 'function' and 'variable' here play 
                  ;; different roles than in other ts modes, so we
                  ;; kept them at level 3.
                  ( function-call misc-punctuation functor)


		  (number variable bracket operator))
		  )
	       
   
     (treesit-major-mode-setup)))

(if (treesit-ready-p 'prolog)
  
    (add-to-list 'auto-mode-alist
		 '("\\.yap\\'"  . prolog-ts-mode))
  (add-to-list 'auto-mode-alist
		 '("\\.ypp\\'"  . prolog-ts-mode))
    (add-to-list 'auto-mode-alist
          '("\\.pl\\'"  . prolog-ts-mode))
    (add-to-list 'auto-mode-alist
		 '("\\.prolog\\'"  . prolog-ts-mode))
    )
(provide 'prolog-ts-mode)


;;; prolog-ts-mode.el ends here
