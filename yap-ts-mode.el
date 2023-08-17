;;; yap~-ts-mode.el --- tree-sitter support for Yap  -*- lexical-binding: t; -*-


;; Copyright (C) 2022-2023 Free Software Foundation, Inc.

;; Author     : Randy Taylor <dev@rjt.dev>
;; Maintainer : Randy Taylor <dev@rjt.dev>
;; Created    : December 2022
;; Keywords   : yap languages tree-sitter

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

(defcustom yap-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `yap-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'prolog)



(defvar yap-ts-mode--syntax-table
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
  "Syntax table for `yap-ts-mode'.")


(defvar yap-ts-mode--operators
  '("=" "-" "*" "/" "+" "%" "~" "|" "&" "^" "<<" ">>" "->"
    "." "<" "<=" ">=" ">" "==" "!=" "!" "&&" "||" "-="
    "+=" "*=" "/=" "%=" "|=" "&=" "^=" ">>=" "<<=" "--" "++")
  "Yap operators for tree-sitter font-locking.")

(defvar yap-ts-mode--brackets
  '("{" "}" "," ";" "[" "]" "(" ")")
  "Yap operators for tree-sitter font-locking.")

(defvar yap-ts-mode--indent-rules
  `((yap
     ((node-is "predicate_definition") column-0 0)
     ((node-is "directive") column-0 0)
     ((parent-is "predicate_definition") parent-bol yap-ts-mode-indent-offset)
     ((node-is "semic")  parent 0)
     ((node-is "rghtarrow")   parent 0)
     ((node-is "close_b")   parent 0)
     ((parent-is "disj")  parent 2)
     ((parent-is "bracketed_term")  parent 2)
     ((node-is "goal")  parent-bol yap-ts-mode-indent-offset)
     ((parent-is "list")  parent-bol 2)
     ((parent-is "arg")  parent-bol 0)
     ((node-is "close_list")  first-sibling 1)
     ))
    "Tree-sitter indent rules for `yap-ts-mode'.")

(defvar yap-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'yap
   :feature 'bracket
   '((["(" "{"  "}" ")"]) @font-lock-bracket-face)

   :language 'yap
   :feature 'builtin
   '((builtin) @font-lock-comment-face)
   ;; `(((foreach_command
   ;;     ((argument) @font-lock-constant-face
   ;;      (:match ,(rx-to-string
   ;;                `(seq bol
   ;;                      (or ,@yap-ts-mode--foreach-options)
   ;;                      eol))
   ;;              @font-lock-constant-face))))
   ;;   ((if_command
   ;;     ((argument) @font-lock-constant-face
   ;;      (:match ,(rx-to-string
   ;;                `(seq bol
   ;;                      (or ,@yap-ts-mode--if-conditions)
   ;;                      eol))
   ;;              @font-lock-constant-face)))))

   :language 'yap
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'yap
   :feature 'operator
   `(operator: (_) @font-lock-bracket-face)

   ;; :language 'yap
   ;; :feature 'bracket
   ;; `([,@yap-ts-mode--brackets] @font-lock-bracket-face)


   ;; :language 'yap
   ;; :feature 'constant
   ;; `(((argument) @font-lock-constant-face
   ;;    (:match ,(rx-to-string
   ;;              `(seq bol
   ;;                    (or ,@prolog-ts-mode--constants)
   ;;                    eol))
   ;;            @font-lock-constant-face)))

   :language 'yap
   :feature 'functor
   '((functor) @font-lock-constant-face)
  
   :language 'yap
   :feature 'function-call
   '((call_atom) @font-lock-function-call-face)
  
   ;; :language 'yap
   ;; :feature 'function
   ;; '((head_atom) @font-lock-function-name-face)

   :language 'yap
   :feature 'eot
   `((eot) @font-lock-warning-face)

   :language 'yap
   :feature 'builtin
   `((builtin) @font-lock-builtin-face)

   :language  'yap
   :feature 'number
   '((number) @font-lock-number-face)

   :language 'yap
   :feature 'string
   '([(quoted_atom) (string) (codes)] @font-lock-string-face)

   ;; :language 'yap
   ;; :feature 'escape-sequence
   ;; :override t
   ;; '((escape_sequence) @font-lock-escape-face)

   :language 'yap
   :feature 'function
   ;; Don't override strings.
   :override t
   '((head_atom) @font-lock-keyword-face)

   :language 'yap
   :feature 'variable
   '((variable) @font-lock-variable-face)

   :language 'yap
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face)
   )
  "Tree-sitter font-lock settings for `prolog-ts-mode'."
  )
nil

;;;###autoload
(define-derived-mode yap-ts-mode prog-mode "Prolog"
  "Major mode for editing Prolog files, powered by tree-sitter."
  :group 'prolog
  :syntax-table yap-ts-mode--syntax-table

  (when (treesit-ready-p 'yap)
    (treesit-parser-create 'yap)


;; Imenu.
;;  (setq-local treesit-defun-type-regexp
    ;;  (regexp-opt '("head")))

(defun yap-ts-mode--imenu ()
  "Return Imenu alist for the current buffer."
  (let* ((node (treesit-buffer-root-node))
         (pred-tree (treesit-induce-sparse-tree
                     node "head_atom" nil 1000))
         (pred-index (yap-ts-mode--imenu-1 pred-tree)))
    (append
     (when pred-index `(("Predicates" . ,pred-index))))))

(defun yap-ts-mode--imenu-1 (node)
  "Helper for `yap-ts-mode--imenu'.
Find string representation for NODE and set marker, then recurse
the subtrees."
  (let* ((ts-node (car node))
         (children (cdr node))
         (subtrees (mapcan #'yap-ts-mode--imenu-1
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
    (setq-local treesit-simple-indent-rules yap-ts-mode--indent-rules)


    ;; Imenu.
    (setq-local imenu-create-index-function #'yap-ts-mode--imenu)
    (setq-local which-func-functions nil)
    
    ;; Font-lock.
    (setq-local treesit-font-lock-settings yap-ts-mode--font-lock-settings)
     (setq-local treesit-font-lock-feature-list
                '((comment function string  builtin error eot)
                  ;; 'function' and 'variable' here play 
                  ;; different roles than in other ts modes, so we
                  ;; kept them at level 3.
                  ( function-call misc-punctuation functor)
                  (number variable bracket operator)))

    (treesit-major-mode-setup)))

(if (treesit-ready-p 'yap)
  
    (add-to-list 'auto-mode-alist
          '("\\.yap\\'"  . yap-ts-mode))
    (add-to-list 'auto-mode-alist
          '("\\.ypp\\'"  . yap-ts-mode))
    (add-to-list 'auto-mode-alist
          '("\\.pl\\'"  . yap-ts-mode))
    (add-to-list 'auto-mode-alist
          '("\\.prolog\\'"  . yap-ts-mode))
    )
(provide 'yap-ts-mode)


;;; yap-ts-mode.el ends here
