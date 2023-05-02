;;; prolog~-ts-mode.el --- tree-sitter support for Prolog  -*- lexical-binding: t; -*-


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

(defcustom prolog-ts-mode-indent-offset 2
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



(defun c-ts-mode--anchor-prev-sibling (node parent bol &rest _)
  "Return the start of the previous named sibling of NODE.

This anchor handles the special case where the previous sibling
is a labeled_statement, in that case, return the child of the
labeled statement instead.  (Actually, recursively go down until
the node isn't a labeled_statement.)  Eg,

label:
  int x = 1;
  int y = 2;

The anchor of \"int y = 2;\" should be \"int x = 1;\" rather than
the labeled_statement.

Return nil if a) there is no prev-sibling, or 2) prev-sibling
doesn't have a child.

PARENT and BOL are like other anchor functions."
  (when-let ((prev-sibling
              (or (treesit-node-prev-sibling node t)
                  (treesit-node-prev-sibling
                   (treesit-node-first-child-for-pos parent bol) t)
                  (treesit-node-child parent -1 t)))
             (continue t))
    (save-excursion
      (while (and prev-sibling continue)
        (pcase (treesit-node-type prev-sibling)
          ;; Get the statement in the label.
          ("labeled_statement"
           (setq prev-sibling (treesit-node-child prev-sibling 2)))
          ;; Get the last statement in the preproc.  Tested by
          ;; "Prev-Sibling When Prev-Sibling is Preproc" test.
          ((or "preproc_if" "preproc_ifdef")
           (setq prev-sibling (treesit-node-child prev-sibling -2)))
          ((or "preproc_elif" "preproc_else")
           (setq prev-sibling (treesit-node-child prev-sibling -1)))
          ((or "#elif" "#else")
           (setq prev-sibling (treesit-node-prev-sibling
                               (treesit-node-parent prev-sibling) t)))
          ;; If the start of the previous sibling isn't at the
          ;; beginning of a line, something's probably not quite
          ;; right, go a step further.
          (_ (goto-char (treesit-node-start prev-sibling))
             (if (looking-back (rx bol (* whitespace))
                               (line-beginning-position))
                 (setq continue nil)
               (setq prev-sibling
                     (treesit-node-prev-sibling prev-sibling)))))))
    ;; This could be nil if a) there is no prev-sibling or b)
    ;; prev-sibling doesn't have a child.
    (treesit-nobbxde-start prev-sibling)))

(defvar prolog-ts-mode--indent-rules
  `((prolog
     ((node-is "inner") parent-bol 2)
     ((node-is "goal") parent-bol 4)
     ))
    "Tree-sitter indent rules for `prolog-ts-mode'.")

(defvar prolog-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'prolog
   :feature 'bracket
   '((["(" "{" "[" "]" "}" ")"]) @font-lock-bracket-face)

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
   ;; :feature 'constant
   ;; `(((argument) @font-lock-constant-face
   ;;    (:match ,(rx-to-string
   ;;              `(seq bol
   ;;                    (or ,@prolog-ts-mode--constants)
   ;;                    eol))
   ;;            @font-lock-constant-face)))

   :language 'prolog
   :feature 'function-call
   '((call_atom) @font-lock-function-call-face)
  
   :language 'prolog
   :feature 'function
   '((head_atom) @font-lock-function-name-face)

   :language 'prolog
   :feature 'keyword
   `((eot) @font-lock-keyword-face)

   :language 'prolog
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
   :feature 'misc-punctuation
   ;; Don't override strings.
   :override 'nil
   '(([":-" "-->" "->" "<" ">"]) @font-lock-misc-punctuation-face)

   :language 'prolog
   :feature 'variable
   '((variable) @font-lock-variable-use-face)

   :language 'prolog
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face)
   )
  "Tree-sitter font-lock settings for `prolog-ts-mode'."
  )
nil

;;;###autoload
(define-derived-mode prolog-ts-mode prog-mode "Prolog"
  "Major mode for editing Prolog files, powered by tree-sitter."
  :group 'prolog
  :syntax-table prolog-ts-mode--syntax-table

  (when (treesit-ready-p 'prolog)
    (treesit-parser-create 'prolog)


;; Imenu.
  (setq-local treesit-defun-name-function #'treesit-node-text)
  (setq-local treesit-simple-imenu-settings
              '(("Predicates" "\\'head\\'" nil nil)))


  ;; Comments.f
    (setq-local comment-start "% ")
    (setq-local comment-end "")
    (setq-local comment-start-skip (rx "%" (* (syntax whitespace))))

    ;; Indent.
    (setq-local treesit-simple-indent-rules prolog-ts-mode--indent-rules)

    ;; Font-lock.
    (setq-local treesit-font-lock-settings prolog-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment function string keyword)
                  ;; 'function' and 'variable' here play slightly
                  ;; different roles than in other ts modes, so we
                  ;; kept them at level 3.
                  ( function-call misc-punctuation)
                  (number variable bracket error)))

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

;;; yap-ts-mode.el ends here
