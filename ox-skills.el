;;; ox-skills.el --- Org export to SKILL.md files -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Giovanni Crisalfi
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>
;; Maintainer: Giovanni Crisalfi <giovanni.crisalfi@protonmail.com>
;; Assisted-by: Claude:claude-opus-4-5
;; Assisted-by: DeepSeek:deepseek-v4-flash
;; Created: 2026-05-06
;; Modified: 2026-05-08
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.2"))
;; Keywords: wp, org, ai, agent, skills
;; Homepage: https://github.com/gicrisf/ox-skills

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ox-skills provides an Org export backend for generating
;; SKILL.md files with YAML frontmatter.

;; Features:
;; - Derives from ox-md (Org core), no external dependencies
;; - Exports Org files to {base-dir}/{name}/SKILL.md
;; - YAML frontmatter from SKILL_* keywords
;; - Dynamic context injection via `:inject yes' source blocks
;; - WIM (What I Mean) export for multi-skill Org files

;; Usage:
;;   M-x ox-skills-export-to-md      Export file/subtree to SKILL.md
;;   M-x ox-skills-export-as-md      Export to buffer
;;   M-x ox-skills-export-wim-to-md  Batch export all skill subtrees

;;; Code:

(require 'ox-md)

;;; Variables

(defgroup ox-skills nil
  "Org export backend for SKILL.md files."
  :group 'org-export
  :prefix "ox-skills-")

(defcustom ox-skills-default-base-dir nil
  "Default base directory for skill output.
If nil, uses the directory containing the Org file."
  :type '(choice (const nil) directory)
  :group 'ox-skills)

;;; YAML Frontmatter

(defconst ox-skills--field-types
  '((name . string)
    (description . string)
    (when_to_use . string)
    (argument-hint . string)
    (arguments . list)
    (disable-model-invocation . bool)
    (user-invocable . bool)
    (allowed-tools . list)
    (model . string)
    (effort . string)
    (context . string)
    (agent . string)
    (paths . list)
    (shell . string))
  "Alist mapping YAML field names to their types.")

(defconst ox-skills--keyword-to-field
  '((:skill-name . name)
    (:skill-description . description)
    (:skill-when-to-use . when_to_use)
    (:skill-argument-hint . argument-hint)
    (:skill-arguments . arguments)
    (:skill-disable-model-invocation . disable-model-invocation)
    (:skill-user-invocable . user-invocable)
    (:skill-allowed-tools . allowed-tools)
    (:skill-model . model)
    (:skill-effort . effort)
    (:skill-context . context)
    (:skill-agent . agent)
    (:skill-paths . paths)
    (:skill-shell . shell))
  "Alist mapping Org export keywords to YAML field names.")

(defun ox-skills--parse-list (str)
  "Parse STR as space-separated list, respecting quoted strings."
  (when (and str (not (string-empty-p str)))
    (split-string-and-unquote str)))

(defun ox-skills--parse-bool (value)
  "Parse VALUE as a YAML boolean string.
Accepts the Elisp booleans t and nil, and the strings
\"true\", \"yes\", \"t\" (true) and \"false\", \"no\", \"nil\" (false)."
  (cond
   ((null value) nil)
   ((eq value t) "true")
   ((stringp value)
    (let ((s (downcase (string-trim value))))
      (cond
       ((member s '("true" "yes" "t")) "true")
       ((member s '("false" "no" "nil" "")) "false")
       (t (error "Invalid boolean value: %s" value)))))
   (t "true")))

(defun ox-skills--yaml-quote-string (str)
  "Quote STR as an inline YAML string value."
  (if (or (null str) (string-empty-p str))
      "\"\""
    (let ((needs-quote (or (string-match-p "[][\n\":{}\\,&*#?|<>=!%@`]" str)
                           (string-match-p "^[ \t]\\|[ \t]$" str)
                           (member (downcase str) '("true" "false" "yes" "no" "null")))))
      (if needs-quote
          (concat "\""
                  (replace-regexp-in-string
                   "\\\\" "\\\\\\\\"
                   (replace-regexp-in-string
                    "\"" "\\\\\""
                    (replace-regexp-in-string
                     "\n" "\\\\n" str)))
                  "\"")
        str))))

(defun ox-skills--yaml-fold-string (str indent)
  "Return STR as a YAML folded block scalar indented by INDENT spaces."
  (let* ((fill-width (- 78 indent))
         (pad (make-string indent ?\s))
         (words (split-string (string-trim str) nil t))
         lines current)
    (dolist (word words)
      (cond
       ((null current) (setq current word))
       ((<= (+ (length current) 1 (length word)) fill-width)
        (setq current (concat current " " word)))
       (t (push current lines) (setq current word))))
    (when current (push current lines))
    (concat ">\n"
            (mapconcat (lambda (l) (concat pad l)) (nreverse lines) "\n"))))

(defun ox-skills--yaml-encode-string (str)
  "Encode STR as a YAML string field value.
Long strings or strings containing quotes use a folded block scalar."
  (cond
   ((or (null str) (string-empty-p str)) "\"\"")
   ((or (> (length str) 76) (string-match-p "\"" str))
    (ox-skills--yaml-fold-string str 2))
   (t (ox-skills--yaml-quote-string str))))

(defun ox-skills--yaml-encode-value (value field-type)
  "Encode VALUE according to FIELD-TYPE for YAML output.
FIELD-TYPE is one of: string, bool, list."
  (pcase field-type
    ('string (ox-skills--yaml-encode-string value))
    ('bool (ox-skills--parse-bool value))
    ('list
     (let ((items (if (listp value) value (ox-skills--parse-list value))))
       (if items
           (concat "[" (mapconcat #'ox-skills--yaml-quote-string items ", ") "]")
         "[]")))))

(defun ox-skills--yaml-frontmatter (info)
  "Generate YAML frontmatter from export INFO plist."
  (let ((fields nil))
    ;; Collect non-nil fields
    (dolist (mapping ox-skills--keyword-to-field)
      (let* ((keyword (car mapping))
             (field (cdr mapping))
             (value (plist-get info keyword))
             (field-type (alist-get field ox-skills--field-types)))
        (when (and value (not (string-empty-p (if (stringp value) value ""))))
          (let ((encoded (ox-skills--yaml-encode-value value field-type)))
            (when encoded
              (push (cons field encoded) fields))))))
    ;; Build YAML string
    (if fields
        (concat "---\n"
                (mapconcat
                 (lambda (f)
                   (format "%s: %s" (car f) (cdr f)))
                 (nreverse fields)
                 "\n")
                "\n---\n\n")
      "")))

;;; Output Path

(defun ox-skills--output-path (info)
  "Compute output path from export INFO plist.
Returns {base-dir}/{subdirs}/{name}/SKILL.md"
  (let* ((base-dir (or (plist-get info :skill-base-dir)
                       ox-skills-default-base-dir
                       (file-name-directory (plist-get info :input-file))
                       default-directory))
         (name (or (plist-get info :skill-name)
                   (file-name-base (or (plist-get info :input-file) "skill"))))
         (subdirs (plist-get info :skill-subdirs))
         (subdir-prefix (when subdirs
                          (concat (mapconcat #'identity subdirs "/") "/"))))
    (expand-file-name (concat subdir-prefix name "/SKILL.md") base-dir)))

;;; Source Block Transcoder

(defun ox-skills--src-block (src-block _contents _info)
  "Transcode SRC-BLOCK element to Markdown.
SRC-BLOCK is the Org element to transcode.
If the block has `:inject yes' header argument, output ```! block.
Otherwise, delegate to parent ox-md transcoder."
  (let* ((lang (org-element-property :language src-block))
         (code (org-element-property :value src-block))
         (params (org-element-property :parameters src-block))
         (inject (when params
                   (let* ((args (org-babel-parse-header-arguments params))
                          (val (cdr (assq :inject args))))
                     (and val (member (format "%s" val) '("yes" "t" "true")))))))
    (if inject
        ;; Output ```! block for dynamic injection
        (concat "```!\n" (org-trim code) "\n```")
      ;; Default: use standard markdown code block
      (concat "```" (or lang "") "\n" (org-trim code) "\n```"))))

;;; Additional Transcoders

(defun ox-skills--example-block (example-block _contents _info)
  "Transcode EXAMPLE-BLOCK to a fenced code block."
  (concat "```\n"
          (org-trim (org-element-property :value example-block))
          "\n```"))

(defun ox-skills--quote-block (_quote-block contents _info)
  "Transcode QUOTE-BLOCK to a Markdown blockquote.
CONTENTS is the already-transcoded block body."
  (let ((lines (split-string (string-trim-right contents) "\n")))
    (concat
     (mapconcat (lambda (l)
                  (if (string-empty-p (string-trim l)) ">" (concat "> " l)))
                lines "\n")
     "\n")))

(defun ox-skills--table (table _contents info)
  "Transcode TABLE to a Markdown pipe table.
INFO is the export communication channel."
  (let (header-rows body-rows in-body)
    (org-element-map table 'table-row
      (lambda (row)
        (if (eq (org-element-property :type row) 'rule)
            (setq in-body t)
          (let ((cells (org-element-map row 'table-cell
                         (lambda (cell)
                           (org-trim
                            (org-export-data (org-element-contents cell) info)))
                         info)))
            (if in-body (push cells body-rows) (push cells header-rows)))))
      info)
    (setq header-rows (nreverse header-rows)
          body-rows (nreverse body-rows))
    (unless in-body
      (setq body-rows (cdr header-rows)
            header-rows (list (car header-rows))))
    (let* ((n-cols (length (car header-rows)))
           (row-str (lambda (cells)
                      (concat "| " (mapconcat #'identity cells " | ") " |")))
           (sep (concat "|" (mapconcat (lambda (_) "---")
                                       (make-list n-cols nil) "|") "|")))
      (concat
       (mapconcat row-str header-rows "\n") "\n"
       sep "\n"
       (when body-rows (concat (mapconcat row-str body-rows "\n") "\n"))))))

;;; Final Output Filter

(defun ox-skills--final-output-filter (output _backend _info)
  "Normalize whitespace in OUTPUT after full export."
  ;; Collapse 3+ consecutive blank lines to one blank line
  (let ((result (replace-regexp-in-string "\n\n\n+" "\n\n" output)))
    ;; Fix list bullets: ox-md emits "-   " and "N.  " — trim to "- " and "N. "
    (setq result (replace-regexp-in-string "\n\\( *\\)-   " "\n\\1- " result))
    (setq result (replace-regexp-in-string "\n\\( *[0-9]+\\.\\)  " "\n\\1 " result))
    result))

;;; Template

(defun ox-skills--template (contents info)
  "Return complete document string after Markdown conversion.
CONTENTS is the transcoded contents string.
INFO is the export info plist."
  (concat (ox-skills--yaml-frontmatter info) contents))

;;; Backend Definition

(org-export-define-derived-backend 'skills 'md
  :menu-entry
  '(?s "Export to SKILL.md"
       ((?s "File to SKILL.md" ox-skills-export-to-md)
        (?S "File to a temporary buffer" ox-skills-export-as-md)
        (?w "Subtree or File to SKILL.md"
            (lambda (a _s v _b)
              (ox-skills-export-wim-to-md nil a v)))
        (?a "All subtrees (or File) to SKILL.md"
            (lambda (a _s v _b)
              (ox-skills-export-wim-to-md :all-subtrees a v)))))
  :filters-alist '((:filter-final-output . ox-skills--final-output-filter))
  :translate-alist
  '((template . ox-skills--template)
    (src-block . ox-skills--src-block)
    (example-block . ox-skills--example-block)
    (quote-block . ox-skills--quote-block)
    (table . ox-skills--table))
  :options-alist
  '((:with-toc nil "toc" nil)
    (:with-smart-quotes nil "'" nil)
    (:with-special-strings nil "-" nil)
    (:skill-name "SKILL_NAME" nil nil nil)
    (:skill-description "SKILL_DESCRIPTION" nil nil nil)
    (:skill-when-to-use "SKILL_WHEN_TO_USE" nil nil nil)
    (:skill-argument-hint "SKILL_ARGUMENT_HINT" nil nil nil)
    (:skill-arguments "SKILL_ARGUMENTS" nil nil nil)
    (:skill-disable-model-invocation "SKILL_DISABLE_MODEL_INVOCATION" nil nil nil)
    (:skill-user-invocable "SKILL_USER_INVOCABLE" nil nil nil)
    (:skill-allowed-tools "SKILL_ALLOWED_TOOLS" nil nil nil)
    (:skill-model "SKILL_MODEL" nil nil nil)
    (:skill-effort "SKILL_EFFORT" nil nil nil)
    (:skill-context "SKILL_CONTEXT" nil nil nil)
    (:skill-agent "SKILL_AGENT" nil nil nil)
    (:skill-paths "SKILL_PATHS" nil nil nil)
    (:skill-shell "SKILL_SHELL" nil nil nil)
    (:skill-base-dir "SKILL_BASE_DIR" nil nil nil)))

;;; Export Commands

;;;###autoload
(defun ox-skills-export-as-md (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a SKILL.md buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write the body
without YAML frontmatter.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org Skills Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'skills "*Org Skills Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (when (fboundp 'markdown-mode) (markdown-mode)))))

;;;###autoload
(defun ox-skills-export-to-md (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a SKILL.md file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write the body
without YAML frontmatter.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (let* ((info (org-combine-plists
                (org-export-get-environment 'skills subtreep)
                ext-plist
                (list :input-file (buffer-file-name))))
         (outfile (ox-skills--output-path info))
         (outdir (file-name-directory outfile)))
    (unless (file-directory-p outdir)
      (make-directory outdir t))
    (org-export-to-file 'skills outfile
      async subtreep visible-only body-only ext-plist
      (lambda (file) file))))

;;; WIM Export
;; The WIM (What I Mean) dispatch pattern is taken from ox-hugo
;; (org-hugo-export-wim-to-md).  The idea: check the subtree at point,
;; walk up to the first ancestor with EXPORT_SKILL_NAME, and fall back
;; to file export when no valid subtree is found.

(defun ox-skills--subtree-plist ()
  "Build export plist from current subtree's EXPORT_SKILL_* properties."
  (let ((plist nil))
    (dolist (mapping ox-skills--keyword-to-field)
      (let* ((keyword (car mapping))
             (prop-name (upcase
                         (concat "EXPORT_"
                                 (substring (symbol-name keyword) 1))))
             (prop-name (replace-regexp-in-string "-" "_" prop-name))
             (value (org-entry-get nil prop-name)))
        (when value
          (setq plist (plist-put plist keyword value)))))
    (let ((base-dir (org-entry-get nil "EXPORT_SKILL_BASE_DIR")))
      (when base-dir
        (setq plist (plist-put plist :skill-base-dir base-dir))))
    plist))

(defun ox-skills--collect-subdirs ()
  "Collect EXPORT_SKILL_SUBDIR values from ancestors of current heading.
Walk up the heading tree and return subdir strings, outermost first."
  (let (subdirs)
    (save-excursion
      (while (org-up-heading-safe)
        (let ((subdir (org-entry-get nil "EXPORT_SKILL_SUBDIR")))
          (when subdir
            (push subdir subdirs)))))
    subdirs))

(defun ox-skills--buffer-has-valid-subtree-p ()
  "Return non-nil if buffer has at least one subtree with EXPORT_SKILL_NAME."
  (org-with-wide-buffer
   (catch 'found
     (org-map-entries
      (lambda () (throw 'found t))
      "EXPORT_SKILL_NAME<>\"\""))))

(defun ox-skills--buffer-has-skill-p ()
  "Return non-nil if the buffer has any skill metadata.
Checks for a file-level SKILL_NAME keyword or any subtree with
EXPORT_SKILL_NAME."
  (or (org-collect-keywords '("SKILL_NAME"))
      (ox-skills--buffer-has-valid-subtree-p)))

(defun ox-skills--get-valid-subtree ()
  "Return point of nearest subtree with EXPORT_SKILL_NAME at or above point.
Walk up from the current heading.  Return the point or nil."
  (org-with-wide-buffer
   (condition-case nil
       (progn
         (org-back-to-heading :invisible-ok)
         (catch :found
           (while t
             (when (org-entry-get nil "EXPORT_SKILL_NAME")
               (throw :found (point)))
             (unless (org-up-heading-safe)
               (throw :found nil)))))
     (error nil))))

(defun ox-skills--do-export-subtree (_async visible-only)
  "Export the skill subtree at point to its SKILL.md file.
VISIBLE-ONLY means only export visible parts of the subtree.
Return the output file path."
  (let* ((ext-plist (ox-skills--subtree-plist))
         (subdirs (ox-skills--collect-subdirs))
         (ext-plist (if subdirs
                        (plist-put ext-plist :skill-subdirs subdirs)
                      ext-plist))
         ;; subtreep=t here so org reads EXPORT_SKILL_* from the heading for the output path
         (info (org-combine-plists
                (org-export-get-environment 'skills t)
                ext-plist
                (list :input-file (buffer-file-name))))
         (outfile (ox-skills--output-path info))
         (outdir (file-name-directory outfile)))
    (unless (file-directory-p outdir)
      (make-directory outdir t))
    (save-restriction
      (org-narrow-to-subtree)
      ;; subtreep=nil so the root heading exports as h1 and children as h2
      (let ((contents (org-export-as 'skills nil visible-only nil ext-plist)))
        (with-temp-file outfile
          (insert contents))))
    outfile))

;;;###autoload
(defun ox-skills-export-wim-to-md (&optional all-subtrees async visible-only)
  "Export the current skill subtree, an ancestor, or the whole file.

This is the \"What I Mean\" export:

- If point is inside a subtree with EXPORT_SKILL_NAME, export
  that subtree.
- If not, walk up headings until one with EXPORT_SKILL_NAME is
  found and export it.
- If no valid subtree exists above point, fall back to file export.

ALL-SUBTREES non-nil means export every subtree in the buffer
that has an EXPORT_SKILL_NAME property.
ASYNC means the export process should happen asynchronously.
VISIBLE-ONLY means only export visible parts of the subtrees."
  (interactive "P")
  (unless (ox-skills--buffer-has-skill-p)
    (user-error "No skill metadata found — add SKILL_NAME or EXPORT_SKILL_NAME"))
  (let ((buf-has-subtree (ox-skills--buffer-has-valid-subtree-p)))
    (cond
     ((and buf-has-subtree all-subtrees)
      (let ((count 0) (files nil))
        (save-excursion
          (org-map-entries
           (lambda ()
             (when (org-entry-get nil "EXPORT_SKILL_NAME")
               (let ((outfile (ox-skills--do-export-subtree async visible-only)))
                 (push outfile files)
                 (setq count (1+ count)))))
           "EXPORT_SKILL_NAME<>\"\"" 'file))
        (message "Exported %d skill%s: %s"
                 count (if (= count 1) "" "s")
                 (mapconcat #'identity (nreverse files) ", "))))
     (buf-has-subtree
      (let ((pos (ox-skills--get-valid-subtree)))
        (if pos
            (save-excursion
              (goto-char pos)
              (let ((outfile (ox-skills--do-export-subtree async visible-only)))
                (message "Exported skill to %s" outfile)
                outfile))
          (ox-skills-export-to-md async nil visible-only))))
     (t
      (ox-skills-export-to-md async nil visible-only)))))

;;; Dispatcher Integration

(defun ox-skills--dispatch-filter (orig-fn &rest args)
  "Hide the skills backend from the export dispatcher when no metadata is present.
ORIG-FN is the original `org-export-dispatch'; ARGS are its arguments."
  (if (ox-skills--buffer-has-skill-p)
      (apply orig-fn args)
    (let ((org-export-registered-backends
           (cl-remove-if (lambda (b)
                           (eq (org-export-backend-name b) 'skills))
                         org-export-registered-backends)))
      (apply orig-fn args))))

(advice-add 'org-export-dispatch :around #'ox-skills--dispatch-filter)

(provide 'ox-skills)

;;; ox-skills.el ends here
