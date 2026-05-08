;;; ox-skills-test.el --- Tests for ox-skills  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Giovanni Crisalfi
;; SPDX-License-Identifier: GPL-3.0-or-later

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

;; Tests for ox-skills — Org export to SKILL.md files.

;;; Code:

(require 'ox-skills)
(require 'ert)

(defvar ox-skills-test-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this test file.")

;;; Backend Registration

(ert-deftest ox-skills-test-backend-defined ()
  "Test that the `skills' backend is registered."
  (should (org-export-get-backend 'skills)))

;;; YAML Encoding

(ert-deftest ox-skills-test-yaml-encode-string ()
  "Test YAML encoding of string values."
  ;; Simple strings pass through unquoted
  (should (equal "hello" (ox-skills--yaml-quote-string "hello")))
  ;; Strings with special chars get quoted
  (should (equal "\"hello: world\"" (ox-skills--yaml-quote-string "hello: world")))
  ;; Empty and nil get empty quotes
  (should (equal "\"\"" (ox-skills--yaml-quote-string "")))
  (should (equal "\"\"" (ox-skills--yaml-quote-string nil))))

(ert-deftest ox-skills-test-yaml-encode-bool ()
  "Test YAML encoding of boolean values."
  ;; Elisp booleans
  (should (equal "true" (ox-skills--parse-bool t)))
  (should-not (ox-skills--parse-bool nil))
  ;; Strings
  (should (equal "true" (ox-skills--parse-bool "true")))
  (should (equal "true" (ox-skills--parse-bool "yes")))
  (should (equal "true" (ox-skills--parse-bool "t")))
  (should (equal "false" (ox-skills--parse-bool "false")))
  (should (equal "false" (ox-skills--parse-bool "no")))
  (should (equal "false" (ox-skills--parse-bool "nil"))))

(ert-deftest ox-skills-test-yaml-encode-list ()
  "Test YAML encoding of list values."
  (should (equal "[a, b, c]"
                 (ox-skills--yaml-encode-value "a b c" 'list)))
  (should (equal "[]"
                 (ox-skills--yaml-encode-value nil 'list))))

(ert-deftest ox-skills-test-yaml-frontmatter ()
  "Test full YAML frontmatter generation."
  (let ((info (list :skill-name "test"
                    :skill-description "A test skill"
                    :skill-arguments "arg1 arg2")))
    (let ((result (ox-skills--yaml-frontmatter info)))
      (should (string-match "name: test" result))
      (should (string-match "description: A test skill" result))
      (should (string-match "arguments: \\[arg1, arg2\\]" result))
      (should (string-prefix-p "---" result))
      (should (string-suffix-p "---\n\n" result)))))

;;; Source Block Transcoder

(ert-deftest ox-skills-test-src-block-inject ()
  "Test that :inject yes src blocks produce ```! output."
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_src sh :inject yes\necho hello\n#+end_src")
    (let* ((org-data (org-element-parse-buffer))
           (src-block (org-element-map org-data 'src-block #'identity nil t)))
      (let ((result (ox-skills--src-block src-block nil nil)))
        (should src-block)
        (should (string-match "```!" result))
        (should (string-match "echo hello" result))))))

(ert-deftest ox-skills-test-src-block-normal ()
  "Test that regular src blocks produce regular ``` blocks."
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_src elisp\n(message \"hi\")\n#+end_src")
    (let* ((org-data (org-element-parse-buffer))
           (src-block (org-element-map org-data 'src-block #'identity nil t)))
      (let ((result (ox-skills--src-block src-block nil nil)))
        (should-not (string-match "```!" result))
        (should (string-match "```elisp" result))
        (should (string-match "(message \"hi\")" result))))))

;;; Example Block Transcoder

(ert-deftest ox-skills-test-example-block ()
  "Test that example blocks produce fenced code blocks."
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_example\nsome text\n#+end_example")
    (let* ((tree (org-element-parse-buffer))
           (block (org-element-map tree 'example-block #'identity nil t)))
      (let ((result (ox-skills--example-block block nil nil)))
        (should (string-prefix-p "```" result))
        (should (string-match "some text" result))
        (should (string-suffix-p "```" (string-trim result)))))))

;;; Quote Block Transcoder

(ert-deftest ox-skills-test-quote-block ()
  "Test that quote blocks produce Markdown blockquotes."
  (let ((result (ox-skills--quote-block nil "line one\nline two\n" nil)))
    (should (string-match "^> line one" result))
    (should (string-match "^> line two" result))))

;;; Table Transcoder

(ert-deftest ox-skills-test-table ()
  "Test that org tables produce Markdown pipe tables."
  (with-temp-buffer
    (org-mode)
    (insert "| A | B |\n|---+---|\n| 1 | 2 |")
    (let ((result (org-export-as 'skills nil nil t)))
      (should (string-match "| A | B |" result))
      (should (string-match "|---|" result))
      (should (string-match "| 1 | 2 |" result)))))

;;; List Parsing

(ert-deftest ox-skills-test-parse-list ()
  "Test list parsing from space-separated string."
  (should (equal '("a" "b" "c") (ox-skills--parse-list "a b c")))
  (should (equal '("one") (ox-skills--parse-list "one")))
  (should-not (ox-skills--parse-list nil))
  (should-not (ox-skills--parse-list "")))

;;; Output Path

(ert-deftest ox-skills-test-output-path ()
  "Test output path computation."
  (let ((info (list :skill-base-dir "/tmp/test"
                    :skill-name "my-skill"
                    :input-file "/some/path/example.org")))
    (should (equal "/tmp/test/my-skill/SKILL.md"
                   (ox-skills--output-path info)))))

(ert-deftest ox-skills-test-output-path-no-name ()
  "Test output path with no explicit name falls back to filename stem."
  (let ((info (list :skill-base-dir "/tmp/test"
                    :input-file "/some/path/example.org")))
    (should (equal "/tmp/test/example/SKILL.md"
                   (ox-skills--output-path info)))))

(ert-deftest ox-skills-test-output-path-with-subdir ()
  "Test output path with subdirectory segments from ancestor sections."
  (let ((info (list :skill-base-dir "/tmp/test"
                    :skill-name "my-skill"
                    :skill-subdirs '("engineering" "tools")
                    :input-file "/some/path/example.org")))
    (should (equal "/tmp/test/engineering/tools/my-skill/SKILL.md"
                   (ox-skills--output-path info)))))

;;; Single-Skill Export (File-Based)

(ert-deftest ox-skills-test-export-to-file ()
  "Test export of single-skill.org to SKILL.md."
  (let* ((src (expand-file-name "data/single-skill.org" ox-skills-test-dir))
         (tmpdir (make-temp-file "ox-skills-test-" t)))
    (unwind-protect
        (with-current-buffer (find-file-noselect src)
          (unwind-protect
              (let* ((outfile (ox-skills-export-to-md
                               nil nil nil nil
                               (list :skill-base-dir tmpdir)))
                     (content (with-temp-buffer
                                (insert-file-contents outfile)
                                (buffer-string))))
                (should outfile)
                (should (file-exists-p outfile))
                ;; YAML frontmatter
                (should (string-prefix-p "---" content))
                (should (string-match "name: example-skill" content))
                (should (string-match "An example skill" content))
                ;; Body content
                (should (string-match "This is the skill body" content))
                ;; :inject yes block
                (should (string-match "```!" content))
                ;; Regular code block
                (should (string-match "```elisp" content)))
            (kill-buffer)))
      (delete-directory tmpdir t))))

;;; WIM Export (Multi-Skill)

(ert-deftest ox-skills-test-wim-export ()
  "Test WIM export of multi-skill.org."
  (let* ((src (expand-file-name "data/multi-skill.org" ox-skills-test-dir))
         (tmpdir (make-temp-file "ox-skills-test-" t)))
    (unwind-protect
        (with-current-buffer (find-file-noselect src)
          (unwind-protect
              (let ((ox-skills-default-base-dir tmpdir))
                (ox-skills-export-wim-to-md :all-subtrees)
                (should (file-exists-p (expand-file-name "skill-one/SKILL.md" tmpdir)))
                (should (file-exists-p (expand-file-name "skill-two/SKILL.md" tmpdir))))
            (kill-buffer)))
      (delete-directory tmpdir t))))

(provide 'ox-skills-test)

;;; ox-skills-test.el ends here
