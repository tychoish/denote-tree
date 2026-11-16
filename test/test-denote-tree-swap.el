;;; test-denote-tree-swap.el --- ERT tests for denote-tree-swap.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Real temp-directory notes: the swap renames files on disk.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'denote-tree-swap)

(defun denote-tree-test--make-org-note (dir id sig title)
  "Create a minimal org Denote note in DIR and return its path."
  (let ((file (expand-file-name
               (concat id "==" sig "--" (downcase (replace-regexp-in-string " " "-" title)) ".org")
               dir)))
    (with-temp-file file
      (insert "#+title:      " title "\n#+date:       [2024-01-01 Mon]\n"
              "#+identifier: " id "\n#+signature: " sig "\n#+filetags:   :test:\n\nBody.\n"))
    file))

(defun denote-tree-test--kill-dir-buffers (dir)
  "Kill every buffer visiting a file under DIR without prompting."
  (let ((kill-buffer-query-functions nil))
    (seq-do (lambda (buf)
              (when-let* ((f (buffer-file-name buf))
                          ((string-prefix-p (expand-file-name dir) (expand-file-name f))))
                (with-current-buffer buf (set-buffer-modified-p nil))
                (kill-buffer buf)))
            (buffer-list))))

(defmacro denote-tree-swap-test--with-notes (specs &rest body)
  "Create notes from SPECS (ID SIG TITLE) in a temp dir and run BODY.
Binds `dir' and `hitl-plan-directory'."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "denote-tree-swap-test-" t))
          (hitl-plan-directory (expand-file-name "plans" dir))
          (denote-directory (list dir))
          (denote-sequence-scheme (quote alphanumeric))
          (denote-rename-confirmations nil))
     (unwind-protect
         (progn
           (seq-do (lambda (s) (apply #'denote-tree-test--make-org-note dir s)) ',specs)
           ,@body)
       (ignore-errors (denote-tree-test--kill-dir-buffers dir))
       (ignore-errors (delete-directory dir t)))))

(defun denote-tree-swap-test--sig (dir id)
  "Return the sequence of the note with identifier ID in DIR."
  (when-let* ((f (car (directory-files dir t (concat "\\`" id "==")))))
    (denote-retrieve-filename-signature f)))

(defun denote-tree-swap-test--file (dir id)
  (car (directory-files dir t (concat "\\`" id "=="))))

(ert-deftest denote-tree-swap-test/swaps-trees-of-different-depth ()
  "A non-sibling swap moves each subtree, fixing descendant alternation."
  (denote-tree-swap-test--with-notes
      (("20240101T100000" "1" "A") ("20240101T100100" "1a" "A kid")
       ("20240101T100200" "2" "B") ("20240101T100300" "2a" "B kid")
       ("20240101T100400" "2a1" "B grandkid"))
    (let ((plan (denote-tree-swap
                 (denote-tree-swap-test--file dir "20240101T100000")
                 (denote-tree-swap-test--file dir "20240101T100400"))))
      (should (eq (hitl-plan-status plan) 'complete))
      (should (equal "2a1" (denote-tree-swap-test--sig dir "20240101T100000")))
      (should (equal "2a1a" (denote-tree-swap-test--sig dir "20240101T100100")))
      (should (equal "1" (denote-tree-swap-test--sig dir "20240101T100400")))
      (should (equal "2" (denote-tree-swap-test--sig dir "20240101T100200"))))))

(ert-deftest denote-tree-swap-test/resumable-by-hand-crank ()
  "Stepping one move at a time pauses, and resuming finishes the swap."
  (denote-tree-swap-test--with-notes
      (("20240101T100000" "1" "A") ("20240101T100100" "1a" "A kid")
       ("20240101T100200" "2" "B"))
    (let* ((plan (denote-tree-swap
                  (denote-tree-swap-test--file dir "20240101T100000")
                  (denote-tree-swap-test--file dir "20240101T100200")
                  t)))
      (should (eq (hitl-plan-status plan) 'paused))
      (should (equal "3" (denote-tree-swap-test--sig dir "20240101T100000")))
      (hitl-plan-execute plan)
      (should (eq (hitl-plan-status plan) 'complete))
      (should (equal "2" (denote-tree-swap-test--sig dir "20240101T100000")))
      (should (equal "2a" (denote-tree-swap-test--sig dir "20240101T100100")))
      (should (equal "1" (denote-tree-swap-test--sig dir "20240101T100200"))))))

(ert-deftest denote-tree-swap-test/move-is-idempotent-after-partial-rename ()
  "A move interrupted after renaming descendants finishes on rerun."
  (denote-tree-swap-test--with-notes
      (("20240101T100000" "1" "A") ("20240101T100100" "1a" "A kid"))
    (let ((target (list :id "20240101T100000" :from "1" :seq "5")))
      ;; Simulate the interruption: only the descendant was renamed.
      (denote-rename-file (denote-tree-swap-test--file dir "20240101T100100")
                          'keep-current 'keep-current "5a" 'keep-current 'keep-current)
      (denote-tree-swap--move (denote-tree-swap--resolve target))
      (should (equal "5" (denote-tree-swap-test--sig dir "20240101T100000")))
      (should (equal "5a" (denote-tree-swap-test--sig dir "20240101T100100")))
      ;; Rerun is a no-op.
      (denote-tree-swap--move (denote-tree-swap--resolve target))
      (should (equal "5" (denote-tree-swap-test--sig dir "20240101T100000"))))))

(ert-deftest denote-tree-swap-test/refuses-ancestor-and-self ()
  (denote-tree-swap-test--with-notes
      (("20240101T100000" "1" "A") ("20240101T100100" "1a" "A kid"))
    (let ((a (denote-tree-swap-test--file dir "20240101T100000"))
          (kid (denote-tree-swap-test--file dir "20240101T100100")))
      (should-error (denote-tree-swap-create-plan a kid) :type 'user-error)
      (should-error (denote-tree-swap-create-plan a a) :type 'user-error))))

(provide 'test-denote-tree-swap)
;;; test-denote-tree-swap.el ends here
