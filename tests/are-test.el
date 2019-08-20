;;; are-test.el --- tests for are -*- lexical-binding: t; -*-

;; Copyright (c) 2019, Hans Jerry Illikainen <hji@dyntopia.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:
(require 'ert)

(defvar are-debug nil
  "Debug messages.")

(defvar are-src-dir
  (concat (file-name-directory
           (directory-file-name
            (file-name-directory load-file-name)))
          (file-name-as-directory "src"))
  "Source directory.")

(defvar are-library
  (concat are-src-dir
          (file-name-as-directory ".libs")
          "libare")
  "Shared library file.")

(add-to-list 'load-path are-src-dir)

(require 'are)
(are-load)

(ert-deftest are-test-string-match ()
  "Test for `are-string-match'."
  (let ((strings
         '("" "m00" "abcd efgh\nijkl 0123"))
        (engine-patterns
         '(((:engine emacs :regexp "" :start nil)
            (:engine pcre2 :regexp "" :start nil))
           ((:engine emacs :regexp "\\([0-9]\\)\\([0-9]\\)" :start 0)
            (:engine pcre2 :regexp "([0-9])([0-9])" :start 0))
           ((:engine emacs :regexp "[0-9]\\'" :start nil)
            (:engine pcre2 :regexp "[0-9]$" :start nil))
           ((:engine emacs :regexp "[0-9]+\\'" :start 0)
            (:engine pcre2 :regexp "[0-9]+$" :start 0))
           ((:engine emacs :regexp "^[a-z]+" :start nil)
            (:engine pcre2 :regexp "^[a-z]+" :start nil))
           ((:engine emacs :regexp "\\`ijkl" :start nil)
            (:engine pcre2 :regexp "^ijkl" :start nil))
           ((:engine pcre2 :regexp ".*" :start 1000 :error t))))
        idx match-data are-engine engine regexp start error)
    (dolist (str strings)
      (dolist (ep engine-patterns)
        (dolist (plist ep)
          (setq engine (plist-get plist :engine)
                regexp (plist-get plist :regexp)
                start (plist-get plist :start)
                error (plist-get plist :error))
          (cond ((eq engine 'emacs)
                 (setq idx (string-match regexp str start)
                       match-data (match-data)))
                (t
                 (setq are-engine engine)
                 (if error
                     (should-error (are-string-match regexp str start))
                   (should (equal (are-string-match regexp str start) idx))
                   (should (equal (match-data) match-data))))))))))

(provide 'are-test)

;;; are-test.el ends here
