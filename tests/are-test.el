;;; are-test.el --- tests for are -*- lexical-binding: t; -*-

;; Copyright (c) 2019, Hans Jerry Illikainen <hji@dyntopia.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:
(require 'ert)
(require 'map)

(defvar are-debug t
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

(defmacro are-test-regexp (str groups &rest alists)
  "Compare the result of the sexps in ALIST."
  (declare (indent defun))
  `(let ((str ,str)
         emacs-result result)
     (when are-debug
       (message "\n"))
     (dolist (elt ',alists)
       (when are-debug
         (message "Running: %S" elt))
       (save-excursion
         (save-match-data
           (condition-case err
               (let ((are-engine (car elt)))
                 (push (eval (cdr elt) `((str . ,str))) result)
                 (push (point) result)
                 (push (match-data) result)
                 (dotimes (i (1+ ,groups))
                   (push (match-string i str) result)
                   (push (match-string-no-properties i str) result)
                   (push (match-beginning i) result)
                   (push (match-end i) result)))
             (error
              (push 'error result)
              (when are-debug
                (message "Error: %S: %S" elt err))))))
       (cond ((eq (car elt) 'emacs)
              (setq emacs-result result)
              (setq result nil))
             (t
              (when are-debug
                (message "Emacs result: %S" emacs-result)
                (message "Other result: %S" result))
              (should (equal result emacs-result)))))))

(defun are-test-ucs-code (name)
  "Retrieve the unicode char code for NAME."
  (map-elt (ucs-names) name))

(defun are-test-ucs-str (&rest names)
  "Retrieve a unicode string that represents the codes in NAMES."
  (mapconcat (lambda (name)
               (char-to-string (are-test-ucs-code name)))
             names ""))

(defun are-test-insert (&rest names)
  "Insert the char codes for NAMES."
  (mapc #'insert (mapcar #'are-test-ucs-code names)))

(ert-deftest are-test-pcre2-re-search-forward ()
  "Test `are-re-search-forward'."
  (let ((are-compile-options '(pcre2-multiline pcre2-utf))
        (case-fold-search nil))
    (with-temp-buffer
      (insert (concat
               "abcd efgh ijkl 01234\n"
               "ABCD EFGH 987\n"
               "FOO 1\n"))

      ;; unicode
      (are-test-insert "PIE" "PINEAPPLE" "CINEMA" "BICYCLE" "SHIP" "METRO" "CAT"
                       "SMALL AIRPLANE" "BED" "HAPPY PERSON RAISING ONE HAND"
                       "CAT" "CAT FACE" "CAT FACE WITH TEARS OF JOY" "CAT"
                       "COPYLEFT SYMBOL" "CYCLONE" "FOGGY" "CLOSED UMBRELLA"
                       "DIGIT ZERO FULL STOP" "DIGIT ZERO COMMA"
                       "DIGIT ONE COMMA" "DIGIT TWO COMMA" "DIGIT THREE COMMA"
                       "DIGIT FOUR COMMA" "DIGIT FIVE COMMA" "DIGIT SIX COMMA"
                       "DIGIT SEVEN COMMA" "DIGIT EIGHT COMMA"
                       "DIGIT NINE COMMA")

      ;; search forward
      (goto-char (point-min))

      (are-test-regexp nil 1
        (emacs . (re-search-forward "\\(.*\\)"))
        (pcre2 . (are-re-search-forward "(.*)")))

      (are-test-regexp nil 1
        (emacs . (re-search-forward "\\(.*\\)" -10))
        (pcre2 . (are-re-search-forward "(.*)" -10)))

      (are-test-regexp nil 1
        (emacs . (re-search-forward "\\(.*\\)" 10))
        (pcre2 . (are-re-search-forward "(.*)" 10)))

      (are-test-regexp nil 0
        (emacs . (re-search-forward "[a-z]"))
        (pcre2 . (are-re-search-forward "[a-z]")))

      (are-test-regexp nil 0
        (emacs . (re-search-forward "[a-z]"))
        (pcre2 . (are-re-search-forward "[a-z]")))

      (are-test-regexp nil 0
        (emacs . (re-search-forward "e.*?[0-9]"))
        (pcre2 . (are-re-search-forward "e.*?[0-9]")))

      (are-test-regexp nil 1
        (emacs . (re-search-forward "e\\(.*\\)?[0-9]"))
        (pcre2 . (are-re-search-forward "e(.*)?[0-9]")))

      (are-test-regexp nil 2
        (emacs . (re-search-forward "e\\(.*\\)?\\([0-9]\\)"))
        (pcre2 . (are-re-search-forward "e(.*)?([0-9])")))

      (are-test-regexp nil 2
        (emacs . (re-search-forward
                  "\\([A-Z]\\{4\\}\\)[[:space:]+]\\([0-9]\\{3\\}\\)$"))
        (pcre2 . (are-re-search-forward
                  "([A-Z]{4})[ ]+([0-9]{3})$")))

      (are-test-regexp nil 2
        (emacs . (re-search-forward "\\([A-Z]\\) \\([0-9]\\)"))
        (pcre2 . (are-re-search-forward "([A-Z]) ([0-9])")))

      (are-test-regexp nil 2
        (emacs . (re-search-forward "\\([A-Z]\\) \\([0-9]\\)" 5))
        (pcre2 . (are-re-search-forward "([A-Z]) ([0-9])" 5)))

      (are-test-regexp nil 2
        (emacs . (re-search-forward "\\([A-Z]\\) \\([0-9]\\)" 5 nil))
        (pcre2 . (are-re-search-forward "([A-Z]) ([0-9])" 5 nil)))

      (are-test-regexp nil 2
        (emacs . (re-search-forward "\\([A-Z]\\) \\([0-9]\\)" 5 t))
        (pcre2 . (are-re-search-forward "([A-Z]) ([0-9])" 5 t)))

      (are-test-regexp nil 2
        (emacs . (re-search-forward "\\([A-Z]\\) \\([0-9]\\)" 5 'noerr))
        (pcre2 . (are-re-search-forward "([A-Z]) ([0-9])" 5 'noerr)))

      (are-test-regexp nil 2
        (emacs . (re-search-forward "\\([A-Z]\\) \\([0-9]\\)" nil nil 1))
        (pcre2 . (are-re-search-forward "([A-Z]) ([0-9])" nil nil 1)))

      (are-test-regexp nil 2
        (emacs . (re-search-forward "\\([A-Z]\\) \\([0-9]\\)" nil nil 2))
        (pcre2 . (are-re-search-forward "([A-Z]) ([0-9])" nil nil 2)))

      (are-test-regexp nil 2
        (emacs . (re-search-forward "\\([A-Z]\\) \\([0-9]\\)" nil nil 3))
        (pcre2 . (are-re-search-forward "([A-Z]) ([0-9])" nil nil 3)))

      (are-test-regexp nil 2
        (emacs . (re-search-forward "\\([A-Z]\\) \\([0-9]\\)" nil nil 1000))
        (pcre2 . (are-re-search-forward "([A-Z]) ([0-9])" nil nil 1000)))

      (are-test-regexp nil 2
        (emacs . (re-search-forward "\\([A-Z]\\) \\([0-9]\\)" nil t 1000))
        (pcre2 . (are-re-search-forward "([A-Z]) ([0-9])" nil t 1000)))

      (are-test-regexp nil 2
        (emacs . (re-search-forward "\\([A-Z]\\) \\([0-9]\\)" nil 'noerr 1000))
        (pcre2 . (are-re-search-forward "([A-Z]) ([0-9])" nil 'noerr 1000)))

      (are-test-regexp nil 2
        (emacs . (re-search-forward "\\([A-Z]\\) \\([0-9]\\)" 10 nil 500))
        (pcre2 . (are-re-search-forward "([A-Z]) ([0-9])" 10 nil 500)))

      (are-test-regexp nil 2
        (emacs . (re-search-forward "\\([A-Z]\\) \\([0-9]\\)" 11 t 500))
        (pcre2 . (are-re-search-forward "([A-Z]) ([0-9])" 11 t 500)))

      (are-test-regexp nil 2
        (emacs . (re-search-forward "\\([A-Z]\\) \\([0-9]\\)" 10 'noerr 500))
        (pcre2 . (are-re-search-forward "([A-Z]) ([0-9])" 10 'noerr 500)))

      (are-test-regexp nil 1
        (emacs . (re-search-forward "\\([0-9]+\\)"))
        (pcre2 . (are-re-search-forward "([0-9]+)")))

      (are-test-regexp nil 0
        (emacs . (re-search-forward "XYZ"))
        (pcre2 . (are-re-search-forward "XYZ")))

      (are-test-regexp nil 0
        (emacs . (re-search-forward "XYZ" nil t))
        (pcre2 . (are-re-search-forward "XYZ" nil t)))

      (are-test-regexp nil 0
        (emacs . (re-search-forward "XYZ" nil 'noerr))
        (pcre2 . (are-re-search-forward "XYZ" nil 'noerr)))

      ;; unicode
      (are-test-regexp nil 0
        (emacs . (re-search-forward (are-test-ucs-str "PIE")))
        (pcre2 . (are-re-search-forward (are-test-ucs-str "PIE"))))

      (are-test-regexp nil 0
        (emacs . (re-search-forward (are-test-ucs-str "PIE" "PINEAPPLE")))
        (pcre2 . (are-re-search-forward (are-test-ucs-str "PIE" "PINEAPPLE"))))

      (are-test-regexp nil 3
        (emacs . (re-search-forward (format "\\(%s\\)\\(.*\\)\\(%s\\)"
                                            (are-test-ucs-str "PIE")
                                            (are-test-ucs-str "CAT"))))
        (pcre2 . (are-re-search-forward (format "(%s)(.*)(%s)"
                                                (are-test-ucs-str "PIE")
                                                (are-test-ucs-str "CAT")))))

      ;; search forward after `point-min'.
      (forward-char 3)

      (are-test-regexp nil 0
        (emacs . (re-search-forward "[a-zA-Z0-9]"))
        (pcre2 . (re-search-forward "[a-zA-Z0-9]")))

      (are-test-regexp nil 0
        (emacs . (re-search-forward "^[a-zA-Z0-9]+"))
        (pcre2 . (re-search-forward "^[a-zA-Z0-9]+")))

      ;; search backward
      (goto-char (point-max))

      (are-test-regexp nil 1
        (emacs . (re-search-forward "\\(.*\\)" nil nil -1))
        (pcre2 . (are-re-search-forward "(.*)" nil nil -1)))

      (are-test-regexp nil 1
        (emacs . (re-search-forward "\\([a-b]\\)" -10 nil -1))
        (pcre2 . (are-re-search-forward "([a-b])" -10 nil -1)))

      (are-test-regexp nil 1
        (emacs . (re-search-forward "\\(.*\\)" 10 nil nil -1))
        (pcre2 . (are-re-search-forward "(.*)" 10 nil nil -1)))

      (are-test-regexp nil 1
        (emacs . (re-search-forward "\\(.*\\)" (- (point-max) 5) nil nil -1))
        (pcre2 . (are-re-search-forward "(.*)" (- (point-max) t) nil nil -1)))

      (are-test-regexp nil 2
        (emacs . (re-search-forward "\\([A-Z]\\) \\([0-9]\\)" nil nil -1))
        (pcre2 . (are-re-search-forward "([A-Z]) ([0-9])" nil nil -1)))

      (are-test-regexp nil 2
        (emacs . (re-search-forward
                  "\\([A-Z]\\) \\([0-9]\\)" (1- (point-max)) nil -1))
        (pcre2 . (are-re-search-forward
                  "([A-Z]) ([0-9])" (1- (point-max)) nil -1)))

      (are-test-regexp nil 2
        (emacs . (re-search-forward
                  "\\([A-Z]\\) \\([0-9]\\)" (1- (point-max)) t -1))
        (pcre2 . (are-re-search-forward
                  "([A-Z]) ([0-9])" (1- (point-max)) t -1)))

      (are-test-regexp nil 2
        (emacs . (re-search-forward
                  "\\([A-Z]\\) \\([0-9]\\)" (1- (point-max)) 'noerr -1))
        (pcre2 . (are-re-search-forward
                  "([A-Z]) ([0-9])" (1- (point-max)) 'noerr -1)))

      (are-test-regexp nil 2
        (emacs . (re-search-forward
                  "\\([A-Z]\\) \\([0-9]\\)" (- (point-max) 2) nil -1))
        (pcre2 . (are-re-search-forward
                  "([A-Z]) ([0-9])" (- (point-max) 2) nil -1)))

      (are-test-regexp nil 2
        (emacs . (re-search-forward
                  "\\([A-Z]\\) \\([0-9]\\)" (- (point-max) 2) t -1))
        (pcre2 . (are-re-search-forward
                  "([A-Z]) ([0-9])" (- (point-max) 2) t -1)))

      (are-test-regexp nil 2
        (emacs . (re-search-forward
                  "\\([A-Z]\\) \\([0-9]\\)" (- (point-max) 2) 'noerr -1))
        (pcre2 . (are-re-search-forward
                  "([A-Z]) ([0-9])" (- (point-max) 2) 'noerr -1)))

      (are-test-regexp nil 2
        (emacs . (re-search-forward "\\([A-Z]\\) \\([0-9]\\)" nil nil -2))
        (pcre2 . (are-re-search-forward "([A-Z]) ([0-9])" nil nil -2)))

      (are-test-regexp nil 2
        (emacs . (re-search-forward "\\([A-Z]\\) \\([0-9]\\)" nil nil -3))
        (pcre2 . (are-re-search-forward "([A-Z]) ([0-9])" nil nil -3)))

      (are-test-regexp nil 0
        (emacs . (re-search-forward "[a-z][a-z]" nil nil -1))
        (pcre2 . (are-re-search-forward "[a-z][a-z]" nil nil -1)))

      (are-test-regexp nil 0
        (emacs . (re-search-forward "[a-z][a-z]" nil nil -2))
        (pcre2 . (are-re-search-forward "[a-z][a-z]" nil nil -2)))

      (are-test-regexp nil 0
        (emacs . (re-search-forward "[a-z][a-z]" nil nil -3))
        (pcre2 . (are-re-search-forward "[a-z][a-z]" nil nil -3)))

      (are-test-regexp nil 2
        (emacs . (re-search-forward "\\([a-z]\\)\\([a-z]\\)" nil nil -4))
        (pcre2 . (are-re-search-forward "([a-z])([a-z])" nil nil -4)))

      (are-test-regexp nil 0
        (emacs . (re-search-forward "[[:space:]][0-9][0-9]" nil nil -2))
        (pcre2 . (are-re-search-forward "[\\s][0-9][0-9]" nil nil -2)))

      (are-test-regexp nil 0
        (emacs . (re-search-forward "[[:space:]][0-9][0-9]" nil nil -1000))
        (pcre2 . (are-re-search-forward "[\\s][0-9][0-9]" nil nil -1000)))

      (are-test-regexp nil 0
        (emacs . (re-search-forward "[[:space:]][0-9][0-9]" nil t -1000))
        (pcre2 . (are-re-search-forward "[\\s][0-9][0-9]" nil t -1000)))

      (are-test-regexp nil 0
        (emacs . (re-search-forward "[[:space:]][0-9][0-9]" nil 'noerr -1000))
        (pcre2 . (are-re-search-forward "[\\s][0-9][0-9]" nil 'noerr -1000)))

      (are-test-regexp nil 0
        (emacs . (re-search-forward "[[:space:]][0-9][0-9]" 10 nil -500))
        (pcre2 . (are-re-search-forward "[\\s][0-9][0-9]" 10 nil -500)))

      (are-test-regexp nil 0
        (emacs . (re-search-forward "[[:space:]][0-9][0-9]" 10 t -500))
        (pcre2 . (are-re-search-forward "[\\s][0-9][0-9]" 10 t -500)))

      (are-test-regexp nil 0
        (emacs . (re-search-forward "[[:space:]][0-9][0-9]" 10 'noerr -500))
        (pcre2 . (are-re-search-forward "[\\s][0-9][0-9]" 10 'noerr -500)))

      (are-test-regexp nil 0
        (emacs . (re-search-forward
                  (are-test-ucs-str "DIGIT NINE COMMA") nil nil -1))
        (pcre2 . (are-re-search-forward
                  (are-test-ucs-str "DIGIT NINE COMMA") nil nil -1)))

      (are-test-regexp nil 0
        (emacs . (re-search-forward (are-test-ucs-str "CAT") nil nil -3))
        (pcre2 . (are-re-search-forward (are-test-ucs-str "CAT") nil nil -3)))

      ;; search backward before `point-max'
      (backward-char 5)

      (are-test-regexp nil 1
        (emacs . (re-search-forward "\\([a-zA-Z0-9]\\)" nil nil -1))
        (pcre2 . (are-re-search-forward "([a-zA-Z0-9])" nil nil -1)))

      (are-test-regexp nil 2
        (emacs . (re-search-forward "\\([A-Z]\\) \\([0-9]\\)" nil nil -1))
        (pcre2 . (are-re-search-forward "([A-Z]) ([0-9])" nil nil -1)))

      (are-test-regexp nil 2
        (emacs . (re-search-forward "\\([A-Z]\\) \\([0-9]\\)" nil nil -2))
        (pcre2 . (are-re-search-forward "([A-Z]) ([0-9])" nil nil -2)))

      (are-test-regexp nil 2
        (emacs . (re-search-forward "\\([A-Z]\\) \\([0-9]\\)" nil nil -3))
        (pcre2 . (are-re-search-forward "([A-Z]) ([0-9])" nil nil -3)))

      ;; neither forward nor backward
      (goto-char (point-min))

      (are-test-regexp nil 1
        (emacs . (re-search-forward "\\([A-Z]\\)" nil nil 0))
        (pcre2 . (are-re-search-forward "([A-Z])" nil nil 0)))

      (are-test-regexp nil 2
        (emacs . (re-search-forward "\\([A-Z]\\)\\([A-Z]\\)" nil nil 0))
        (pcre2 . (are-re-search-forward "([A-Z])([A-Z])" nil nil 0)))

      (are-test-regexp nil 1
        (emacs . (re-search-forward "\\(.*\\)" -10 nil 0))
        (pcre2 . (are-re-search-forward "(.*)" -10 nil 0)))

      (are-test-regexp nil 1
        (emacs . (re-search-forward "\\(.*\\)" -1000 nil 0))
        (pcre2 . (are-re-search-forward "(.*)" -1000 nil 0)))

      (are-test-regexp nil 1
        (emacs . (re-search-forward "\\(.*\\)" -1000 t 0))
        (pcre2 . (are-re-search-forward "(.*)" -1000 t 0)))

      (are-test-regexp nil 1
        (emacs . (re-search-forward "\\(.*\\)" 1000 nil 0))
        (pcre2 . (are-re-search-forward "(.*)" 1000 nil 0)))

      (are-test-regexp nil 1
        (emacs . (re-search-forward "\\(.*\\)" 1000 t 0))
        (pcre2 . (are-re-search-forward "(.*)" 1000 t 0)))

      (forward-char 5)

      (are-test-regexp nil 1
        (emacs . (re-search-forward "\\(.*\\)" -1000 nil 0))
        (pcre2 . (are-re-search-forward "(.*)" -1000 nil 0)))

      (are-test-regexp nil 1
        (emacs . (re-search-forward "\\(.*\\)" 1000 nil 0))
        (pcre2 . (are-re-search-forward "(.*)" 1000 nil 0)))

      (are-test-regexp nil 2
        (emacs . (re-search-forward "\\([A-Z]\\)\\([A-Z]\\)" nil nil 0))
        (pcre2 . (are-re-search-forward "([A-Z])([A-Z])" nil nil 0)))

      (are-test-regexp nil 3
        (emacs . (re-search-forward "\\([A-Z]\\)\\([A-Z]\\)\\(.*\\)" nil nil 0))
        (pcre2 . (are-re-search-forward "([A-Z])([A-Z])(.*)" nil nil 0)))

      (are-test-regexp nil 3
        (emacs . (re-search-forward "notfound" nil nil 0))
        (pcre2 . (are-re-search-forward "notfound" nil nil 0)))

      (are-test-regexp nil 3
        (emacs . (re-search-forward "notfound" 2 nil 0))
        (pcre2 . (are-re-search-forward "notfound" 2 nil 0)))

      (are-test-regexp nil 3
        (emacs . (re-search-forward "notfound" 200 nil 0))
        (pcre2 . (are-re-search-forward "notfound" 200 nil 0)))

      (are-test-regexp nil 3
        (emacs . (re-search-forward "notfound" nil t 0))
        (pcre2 . (are-re-search-forward "notfound" nil t 0)))

      (are-test-regexp nil 3
        (emacs . (re-search-forward "notfound" nil 'noerr 0))
        (pcre2 . (are-re-search-forward "notfound" nil 'noerr 0)))

      (are-test-regexp nil 3
        (emacs . (re-search-forward "notfound" 2 'noerr 0))
        (pcre2 . (are-re-search-forward "notfound" 2 'noerr 0)))

      (are-test-regexp nil 3
        (emacs . (re-search-forward "notfound" 10 'noerr 0))
        (pcre2 . (are-re-search-forward "notfound" 10 'noerr 0))))))

(ert-deftest are-test-string-match ()
  "Test for `are-string-match'."
  (let ((are-compile-options '(pcre2-multiline pcre2-utf))
        (case-fold-search nil)
        (str (concat
              "abcd efgh ijkl 01234\n"
              "ABCD EFGH 987\n"
              "FOO 1\n"
              (are-test-ucs-str
               "PIE" "PINEAPPLE" "CINEMA" "BICYCLE" "SHIP" "METRO" "CAT"
               "SMALL AIRPLANE" "BED" "HAPPY PERSON RAISING ONE HAND"
               "CAT" "CAT FACE" "CAT FACE WITH TEARS OF JOY" "CAT"
               "COPYLEFT SYMBOL" "CYCLONE" "FOGGY" "CLOSED UMBRELLA"
               "DIGIT ZERO FULL STOP" "DIGIT ZERO COMMA"
               "DIGIT ONE COMMA" "DIGIT TWO COMMA" "DIGIT THREE COMMA"
               "DIGIT FOUR COMMA" "DIGIT FIVE COMMA" "DIGIT SIX COMMA"
               "DIGIT SEVEN COMMA" "DIGIT EIGHT COMMA"
               "DIGIT NINE COMMA")
              "\n"
              (are-test-ucs-str
               "PIE" "PINEAPPLE" "CINEMA" "CAT" "CAT FACE" "BED" "PIE")
              "\n"
              "foo bar Baz qUX 111 222 321\n")))
    (are-test-regexp str 0
      (emacs . (string-match "\\`[a-z]+" str))
      (pcre2 . (are-string-match "^[a-z]+" str)))

    (are-test-regexp str 0
      (emacs . (string-match "[a-z]+" str 6))
      (pcre2 . (are-string-match "[a-z]+" str 6)))

    (are-test-regexp str 0
      (emacs . (string-match ".*" str 1000))
      (pcre2 . (are-string-match ".*" str 1000)))

    (are-test-regexp str 0
      (emacs . (string-match ".*" str -1000))
      (pcre2 . (are-string-match ".*" str -1000)))

    (are-test-regexp str 1
      (emacs . (string-match "\\([0-9]\\)" str))
      (pcre2 . (are-string-match "([0-9])" str)))

    (are-test-regexp str 1
      (emacs . (string-match "\\([0-9]+\\)$" str))
      (pcre2 . (are-string-match "([0-9]+)$" str)))

    (are-test-regexp str 2
      (emacs . (string-match "\\([0-9]\\)\\([0-9]\\)" str))
      (pcre2 . (are-string-match "([0-9])([0-9])" str)))

    (are-test-regexp str 2
      (emacs . (string-match "\\([A-Z]+\\).*\\([0-9]+\\)" str))
      (pcre2 . (are-string-match "([A-Z]+).*([0-9]+)" str)))

    (are-test-regexp str 1
      (emacs . (string-match "\\(.*\\)" str))
      (pcre2 . (are-string-match "(.*)" str)))

    (are-test-regexp str 0
      (emacs . (string-match (are-test-ucs-str "PIE") str))
      (pcre2 . (are-string-match (are-test-ucs-str "PIE") str)))

    (are-test-regexp str 0
      (emacs . (string-match (are-test-ucs-str "PIE") str 20))
      (pcre2 . (are-string-match (are-test-ucs-str "PIE") str 20)))

     (are-test-regexp str 0
       (emacs . (string-match
                 (concat (are-test-ucs-str "PIE" "PINEAPPLE")
                         "."
                         (are-test-ucs-str "CAT"))
                 str))
       (pcre2 . (are-string-match
                 (concat (are-test-ucs-str "PIE" "PINEAPPLE")
                         "."
                         (are-test-ucs-str "CAT"))
                 str)))

     (are-test-regexp str 3
       (emacs . (string-match
                 (concat "\\(" (are-test-ucs-str "CAT") "\\)"
                         "\\(.*\\)"
                         "\\(" (are-test-ucs-str "CAT") "\\)")
                 str))
       (pcre2 . (are-string-match
                 (concat "(" (are-test-ucs-str "CAT") ")"
                         "(.*)"
                         "(" (are-test-ucs-str "CAT") ")")
                         str)))

    (are-test-regexp str 0
      (emacs . (string-match "" str))
      (pcre2 . (are-string-match "" str)))

    (are-test-regexp "" 0
      (emacs . (string-match ".*" ""))
      (pcre2 . (are-string-match ".*" "")))

    (are-test-regexp "" 0
      (emacs . (string-match "" ""))
      (pcre2 . (are-string-match "" "")))))


(ert-deftest are-test-engine ()
  "Tests for `are-engine'."
  (should (equal (are-engine (are-compile "[a-z]" 0 'pcre2)) 'pcre2)))

(provide 'are-test)

;;; are-test.el ends here
