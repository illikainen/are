;;; are-test.el --- tests for are -*- lexical-binding: t; -*-

;; Copyright (c) 2019, Hans Jerry Illikainen <hji@dyntopia.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:
(require 'cl-lib)
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

(defvar are-test-count 0
  "Test counter.")

(add-to-list 'load-path are-src-dir)

(require 'are)
(are-load)

(defun are-test-regexps (elts &optional str)
  "Compare the result of the sexps in ELTS."
  (are--debug "\n\nTest %d" (cl-incf are-test-count))
  (let (emacs-result result)
    (dolist (elt elts)
      (are--debug "Evaluating engine: %S -- %S" (car elt) (cdr elt))
      (save-excursion
        (save-match-data
          (condition-case err
              (let ((are-engine (car elt)))
                (push (eval (cdr elt)) result)
                (push (point) result)
                (push (match-data) result)
                (dotimes (n (length (match-data)))
                  (push (match-string n str) result)
                  (push (match-string-no-properties n str) result)
                  (push (match-beginning n) result)
                  (push (match-end n) result)))
            (search-failed
             ;; Can't push `cdr' since it contains the failed regexp, which
             ;; won't necessarily match between engines.
             (are--debug "Error: %S: %S" (car elt) err))
            (error
             (push err result)))))
      (cond ((eq (car elt) 'emacs)
             (setq emacs-result result)
             (setq result nil))
            (t
             (are--debug "Emacs result: %S" emacs-result)
             (are--debug "Other result: %S" result)
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

(defvar are-test-regexps
  `(((emacs . "\\(.*\\)")
     (pcre2 . "(.*)"))
    ((emacs . "[a-z]")
     (pcre2 . "[a-z]"))
    ((emacs . "\\([a-z]\\)\\([a-z]\\)")
     (pcre2 . "([a-z])([a-z])"))
    ((emacs . "\\([0-9][0-9]\\)\\(.*\\)")
     (pcre2 . "([0-9][0-9])(.*)"))
    ((emacs . ,(are-test-ucs-str "PIE"))
     (pcre2 . ,(are-test-ucs-str "PIE")))
    ((emacs . ,(format "\\(%s\\)\\(.*\\)\\(%s\\)"
                       (are-test-ucs-str "PIE")
                       (are-test-ucs-str "CAT")))
     (pcre2 . ,(format "(%s)(.*)(%s)"
                       (are-test-ucs-str "PIE")
                       (are-test-ucs-str "CAT")))))
  "Regexp test cases.")

(defvar are-test-strings
  `(,(concat "abcd efgh ijkl 01234 xyz\n"
             "ABCD EFGH 987\n"
             "FOO 1\n"
             (are-test-ucs-str "PIE" "PINEAPPLE" "CINEMA" "BICYCLE" "SHIP"
                               "METRO" "CAT" "SMALL AIRPLANE" "BED"
                               "HAPPY PERSON RAISING ONE HAND" "CAT"
                               "CAT FACE" "CAT FACE WITH TEARS OF JOY" "CAT"
                               "COPYLEFT SYMBOL" "CYCLONE" "FOGGY"
                               "CLOSED UMBRELLA" "DIGIT ZERO FULL STOP"
                               "DIGIT ZERO COMMA" "DIGIT ONE COMMA"
                               "DIGIT TWO COMMA" "DIGIT THREE COMMA"
                               "DIGIT FOUR COMMA" "DIGIT FIVE COMMA"
                               "DIGIT SIX COMMA" "DIGIT SEVEN COMMA"
                               "DIGIT EIGHT COMMA" "DIGIT NINE COMMA")
             "foo\n"
             "bar baz\n"
             "qux")
    "")
  "Test strings.")

(ert-deftest are-test-pcre2-re-search-forward ()
  "Tests for `are-re-search-forward'."
  (let ((case-fold-search nil))
    (dolist (str are-test-strings)
      (dolist (regexp are-test-regexps)
        (with-temp-buffer
          (insert str)
          (let ((positions (list (point-min)
                                 (+ (point-min) 1)
                                 (+ (point-min) 5)
                                 (point-max)
                                 (- (point-max) 1)
                                 (- (point-max) 5)))
                (bounds (list nil -100 -3 -2 -1 0 100
                              (point-min)
                              (+ (point-min) 1)
                              (+ (point-min) 5)
                              (point-max)
                              (- (point-max) 1)
                              (- (point-max) 5)))
                (counts (list nil -100 -3 -1 0 1 3 100))
                (noerrors (list nil t ''other)))
            (dolist (pos positions)
              (dolist (bound bounds)
                (dolist (count counts)
                  (dolist (noerror noerrors)
                    (goto-char pos)
                    (are-test-regexps
                     `((emacs . (re-search-forward
                                 ,(alist-get 'emacs regexp)
                                 ,bound
                                 ,noerror
                                 ,count))
                       (pcre2 . (are-re-search-forward
                                 ,(alist-get 'pcre2 regexp)
                                 ,bound
                                 ,noerror
                                 ,count))))))))))))))

(ert-deftest are-test-string-match ()
  "Test for `are-string-match'."
  (let ((case-fold-search nil))
    (dolist (str are-test-strings)
      (let ((starts (list -100 -10 -2 -1 0 1 2 (length str)
                          (- (length str) 1) (- (length str) 2))))
        (dolist (start starts)
          (dolist (regexp are-test-regexps)
            (are-test-regexps
             `((emacs . (string-match ,(alist-get 'emacs regexp)
                                      ,str ,start))
               (pcre2 . (are-string-match ,(alist-get 'pcre2 regexp)
                                          ,str ,start)))
             str)))))))

(ert-deftest are-test-engine ()
  "Tests for `are-engine'."
  (should (equal (are-engine (are-compile "[a-z]" nil 'pcre2)) 'pcre2)))

(provide 'are-test)

;;; are-test.el ends here
