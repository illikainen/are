;;; are.el --- Additional Regexp Engines -*- lexical-binding: t; -*-

;; Copyright (c) 2019, Hans Jerry Illikainen <hji@dyntopia.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; This package exposes additional regexp engines to Emacs.

;;; Code:
(defgroup are nil
  "Additional Regexp Engines"
  :group 'are
  :prefix "are-")

(defcustom are-engine 'pcre2
  "Regexp engine to use."
  :group 'are
  :type '(choice (const :tag "PCRE2" pcre2)))

(defcustom are-compile-options
  '((pcre2 . (pcre2-multiline pcre2-utf)))
  "Default compile options."
  :group 'are
  :type 'list)

(defcustom are-match-options nil
  "Default match options."
  :group 'are
  :type 'list)

(defcustom are-debug nil
  "Whether to show debug messages"
  :group 'are
  :type 'boolean)

(defcustom are-library
  (concat (file-name-as-directory
           (directory-file-name
            (file-name-directory
             (or load-file-name buffer-file-name))))
          "libare")
  "Shared library."
  :group 'are
  :type 'string)

(defun are-load ()
  "Load `are-library'."
  (load are-library))


(defun are-compile (regexp &optional options engine)
  "Compile REGEXP with OPTIONS for ENGINE."
  (let* ((engine (or engine are-engine))
         (options (or options (alist-get engine are-compile-options))))
    (are--compile regexp options engine)))

(defun are-match (regexp str &optional options)
  "Match STR with REGEXP and OPTIONS."
  (let* ((engine (are-engine regexp))
         (options (alist-get engine are-match-options)))
    (are--match regexp str options)))

(defun are-engine (regexp)
  "Return engine for a compiled REGEXP."
  (are--engine regexp))

(defun are--adjust-match-data (mdata &optional start)
  "Add START to each element in MDATA"
  (mapcar (lambda (elt)
            (+ elt (or start 0)))
          mdata))

(defun are--set-match-data (mdata &optional use-markers)
  "Set MDATA as match data.

If USE-MARKERS is non-nil, a marker is created for each element
in MDATA."
  (set-match-data (mapcar (lambda (elt)
                            (if use-markers
                                (set-marker (make-marker) elt)
                              elt))
                          mdata)))

(defun are-string-match (regexp string &optional start)
  "Search for REGEXP in STRING, starting at START.

See `string-match'."
  (let* ((re (are-compile regexp))
         (str (substring string start))
         (mdata (are--adjust-match-data (are-match re str) start)))
    (are--set-match-data mdata)
    (car mdata)))

(defun are-re-search-forward (regexp &optional bound noerror count)
  "Search forward from `point' for REGEXP.

See `re-search-forward'."
  (let* ((re (are-compile regexp))
         (cnt (or count 1))
         (start (cond ((> cnt 0)
                       (point))
                      ((not bound)
                       (point-min))
                      (t
                       (max (point-min) bound))))
         (end (cond ((and (> cnt 0) (not bound))
                     (point-max))
                    ((and (<= cnt 0) (not bound))
                     (point-max))
                    ((and (> cnt 0) (< bound (point)))
                     (signal 'args-out-of-range (list bound)))
                    ((and (<= cnt 0) (> bound (point)))
                     (signal 'args-out-of-range (list bound)))
                    ((and (= cnt 0) (< bound (point-min)))
                     (point-min))
                    ((> cnt 0)
                     (1- (+ (point) bound)))
                    ((<= cnt 0)
                     (1- (point)))))
         (str (buffer-substring start end))
         (pt (ignore-errors
               (cond ((> cnt 0)
                      (cadr (are--re-search-forward-forward re str cnt)))
                     ((< cnt 0)
                      (save-excursion
                        (goto-char start)
                        (car (are--re-search-forward-backward re str cnt))))
                     ((eq cnt 0)
                      ;; If COUNT is 0, `re-search-forward' validates the bound
                      ;; and signals if it's invalid.  However, no actual
                      ;; search seem to take place.  Instead, the start and end
                      ;; positions of `match-data' is set to `point', and
                      ;; `point' is returned..
                      (set-match-data (list (set-marker (make-marker) (point))
                                            (set-marker (make-marker) (point))))
                      (point))
                      ))))
    (if pt
        (goto-char pt)
      (cl-case noerror
        ((nil) (signal 'search-failed (list regexp)))
        ((t) nil)
        (otherwise
         (if (> cnt 0)
             (goto-char end)
           (goto-char start)))))
    pt))

(defun are--re-search-forward-forward (re str count)
  "Do a forward search with `are-re-search-forward'."
  (let ((offset 0)
        mdata)
    (dotimes (n count)
      (setq mdata (are-match re (substring str offset)))
      (unless mdata
        (signal 'search-failed '()))
      (setq mdata (are--adjust-match-data mdata (+ (point) offset)))
      (setq offset (cadr mdata))
      (are--set-match-data mdata t))
    mdata))

(defun are--re-search-forward-backward (re str count)
  "Do a backward search with `are-re-search-forward'."
  (let* ((start (length str))
         (end start)
         mdata)
    (while (and (< count 0) (> start 0))
      (setq mdata (are-match re (substring str start end)))
      (when mdata
        (setq mdata (are--adjust-match-data mdata (+ (point) start)))
        (setq end start)
        (are--set-match-data mdata t)
        (cl-incf count))
      (cl-decf start))
    mdata))

(provide 'are)

;;; are.el ends here
