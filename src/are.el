;;; are.el --- Additional Regexp Engines -*- lexical-binding: t; -*-

;; Copyright (c) 2019, Hans Jerry Illikainen <hji@dyntopia.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; This package exposes additional regexp engines to Emacs.

;;; Code:
(require 'cl-lib)
(require 'libare)

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

(defvar-local are--active nil
  "Whether ARE is used in the current buffer.")

;;
;; `occur'.
;;
(defun are-occur (&optional fn)
  "Use `occur' with are."
  (interactive)
  (call-interactively #'occur))

(defun are-occur-1 (fun regexp nlines bufs &optional buf-name)
  "Advice for `occur-1'.

The relevant regexp functions are let-bound if `occur-1' is (or
was) invoked through `are-occur'.  Furthermore, a hook is
temporarily added that prepares the resulting occur buffer for
`revert-buffer' (hence the was).

The reason for the temporary hook is that other hooks may change
the name of the occur buffer before the original implementation
of `occur-1' return, so the setup can't be done with an occur
buffer from `get-buffer'."
  (if (or (eq this-command 'are-occur)
          (and (derived-mode-p 'occur-mode) are--active))
      (cl-letf* (((symbol-function 're-search-forward) #'are-re-search-forward)
                 ((symbol-function 'string-match) #'are-string-match)
                 (hook (lambda () (setq are--active t)))
                 (occur-mode-hook `(,hook ,@occur-mode-hook)))
        (funcall fun regexp nlines bufs buf-name))
    (funcall fun regexp nlines bufs buf-name)))

;;
;; Non-interactive functions.
;;
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

(defun are-string-match (regexp string &optional start)
  "Search for REGEXP in STRING, starting at START.

See `string-match'."
  (condition-case err
      (let* ((re (are-compile regexp))
             (str (substring string start))
             (mdata (are--adjust-match-data (are-match re str)
                                            (if (and start (cl-minusp start))
                                                (+ (length string) start)
                                              start))))
        (are--set-match-data (or mdata '(0 0)))
        (car mdata))
    (args-out-of-range
     ;; For compatibility with `string-match'.
     (signal (car err) (list (cadr err) (caddr err))))))

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
                     (point))
                    ((and (> cnt 0) (< bound (point)))
                     (error "Invalid search bound (wrong side of point)"))
                    ((and (<= cnt 0) (> bound (point)))
                     (error "Invalid search bound (wrong side of point)"))
                    ((and (= cnt 0) (< bound (point-min)))
                     (point-min))
                    ((> cnt 0)
                     (min bound (point-max)))
                    ((<= cnt 0)
                     (point))))
         (str (buffer-substring start end))
         (pt (cond ((> cnt 0)
                    (cadr (are--re-search-forward re str start cnt)))
                   ((< cnt 0)
                    (car (are--re-search-backward re str start cnt)))
                   ((eq cnt 0)
                    ;; If COUNT is 0, `re-search-forward' validates the bound
                    ;; and signals if it's invalid.  However, no actual search
                    ;; seem to take place.  Instead, the start and end
                    ;; positions of `match-data' is set to `point', and `point'
                    ;; is returned..
                    (set-match-data (list (set-marker (make-marker) (point))
                                          (set-marker (make-marker) (point))))
                    (point)))))
    (if pt
        (goto-char pt)
      (cl-case noerror
        ((nil)
         (signal 'search-failed (list regexp)))
        ((t)
         nil)
        (otherwise
         (if (> cnt 0)
             (goto-char end)
           (goto-char start)))))
    pt))

(defun are-re-search-backward (regexp &optional bound noerror count)
  "Search backward from `point' for REGEXP.

See `re-search-backward'."
  (are-re-search-forward regexp bound noerror (- (or count 1))))

(defun are--re-search-forward (re str offset count)
  "Do a forward search with `are-re-search-forward'."
  (let ((start 0)
        mdata)
    (while (> count 0)
      (setq mdata (are-match re (substring str start)))
      (if (null mdata)
          (setq count 0)
        (cl-decf count)
        (setq mdata (are--adjust-match-data mdata (+ offset start)))
        (setq start (- (cadr mdata) (point)))
        (are--set-match-data mdata t)))
    mdata))

(defun are--re-search-backward (re str offset count)
  "Do a backward search with `are-re-search-forward'."
  (let* ((start (length str))
         (end start)
         mdata)
    (while (and (< count 0) (>= start 0) (>= end 0))
      (setq mdata (are-match re (substring str start end)))
      (when mdata
        (cl-incf count)

        (if (null (eq start end))
            (setq mdata (are--adjust-match-data mdata (+ offset start)))
          ;; If we have a match in a zero-length string, the regexp is
          ;; something akin to .* -- and in that case we may as well pretend
          ;; that we have COUNT successful matches already since that is that
          ;; would happen if we continue iterating until the end.
          (setq mdata (are--adjust-match-data mdata (point) t))
          (setq count 0))

        (are--set-match-data mdata t)
        (setq end start))
      (cl-decf start))
    (when (zerop count)
      mdata)))

(defun are--adjust-match-data (mdata &optional start absolute)
  "Add START to each element in MDATA.

If ABSOLUTE is non-nil, set each element to START instead."
  (mapcar (lambda (elt)
            (if absolute
                (or start 0)
              (+ elt (or start 0))))
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

(defun are--debug (fmt &rest args)
  "Print a debug message."
  (when are-debug
    (apply #'message fmt args)))

;;
;; Minor mode.
;;
(define-minor-mode are-mode
  "Minor mode for ARE."
  :init-value nil
  :global t
  (cl-case are-mode
    ((t)
     (advice-add 'occur-1 :around #'are-occur-1))
    ((nil)
     (advice-remove 'occur-1 #'are-occur-1))))

(provide 'are)

;;; are.el ends here
