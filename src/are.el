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
  '((pcre2 . (multiline utf)))
  "Default compile options."
  :group 'are
  :type 'list)

(defcustom are-match-options nil
  "Default match options."
  :group 'are
  :type 'list)

(defcustom are-debug nil
  "Whether to show debug messages."
  :group 'are
  :type 'boolean)

(defvar are--debug-buffer "*are*"
  "Name of the debug buffer.")

(defvar are--debug-last nil
  "Last debug message.")

(defvar-local are--active nil
  "Whether ARE is used in the current buffer.")

(defmacro are-override (callers regexp &rest body)
  "Temporarily override built-in regexp functions and execute BODY.

The conditions for overriding the built-in functions are that the
caller must be one of CALLERS and its regular expression must be
equal to REGEXP."
  (declare (indent defun))
  `(cl-letf* ((callers (if (consp ,callers)
                           ,callers
                         (list ,callers)))
              (f (lambda (or-fun orig-fun)
                   (lambda (reg &rest args)
                     (let* ((sym (intern (subr-name orig-fun)))
                            (frame (backtrace-frame 1 sym))
                            (fun (cadr frame)))
                       (if (and (memq fun callers) (equal ,regexp reg))
                           (apply or-fun reg args)
                         (are--debug "%S: not overriding for '%S'"
                                     (if (symbolp fun)
                                         fun
                                       (type-of fun))
                                    reg)
                         (apply orig-fun reg args))))))
              (orig-looking-at (symbol-function 'looking-at))
              (orig-looking-at-p (symbol-function 'looking-at-p))
              (orig-re-search-forward (symbol-function 're-search-forward))
              (orig-re-search-backward (symbol-function 're-search-backward))
              (orig-string-match (symbol-function 'string-match))
              (orig-string-match-p (symbol-function 'string-match-p))
              ((symbol-function 'looking-at)
               (funcall f #'are-looking-at orig-looking-at))
              ((symbol-function 'looking-at-p)
               (funcall f #'are-looking-at-p orig-looking-at-p))
              ((symbol-function 're-search-forward)
               (funcall f #'are-re-search-forward orig-re-search-forward))
              ((symbol-function 're-search-backward)
               (funcall f #'are-re-search-backward orig-re-search-backward))
              ((symbol-function 'string-match)
               (funcall f #'are-string-match orig-string-match))
              ((symbol-function 'string-match-p)
               (funcall f #'are-string-match-p orig-string-match-p)))
     ,@body))

;;
;; `hi-lock-mode'
;;
(declare-function hi-lock-read-face-name "hi-lock")
(declare-function hi-lock-regexp-okay "hi-lock")
(declare-function hi-lock-set-pattern "hi-lock")

(defun are-hi-lock-face-buffer (&optional regexp face)
  "Highlight each match of REGEXP with FACE."
  (interactive)
  (require 'hi-lock)
  (let ((regexp (or regexp (read-regexp "Regexp to highlight" 'regexp-history)))
        (face (or face (hi-lock-read-face-name)))
        ;; If hi-lock detects that `font-lock-mode' is enabled it will add the
        ;; patterns as font-lock keywords, which would circumvent ARE.
        font-lock-mode)
    (when (hi-lock-regexp-okay regexp)
      (are-override #'hi-lock-set-pattern regexp
        (hi-lock-set-pattern regexp face)))))

(defun are-hi-lock-unface-buffer ()
  "Remove highlighting of REGEXP."
  (interactive)
  (call-interactively #'hi-lock-unface-buffer))

;;
;; `isearch-mode'
;;
(defun are-isearch-forward-regexp ()
  "Search forward."
  (interactive)
  (let ((isearch-search-fun-function #'are-isearch-search-fun-function))
    (isearch-forward-regexp)))

(defun are-isearch-backward-regexp ()
  "Search backward."
  (interactive)
  (let ((isearch-search-fun-function #'are-isearch-search-fun-function))
    (isearch-backward-regexp)))

(defun are-isearch-repeat-forward ()
  "Repeat a search forwards."
  (interactive)
  (let ((isearch-search-fun-function #'are-isearch-search-fun-function))
    (isearch-repeat-forward)))

(defun are-isearch-repeat-backward ()
  "Repeat a search backwards."
  (interactive)
  (let ((isearch-search-fun-function #'are-isearch-search-fun-function))
    (isearch-repeat-backward)))

(defun are-isearch-repeat (&optional arg)
  "Repeat a search in the previous direction.

With a non-nil ARG the search repeats in the opposite direction."
  (interactive "P")
  (if (if arg (not isearch-forward) isearch-forward)
      (are-isearch-repeat-forward)
    (are-isearch-repeat-backward)))

(defun are-isearch (&optional arg)
  "Start an incremental search forwards.

With a non-nil ARG the search is made backwards."
  (interactive "P")
  (if arg
      (are-isearch-backward-regexp)
    (are-isearch-forward-regexp)))

(defun are-isearch-search-fun-function ()
  "Return a search function for isearch."
  (lambda (regexp &optional bound noerror count)
    (if isearch-forward
        (are-re-search-forward regexp bound noerror count)
      (are-re-search-backward regexp bound noerror count))))

;;
;; `occur'.
;;
(defun are-occur (regexp &optional nlines region)
  "Use occur with are.

See `occur' for an explanation of REGEXP, NLINES and REGION."
  (interactive
   (nconc (occur-read-primary-args)
          (and (use-region-p) (list (region-bounds)))))
  (setq this-command #'are-occur)
  (occur regexp nlines region))

(defun are-occur-1 (fun regexp nlines bufs &optional buf-name)
  "Advice for `occur-1'.

The built-in regexp functions are overridden if `occur-1' is (or
was) invoked through `are-occur'.  Furthermore, a hook is
temporarily added that prepares the resulting occur buffer for
`revert-buffer' (hence the was).

The reason for the temporary hook is that other hooks may change
the name of the occur buffer before the original implementation
of `occur-1' return, so the setup can't be done with an occur
buffer from `get-buffer'.

See `occur-1' for a description of FUN REGEXP NLINES BUFS and
BUF-NAME."
  (if (or (eq this-command 'are-occur)
          (and (derived-mode-p 'occur-mode) are--active))
      (are-override #'occur-engine regexp
        (let* ((hook (lambda () (setq are--active t)))
               (occur-mode-hook `(,hook ,@occur-mode-hook)))
          (funcall fun regexp nlines bufs buf-name)))
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
         (options (or options (alist-get engine are-match-options))))
    (are--match regexp str options)))

(defun are-engine (regexp)
  "Return engine for a compiled REGEXP."
  (are--engine regexp))

(defun are-looking-at (regexp)
  "Return t if the text after point match REGEXP."
  (save-excursion
    (let ((are-compile-options '((pcre2 . (anchored)))))
      (and (are-re-search-forward regexp (line-end-position) t) t))))

(defun are-looking-at-p (regexp)
  "Return t if the text after point match REGEXP.

This function preserves match data.  See `looking-at-p'."
  (save-match-data
    (are-looking-at regexp)))

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
        (are--set-match-data mdata)
        (car mdata))
    (args-out-of-range
     ;; For compatibility with `string-match'.
     (signal (car err) (list (cadr err) (caddr err))))))

(defun are-string-match-p (regexp string &optional start)
  "Search for REGEXP in STRING, starting at START.

This preserves the match data.  See `string-match-p'."
  (save-match-data
    (are-string-match regexp string start)))

(defun are-re-search-forward (regexp &optional bound noerror count)
  "Search forward from `point' for REGEXP.

See `re-search-forward' for a description of BOUND NOERROR and
COUNT."
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

See `re-search-backward' for a description of BOUND NOERROR and
COUNT."
  (are-re-search-forward regexp bound noerror (- (or count 1))))

(defun are--re-search-forward (re str offset count)
  "Search forward for COUNT matches for RE in STR with OFFSET."
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
  "Search backward for COUNT matches for RE in STR with OFFSET."
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
  "Format FMT with ARGS and insert it in `are--debug-buffer'."
  (when are-debug
    (let ((str (apply #'format (concat fmt "\n") args)))
      (unless (equal str are--debug-last)
        (setq are--debug-last str)
        (with-current-buffer (get-buffer-create are--debug-buffer)
          (let ((inhibit-read-only t))
            (save-excursion
              (goto-char (point-max))
              (insert str))))))))

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
