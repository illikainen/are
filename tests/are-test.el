;;; are-test.el --- tests for are -*- lexical-binding: t; -*-

;; Copyright (c) 2019, Hans Jerry Illikainen <hji@dyntopia.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:
(require 'ert)

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

(provide 'are-test)

;;; are-test.el ends here
