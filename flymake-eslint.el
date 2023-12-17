;;; flymake-eslint.el --- A Flymake backend for Javascript using eslint  -*- lexical-binding: t; -*-

;; Version: 1.6.0
;; Author: Dan Orzechowski
;; Contributor: Terje Larsen
;; URL: https://github.com/orzechowskid/flymake-eslint
;; Package-Requires: ((emacs "26.1"))
;; Keywords: languages, tools

;;; Commentary:

;; A backend for Flymake which uses eslint.  Enable it with M-x
;; flymake-eslint-enable RET.  Alternately, configure a mode-hook for your
;; Javascript major mode of choice:

;; (add-hook 'some-js-major-mode-hook #'flymake-eslint-enable)

;; A handful of configurable options can be found in the flymake-eslint
;; customization group: view and modify them with the M-x customize-group RET
;; flymake-eslint RET.

;; License: MIT

;;; Code:

;;;; Requirements

;;;; Customization

(defgroup flymake-eslint nil
  "Flymake checker for Javascript using eslint."
  :group 'programming
  :prefix "flymake-eslint-")

(defcustom flymake-eslint-executable-name "eslint"
  "Name of executable to run when checker is called."
  :type 'string
  :group 'flymake-eslint)

(defcustom flymake-eslint-executable-args nil
  "Extra arguments to pass to eslint."
  :type '(choice string (repeat string))
  :group 'flymake-eslint)

(defcustom flymake-eslint-project-root nil
  "Project root directory."
  :type 'string
  :group 'flymake-eslint)

(defvar flymake-eslint--message-regexp
  "^[[:space:]]*\\([[:digit:]]+\\):\\([[:digit:]]+\\)[[:space:]]+\\(\\(?:error\\|warning\\)\\)[[:space:]]+\\(.+?\\)[[:space:]]\\{2,\\}\\(.*\\)$"
  "Regexp to match eslint messages.")

(defvar flymake-eslint-bin nil)
(defvar-local flymake-eslint--process nil
  "Eslint process for the current buffer.")

(defun flymake-eslint-find-eslint ()
  "Locate the eslint executable within a project or system-wide."
  (or (when-let* ((root (locate-dominating-file
                         (or (buffer-file-name)
                             default-directory)
                         "node_modules"))
                  (eslint (and root
                               (expand-file-name
                                (concat "node_modules/.bin/"
                                        (or flymake-eslint-executable-name
                                            "eslint"))
                                root))))
        (when (and (file-exists-p eslint)
                   (file-executable-p eslint))
          eslint))
      (executable-find "eslint")))

(defun flymake-eslint-find-project-root ()
  "Locate the nearest \"package.json\" to determine project root."
  (locate-dominating-file
   (or (buffer-file-name)
       default-directory)
   "package.json"))


;;;###autoload
(defun flymake-eslint-print-config ()
  "Print eslint config for current file."
  (interactive)
  (require 'json)
  (let* ((cmd (flymake-eslint-find-eslint))
         (file (or buffer-file-name (car (directory-files default-directory
                                                          nil
                                                          "\\.[jctm]s[x]?\\'"
                                                          t)))))
    (with-current-buffer (get-buffer-create
                          (format "*flymake-eslint-config-%s*"
                                  cmd))
      (setq buffer-read-only nil)
      (erase-buffer)
      (let ((status (call-process cmd nil t nil "--print-config" file)))
        (goto-char (point-min))
        (when (= 0 status)
          (when-let ((config
                      (when (fboundp 'json-read)
                        (json-read))))
            (delete-region (point-min)
                           (point-max))
            (insert (pp-to-string config)))))
      (pp-buffer)
      (delay-mode-hooks (emacs-lisp-mode)
                        (font-lock-ensure))
      (setq buffer-undo-list nil)
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (unless (get-buffer-window (current-buffer))
        (pop-to-buffer (current-buffer))))))


(defun flymake-eslint--report (eslint-stdout-buffer source-buffer)
  "Create Flymake diag messages from contents of ESLINT-STDOUT-BUFFER.
They are reported against SOURCE-BUFFER.
Return a list of results."
  (with-current-buffer eslint-stdout-buffer
    (goto-char (point-min))
    (if (looking-at-p "Error:")
        (pcase-let ((`(,beg . ,end)
                     (with-current-buffer source-buffer
                       (cons (point-min)
                             (point-max))))
                    (msg (thing-at-point 'line t)))
          (list (flymake-make-diagnostic source-buffer beg end :error msg)))
      (let ((current nil))
        (while
            (not
             (eobp))
          (if
              (looking-at flymake-eslint--message-regexp)
              (progn
                (setq current
                      (cons
                       (let* ((row
                               (string-to-number
                                (match-string 1)))
                              (column
                               (string-to-number
                                (match-string 2)))
                              (type
                               (match-string 3))
                              (msg
                               (match-string 4))
                              (lint-rule
                               (match-string 5))
                              (msg-text
                               (format "%s: %s [%s]" type msg lint-rule))
                              (type-symbol
                               (if
                                   (equal type '"warning")
                                   (let nil :warning)
                                 (let nil :error)))
                              (src-pos
                               (flymake-diag-region source-buffer row column)))
                         (flymake-make-diagnostic source-buffer
                                                  (car src-pos)
                                                  (min
                                                   (buffer-size source-buffer)
                                                   (cdr src-pos))
                                                  type-symbol msg-text
                                                  (list :rule-name lint-rule)))
                       current))))
          (forward-line 1))
        (nreverse current)))))

(defun flymake-eslint--create-process (source-buffer callback)
  "Create linter process for SOURCE-BUFFER.
CALLBACK is invoked once linter has finished the execution.
CALLBACK accepts a buffer containing stdout from linter as its
argument."
  (when (process-live-p flymake-eslint--process)
    (kill-process flymake-eslint--process))
  (let* ((default-directory (or flymake-eslint-project-root default-directory))
         (program (flymake-eslint-find-eslint)))
    (setq flymake-eslint--process
          (make-process
           :name "flymake-eslint"
           :noquery t
           :connection-type 'pipe
           :buffer (generate-new-buffer " *flymake-eslint*")
           :command `(,program
                      "--no-color" "--no-ignore" "--stdin" "--stdin-filename"
                      ,(buffer-file-name source-buffer)
                      ,@(if (listp flymake-eslint-executable-args)
                            flymake-eslint-executable-args
                          (list flymake-eslint-executable-args)))
           :sentinel
           (lambda (proc &rest _ignored)
             (when (and (eq 'exit (process-status proc))
                        (buffer-live-p source-buffer)
                        (eq proc (buffer-local-value 'flymake-eslint--process
                                                     source-buffer)))
               (let ((proc-buffer (process-buffer proc)))
                 (funcall callback proc-buffer)
                 (kill-buffer proc-buffer))))))))

(defun flymake-eslint--check-and-report (source-buffer report-fn)
  "Run eslint against SOURCE-BUFFER.
Use REPORT-FN to report results."
  (flymake-eslint--create-process
   source-buffer
   (lambda (eslint-stdout)
     (funcall report-fn (flymake-eslint--report eslint-stdout source-buffer))))
  (with-current-buffer source-buffer
    (process-send-string flymake-eslint--process (buffer-string))
    (process-send-eof flymake-eslint--process)))

(defun flymake-eslint--checker (report-fn &rest _ignored)
  "Run eslint on the current buffer.
Report results using REPORT-FN.  All other parameters are
currently ignored."
  (flymake-eslint--check-and-report (current-buffer) report-fn))

;;;###autoload
(defun flymake-eslint-enable ()
  "Enable Flymake and flymake-eslint.
Add this function to some js major mode hook."
  (interactive)
  (when (and buffer-file-name
             (flymake-eslint-find-eslint))
    (make-local-variable 'flymake-eslint-project-root)
    (add-hook 'flymake-diagnostic-functions 'flymake-eslint--checker nil t)))


;;;; Footer

(provide 'flymake-eslint)

;;; flymake-eslint.el ends here
