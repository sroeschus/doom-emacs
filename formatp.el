;;; formatp.el --- Create patches from git commits

;;; Copyright C) 2021 Stefan Roesch

;;; Author: Stefan Roesch <shr@fb.com>
;;; Version: 0.2
;;; Keywords: kernel, git, patch

;;; Commentary:

;;; formatp.el creates patch files from one or more git
;;; commits.

;;; New untested code:
;;;
;;;
;;; (defun call-checkpatch (fname)
;;;   "Run PROGRAM with ARGS and return the exit code and output in a list."
;;;   (with-temp-buffer
;;;     (list (call-process "~/linux.upstream/scripts/checkpatch.pl" nil (current-buffer) nil fname)
;;;           (buffer-string))))
;;;
;;; (setq check-output (call-checkpatch "0001-io_uring-call-stack-dump-on-sleep.patch"))
;;;
;;; (file-expand-wildcards "/home/shr/patch-btrfs-nowait.v7/*.patch")
;;; (dolist (x (file-expand-wildcards "/home/shr/patch-btrfs-nowait.v7/*.patch"))
;;;         (message "Checkpath for %s" (string-trim x))
;;;         (setq check-output (call-checkpatch (string-trim x)))
;;;         (if (= 0 (car check-output))
;;;             (message "OK")
;;;           (message "ERROR# %s" (car check-output))
;;;           (message "%s" (cdr check-output))
;;;         ))
;;; Code:

(require 'transient)

(defun format-patch-prepare-buffer (buffer)
  "Switch to and prepare buffer for running."
  (switch-to-buffer buffer)
  (goto-char (point-min))
  (erase-buffer))

(defun call-checkpatch (dir fname)
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (with-temp-buffer
    (list (call-process (format "%sscripts/checkpatch.pl" dir) nil (current-buffer) nil fname)
          (buffer-string))))

(defun format-patch-cmd (&optional args)
  (interactive
   (list (transient-args 'transient-format-patch)))
  (setq arg-str "")
  (setq num-patches nil)
  (setq output-dir nil)
  (setq version nil)
  (setq dirname nil)
  (while args
    (if (string-match "-c=\\([0-9]+\\)" (car args))
        (setq num-patches (match-string 1 (car args))))
    ;; (if (string-match "-o \\([a-zA-Z0-9_-~//]\\)+" (car args))
    (if (string-match "-d=\\([^ ]+\\)" (car args))
        (setq output-dir (match-string 1 (car args))))
    (if (string-match "-v=\\([0-9]+\\)" (car args))
        (setq version (match-string 1 (car args))))
    (if (string-match "--signoff" (car args))
        (setq arg-str (concat arg-str (car args))))
    (if (string-match "--rfc" (car args))
        (setq arg-str (concat arg-str (car args))))
    (if (string-match "--cover-letter" (car args))
        (setq arg-str (concat arg-str " " (car args))))
    (setq args (cdr args)))

  ;; (setq dirname (substring output-dir 3))
  (setq dirname output-dir)
  (format-patch-prepare-buffer "format patch")
  (make-directory dirname t)
  (insert (format "Created directory: %s.\n" dirname))

  (insert "Formatting patches.\n")
  (setq cmd (format "git format-patch --base=auto -v %s -%s %s -o %s HEAD" version num-patches arg-str (or dirname "")))
  (message "%s\n" cmd)
  ;;(setq cmd (format "git format-patch -v %s -%s %s -o %s HEAD" version num-patches arg-str (or dirname "")))
  (setq rc (shell-command-to-string cmd))
  (insert (format "\nFormatted patch:\n\n%s.\n" rc))

  (insert "\nRunning check patch:\n\n")
  (dolist (x (file-expand-wildcards (format "%s/*.patch" dirname)))
    (insert (format "Checkpath for %-100s" x))
    (setq check-output (call-checkpatch default-directory x))
    (if (= 0 (car check-output))
        (insert "   [OK]\n")
      (progn
        (insert (format "   [ERROR# %s]\n" (car check-output)))
        (insert "%s" (cdr check-output)))
      )
    (current-buffer))

  (if (y-or-n-p "Load cover letter?")
    (progn
      (setq cover-file (file-expand-wildcards (format "%s/*cover-letter.patch" dirname)))
      (find-file (car cover-file)))))

(transient-define-prefix transient-format-patch ()
  "Format Patch"

  ;; :value '("-v 1" "-n 1")

  ;; ["Arguments"
  ;;   ("-n" "# patches"        "-n " :class transient-option)
  ;;   ("-o" "output directory" "-o " :class transient-option)
  ;;   ("-v" "version"          "-v " :class transient-option)
  ;;   ("-s" "signoff"          "--signoff")
  ;;   ("-r" "rfc"              "--rfc")
  ;;   ("-c" "cover-letter"     "--cover-letter")]
  ;; ["Commands"
  ;;  ("w" "write" format-patch-cmd)])
  ["Arguments"
    ("-c" "# patches"        "-c=")
    ("-d" "output directory" "-d=")
    ("-v" "version"          "-v=")
    ("-s" "signoff"          "--signoff")
    ("-r" "rfc"              "--rfc")
    ("-l" "cover-letter"     "--cover-letter")]
  ["Commands"
   ("w" "write" format-patch-cmd)])

;;; ###autoload
(defun formatp-menu ()
  (interactive)
  (transient-format-patch))

(provide 'formatp)
;;; formatp.el ends here
