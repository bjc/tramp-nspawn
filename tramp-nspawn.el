;;; tramp-nspawn.el --- Tramp integration for systemd-nspawn containers  -*- lexical-binding: t; -*-

;; Copyright © 2021-2022 Free Software Foundation, Inc.

;; Author: Brian Cully <bjc@kublai.com>
;; Maintainer: Brian Cully <bjc@kublai.com>
;; URL: https://github.com/bjc/tramp-nspawn
;; Keywords: tramp, nspawn, machinectl, systemd, systemd-nspawn
;; Version: 1.0.1
;; Package-Requires: ((emacs "23"))

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;
;; ‘tramp-nspawn’ allows Tramp access to environments provided by
;; systemd-nspawn.
;;
;; ## Usage
;;
;; Call ‘tramp-nspawn-setup’ in your Emacs initialization.
;;
;;     (add-hook 'after-init-hook 'tramp-nspawn-setup)
;;
;; Open a file on a running systemd-nspawn container:
;;
;;     C-x C-f /nspawn:USER@CONTAINER:/path/to/file
;;
;; Where:
;;     USER          is the user on the container to connect as (optional)
;;     CONTAINER     is the container to connect to
;;
;; ## Privileges
;;
;; systemd-nspawn and its container utilities often require super user
;; access to run, and this package does not escalate privileges in
;; order to accomplish that.
;;
;; One way of working around this using Tramp’s built-in multi-hop
;; facilities with doas or sudo to raise your privileges.
;;
;; Another possibility is using polkit(8) to allow specific users
;; access to the requisite services (machinectl).
;;

;;; Code:

(require 'tramp)

(defgroup tramp-nspawn nil
  "Tramp integration for systemd-nspawn containers."
  :prefix "tramp-nspawn-"
  :group 'applications
  :link '(url-link :tag "repo" "https://git.spork.org/tramp-nspawn.git")
  :link '(emacs-commentary-link :tag "Commentary" "tramp-nspawn"))

(defcustom tramp-nspawn-machinectl-program "machinectl"
  "Name of the machinectl program."
  :type 'string
  :group 'tramp-nspawn)

(defconst tramp-nspawn-method "nspawn"
  "Tramp method name to use to connect to systemd-nspawn containers.")

(defun tramp-nspawn--completion-function (&rest _args)
  "List systemd-nspawn containers available for connection.

This function is used by ‘tramp-set-completion-function’, please
see its function help for a description of the format."
  (let* ((raw-list (shell-command-to-string
                    (concat tramp-nspawn-machinectl-program
                            " list -q")))
         (lines (cdr (split-string raw-list "\n")))
         (first-words (mapcar (lambda (line) (car (split-string line)))
                              lines))
         (machines (seq-take-while (lambda (name) name) first-words)))
    (mapcar (lambda (m) (list nil m)) machines)))


;; todo: check tramp-async-args and tramp-direct-async
(defun tramp-nspawn--add-method ()
  "Add Tramp method handler for nspawn containers."
  (push `(,tramp-nspawn-method
          (tramp-login-program ,tramp-nspawn-machinectl-program)
          (tramp-login-args (("shell")
                             ("-q")
                             ("--uid" "%u")
                             ("%h")))
          (tramp-remote-shell "/bin/sh")
          (tramp-remote-shell-login ("-l"))
          (tramp-remote-shell-args ("-i" "-c")))
        tramp-methods))

(defun tramp-nspawn--remove-method ()
  "Remove Tramp method handler for nspawn containers."
  (setf (alist-get tramp-nspawn-method tramp-methods nil t 'string=) nil))

(defun tramp-nspawn-unload-function ()
  "Remove Tramp method handler and completion functions."
  (tramp-set-completion-function tramp-nspawn-method nil)
  (tramp-nspawn--remove-method)
  nil)

;;;###autoload
(defun tramp-nspawn-setup ()
  "Initialize systemd-nspawn support for Tramp."
  (tramp-nspawn--add-method)
  (tramp-set-completion-function tramp-nspawn-method
                                 '((tramp-nspawn--completion-function ""))))

(provide 'tramp-nspawn)
;;; tramp-nspawn.el ends here
