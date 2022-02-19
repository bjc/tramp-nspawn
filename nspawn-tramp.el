;;; nspawn-tramp.el -- TRAMP integration for systemd-nspawn containers  -*- lexical-binding: t; -*-

;; Copyright © 2021-2022 Free Software Foundation, Inc.

;; Author: Brian Cully <bjc@kublai.com>
;; Maintainer: Brian Cully <bjc@kublai.com>
;; URL: https://github.com/bjc/nspawn-tramp
;; Keywords: tramp, nspawn, machinectl, systemd, systemd-nspawn
;; Maintainer: Brian Cully <bjc@kublai.com>
;; Version: 1.0
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
;; ‘nspawn-tramp’ allows TRAMP to work with containers provided by
;; systemd-nspawn.
;;
;; ## Usage
;;
;; Call ‘nspawn-tramp-setup’ in your Emacs initialization.
;;
;;     (add-hook 'after-init-hook 'nspawn-tramp-setup)
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
;; One way of working around this using TRAMP’s built-in multi-hop
;; facilities with doas or sudo to raise your privileges.
;;
;; Another possibility is using polkit(8) to allow specific users
;; access to the requisite services (machinectl).
;;

;;; Code:

(require 'tramp)

(defgroup nspawn-tramp nil
  "TRAMP integration for systemd-nspawn containers."
  :prefix "nspawn-tramp-"
  :group 'applications
  :link '(url-link :tag "Github" "https://github.com/bjc/nspawn-tramp")
  :link '(emacs-commentary-link :tag "Commentary" "nspawn-tramp"))

(defcustom nspawn-tramp-machinectl-file-name "machinectl"
  "File name of machinectl executable."
  :type 'string
  :group 'nspawn-tramp)

(defconst nspawn-tramp-method "nspawn"
  "TRAMP method name to use to connect to systemd-nspawn containers.")

(defun nspawn-tramp--completion-function (&rest _args)
  "List systemd-nspawn containers available for connection.

This function is used by ‘tramp-set-completion-function’, please
see its function help for a description of the format."
  (let* ((raw-list (shell-command-to-string
                    (concat nspawn-tramp-machinectl-file-name
                            " list -q")))
         (lines (cdr (split-string raw-list "\n")))
         (first-words (mapcar (lambda (line) (car (split-string line)))
                              lines))
         (machines (seq-take-while (lambda (name) name) first-words)))
    (mapcar (lambda (m) (list nil m)) machines)))


(defun nspawn-tramp--add-method ()
  "Add TRAMP method handler for nspawn containers."
  (push `(,nspawn-tramp-method
          (tramp-login-program ,nspawn-tramp-machinectl-file-name)
          (tramp-login-args (("shell")
                             ("-q")
                             ("--uid" "%u")
                             ("%h")))
          (tramp-remote-shell "/bin/sh")
          (tramp-remote-shell-login ("-l"))
          (tramp-remote-shell-args ("-i" "-c")))
        tramp-methods))

(defun nspawn-tramp--remove-method ()
  "Remove TRAMP method handler for nspawn containers."
  (setf (alist-get nspawn-tramp-method tramp-methods nil t 'string=) nil))

(defun nspawn-tramp-unload-function ()
  "Remove TRAMP method handler and completion functions."
  (tramp-set-completion-function nspawn-tramp-method nil)
  (nspawn-tramp--remove-method)
  nil)

;;;###autoload
(defun nspawn-tramp-setup ()
  "Initialize systemd-nspawn support for TRAMP."
  (nspawn-tramp--add-method)
  (tramp-set-completion-function nspawn-tramp-method
                                 '((nspawn-tramp--completion-function ""))))

(provide 'nspawn-tramp)
;;; nspawn-tramp.el ends here
