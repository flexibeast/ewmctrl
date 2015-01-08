;;; ewmctrl.el --- use `wmctrl' to manage desktop windows.

;; Copyright (C) 2014  Alexis <flexibeast@gmail.com>

;; Author: Alexis <flexibeast@gmail.com>
;; Maintainer: Alexis <flexibeast@gmail.com>
;; Created: 2015-01-08
;; Keywords: desktop, windows

;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;; Commentary:

;; `ewmctrl' provides an Emacs interface to the `wmctrl' command-line window-management program.

;; ## Table of Contents

;; - [Installation](#installation)
;; - [Usage](#usage)
;; - [Issues](#issues)
;; - [License](#license)

;; ## Installation

;; Put `ewmctrl.el' in your load-path and do a `(require 'ewmctrl)'.

;; ## Usage

;; Create an `ewmctrl' buffer with `M-x ewmctrl'.

;; The default keybindings, applicable to the desktop window selected by the line at point, are:

;; * RET - Switch to the selected desktop window (`ewmctrl-focus-window').

;; * D - Delete the selected desktop window (`ewmctrl-delete-window').

;; * g - Refresh the list of desktop windows (`ewmctrl-refresh').

;; * i - Change the icon name of the selected desktop window (`ewmctrl-change-window-icon-name').

;; * n - Change the name of the selected desktop window (`ewmctrl-change-window-name').

;; * Sn - Sort the list of desktop windows lexicographically by name (`ewmctrl-sort-by-name').

;; * SN - Sort the list of desktop windows reverse-lexicographically by name (`ewmctrl-sort-by-name-reversed').

;; * Sp - Sort the list of desktop windows numerically by PID (`ewmctrl-sort-by-pid').

;; * SP - Sort the list of desktop windows reverse-numercially by PID (`ewmctrl-sort-by-pid-reversed').

;; Customisation options are available via the `ewmctrl' customize-group.

;; ## Issues / bugs

;; Deletion of windows does not work in i3 4.8 and earlier due to [i3 bug #1396](http://bugs.i3wm.org/query/ticket/1396).

;; If you discover an issue or bug in `ewmctrl' not already noted:

;; * as a TODO item, or

;; * in [the project's "Issues" section on GitHub](https://github.com/flexibeast/ewmctrl/issues),

;; please create a new issue with as much detail as possible, including:

;; * which version of Emacs you're running on which operating system, and

;; * how you installed `ewmctrl'.

;; ## License

;; [GNU General Public License version 3](http://www.gnu.org/licenses/gpl.html), or (at your option) any later version.

;;; Code:


(defgroup ewmctrl nil
  "Emacs interface to `wmctrl'."
  :group 'external)

(defcustom ewmctrl-wmctrl-path "/usr/bin/wmctrl"
  "Absolute path of `wmctrl' executable."
  :type '(file :must-match t)
  :group 'ewmctrl)

(defcustom ewmctrl-wmctrl-switches "-lpG"
  "Switches to pass to `wmctrl' executable."
  :type 'string
  :group 'ewmctrl)

(defcustom ewmctrl-sort-field 'name
  "Field on which to sort the list of desktop windows."
  :type '(list name name-reversed pid pid-reversed)
  :group 'ewmctrl)


(defun ewmctrl-list-windows ()
  "Use `wmctrl' to get a list of desktop windows."
  (let ((bfr (generate-new-buffer " *ewmctrl-output*"))
        (windows-list '()))
    (call-process-shell-command (concat ewmctrl-wmctrl-path " " ewmctrl-wmctrl-switches) nil bfr)
    (with-current-buffer bfr
      (goto-char (point-min))
      (while (re-search-forward "^\\(\\S-+\\)\\s-+\\(\\S-+\\)\\s-+\\(\\S-+\\)\\s-+\\(\\S-+\\)\\s-+\\(\\S-+\\)\\s-+\\(\\S-+\\)\\s-+\\(\\S-+\\)\\s-+\\(\\S-+\\)\\s-+\\(.+\\)" nil t)
        (setq windows-list
              (append windows-list
                      (list
                       `((window-id . ,(match-string 1))
                         (desktop-number . ,(match-string 2))
                         (pid . ,(match-string 3))
                         (x-offset . ,(match-string 4))
                         (y-offset . ,(match-string 5))
                         (width . ,(match-string 6))
                         (height . ,(match-string 7))
                         (client-host . ,(match-string 8))
                         (title . ,(match-string 9))))))))
    (kill-buffer bfr)
    (cond
     ((eq 'name ewmctrl-sort-field)
      (sort windows-list #'(lambda (e1 e2)
                             (string<
                              (downcase (cdr (assoc 'title e1)))
                              (downcase (cdr (assoc 'title e2)))))))
     ((eq 'name-reversed ewmctrl-sort-field)
      (sort windows-list #'(lambda (e1 e2)
                             (string<
                              (downcase (cdr (assoc 'title e2)))
                              (downcase (cdr (assoc 'title e1)))))))
     ((eq 'pid ewmctrl-sort-field)
      (sort windows-list #'(lambda (e1 e2)
                             (string<
                              (cdr (assoc 'pid e1))
                              (cdr (assoc 'pid e2))))))
     ((eq 'pid-reversed ewmctrl-sort-field)
      (sort windows-list #'(lambda (e1 e2)
                             (string<
                              (cdr (assoc 'pid e2))
                              (cdr (assoc 'pid e1))))))
     (t
      windows-list))))

(defun ewmctrl-sort-by-name ()
  "Sort list of desktop windows lexicographically on the name field."
  (interactive)
  (setq ewmctrl-sort-field 'name)
  (ewmctrl-refresh))

(defun ewmctrl-sort-by-name-reversed ()
  "Sort list of desktop windows reverse-lexicographically on the
name field."
  (interactive)
  (setq ewmctrl-sort-field 'name-reversed)
  (ewmctrl-refresh))

(defun ewmctrl-sort-by-pid ()
  "Sort list of desktop windows numerically on the PID field."
  (interactive)
  (setq ewmctrl-sort-field 'pid)
  (ewmctrl-refresh))

(defun ewmctrl-sort-by-pid-reversed ()
  "Sort list of desktop windows reverse-numerically on the
PID field."
  (interactive)
  (setq ewmctrl-sort-field 'pid-reversed)
  (ewmctrl-refresh))

(defun ewmctrl-focus-window ()
  "Give focus to desktop window whose title matches the line at point."
  (interactive)
  (let ((id (get-text-property (point) 'window-id)))
    (call-process-shell-command (concat ewmctrl-wmctrl-path " -i -a '" id "'"))))

(defun ewmctrl-delete-window ()
  "Delete desktop window specified at point."
  (interactive)
  (let ((id (get-text-property (point) 'window-id)))
    (if (yes-or-no-p (concat "Delete window '" (get-text-property (point) 'title) "'? "))
        (progn
          (call-process-shell-command (concat ewmctrl-wmctrl-path " -i -c '" id "'"))
          (ewmctrl-refresh)))))

(defun ewmctrl-change-window-name (name)
  "Change name of desktop window specified at point."
  (interactive "sNew window name: ")
  (let ((id (get-text-property (point) 'window-id)))
    (call-process-shell-command (concat ewmctrl-wmctrl-path " -i -r '" id "' -N '" name "'"))
    (ewmctrl-refresh)))

(defun ewmctrl-change-window-icon-name (name)
  "Change icon name of desktop window specified at point."
  (interactive "sNew window icon name: ")
  (let ((id (get-text-property (point) 'window-id)))
    (call-process-shell-command (concat ewmctrl-wmctrl-path " -i -r '" id "' -I '" name "'"))
    (ewmctrl-refresh)))

(defun ewmctrl-refresh ()
  "Refresh the contents of the *ewmctrl* buffer."
  (interactive)
  (with-current-buffer "*ewmctrl*"
    (let ((inhibit-read-only t)
          (window-list (ewmctrl-list-windows)))
      (erase-buffer)
      (insert (propertize "    PID  Name\n" 'face '(foreground-color . "ForestGreen")))
      (insert (propertize "  -----  ----\n" 'face '(foreground-color . "ForestGreen")))
      (dolist (win window-list)
        (insert (propertize (concat "  " (format "%5s" (cdr (assoc 'pid win))) "  " (cdr (assoc 'title win)) "\n")
                            'window-id (cdr (assoc 'window-id win))
                            'title (cdr (assoc 'title win))))))))


(define-derived-mode ewmctrl-mode special-mode "ewmctrl"
  "Major mode for managing desktop windows via `wmctrl'."
  (read-only-mode)
  (define-key ewmctrl-mode-map (kbd "RET") 'ewmctrl-focus-window)
  (define-key ewmctrl-mode-map (kbd "D") 'ewmctrl-delete-window)
  (define-key ewmctrl-mode-map (kbd "g") 'ewmctrl-refresh)
  (define-key ewmctrl-mode-map (kbd "i") 'ewmctrl-change-window-icon-name)
  (define-key ewmctrl-mode-map (kbd "n") 'ewmctrl-change-window-name)
  (define-key ewmctrl-mode-map (kbd "Sn") 'ewmctrl-sort-by-name)
  (define-key ewmctrl-mode-map (kbd "SN") 'ewmctrl-sort-by-name-reversed)
  (define-key ewmctrl-mode-map (kbd "Sp") 'ewmctrl-sort-by-pid)
  (define-key ewmctrl-mode-map (kbd "SP") 'ewmctrl-sort-by-pid-reversed))

;;;###autoload
(defun ewmctrl ()
  "Create and populate a new *ewmctrl* buffer."
  (interactive)
  (if (not (file-exists-p ewmctrl-wmctrl-path))
      (error "No `wmctrl' executable found at `ewmctrl-wmctrl-path'"))
  (let ((bfr (get-buffer-create "*ewmctrl*")))
    (ewmctrl-refresh)
    (switch-to-buffer bfr)
    (ewmctrl-mode)))


;; --

(provide 'ewmctrl)

;;; ewmctrl.el ends here
