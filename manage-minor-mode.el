;;; manage-minor-mode.el --- Manage your minor-modes easily -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2014 by Shingo Fukuyama

;; Version: 1.0
;; Author: Shingo Fukuyama - http://fukuyama.co
;; URL: https://github.com/ShingoFukuyama/manage-minor-mode
;; Created: Mar 8 2014
;; Keywords: minor-mode manage emacs
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;;; Commentary:

;; Usage:
;; 1. M-x manage-minor-mode
;; 2. Minor modes list buffer will appear
;; 3. Then type "RET" key on any minor-mode you want to toggle active/inactive

;;; Code:

(require 'cl-lib)

(defun manage-minor-mode--active-list ()
  "Get a list of which minor modes are enabled in the current buffer."
  (let ($list)
    (mapc (lambda ($mode)
            (condition-case nil
                (if (and (symbolp $mode) (symbol-value $mode))
                    (setq $list (cons $mode $list)))
              (error nil)))
          minor-mode-list)
    (sort $list 'string<)))

(defun manage-minor-mode--inactive-list ()
  "Get a list of which minor modes are disabled in the current buffer."
  (let ($list)
    (mapc (lambda ($mode)
            (condition-case nil
                (if (and (symbolp $mode) (not (symbol-value $mode)))
                    (setq $list (cons $mode $list)))
              (error nil)))
          minor-mode-list)
    (sort $list 'string<)))

(defgroup manage-minor-mode nil
  "Group for manage minor modes"
  :prefix "manage-minor-mode-"
  :group 'convenience)

(defvar manage-minor-mode-buffer "*manage-minor-mode*")
(defvar manage-minor-mode-target-buffer nil)

(defface manage-minor-mode-face-active
  '((t :inherit success :foreground "#33eeeee"))
  "Face for active minor modes"
  :group 'manage-minor-mode)

(defface manage-minor-mode-face-inactive
  '((t :inherit warning :foreground "#ff6666"))
  "Face for inactive minor modes"
  :group 'manage-minor-mode)

(defface manage-minor-mode-face-changed
  '((t :inherit underline :foreground "#eeee00"))
  "Face for last changed minor modes"
  :group 'manage-minor-mode)

(defvar manage-minor-mode-map
  (let (($map (make-sparse-keymap)))
    (define-key $map (kbd "RET") 'manage-minor-mode--toggle)
    (define-key $map (kbd "<mouse-1>") 'manage-minor-mode--toggle)
    $map))

(defun manage-minor-mode--goto-line ($line)
  (goto-char (point-min))
  (forward-line (- $line 1)))

(defsubst manage-minor-mode--enable ($mode)
  (funcall $mode 1))

(defsubst manage-minor-mode--disable ($mode)
  (funcall $mode 0))

(defun manage-minor-mode--toggle ()
  "Toggle a minor-mode status under the cursor"
  (interactive)
  (let (($po (if (eolp) (- (point) 1) (point)))
        ($li (line-number-at-pos)))
    (cl-case (get-text-property $po 'manage-minor-mode)
      (active   (let (($mode (intern (thing-at-point 'symbol))))
                  (with-current-buffer manage-minor-mode-target-buffer
                    (manage-minor-mode--disable $mode)
                    (manage-minor-mode $mode)
                    (manage-minor-mode--goto-line $li))))
      (inactive (let (($mode (intern (thing-at-point 'symbol))))
                  (with-current-buffer manage-minor-mode-target-buffer
                    (manage-minor-mode--enable $mode)
                    (manage-minor-mode $mode)
                    (manage-minor-mode--goto-line $li)
                    (goto-char (point-at-eol))
                    (goto-char (previous-single-property-change (point) 'manage-minor-mode))))))))

(defun manage-minor-mode (&optional $last-toggled-item)
  (interactive)
  (let* (($act   (manage-minor-mode--active-list))
         ($inact (manage-minor-mode--inactive-list))
         ($max-line (+ (length $act) (length $inact)))
         ($current-buf (current-buffer))
         ($current-win (get-buffer-window $current-buf)))
    (when (get-buffer manage-minor-mode-buffer)
      (kill-buffer manage-minor-mode-buffer)
      (select-window $current-win))
    (pop-to-buffer manage-minor-mode-buffer)
    (set (make-local-variable 'manage-minor-mode-target-buffer) $current-buf)
    (cl-dotimes (ignored $max-line)
      (insert (format "|  \n")))
    ;; Insert inactive minor-modes
    (goto-char (point-min))
    (mapc (lambda ($m)
            (goto-char (point-at-eol))
            (insert
             (propertize
              (format "%s" $m)
              'face 'manage-minor-mode-face-inactive
              'manage-minor-mode 'inactive
              'pointer           'hand))
            (forward-line))
          $inact)
    ;; Insert active minor-modes
    (goto-char (point-min))
    (mapc (lambda ($m)
            (insert
             (propertize
              (format "%s  " $m)
              'face 'manage-minor-mode-face-active
              'manage-minor-mode 'active
              'pointer           'hand))
            (forward-line))
          $act)
    ;; header
    (goto-char (point-min))
    (insert (concat "Active" "  |  " "Inactive" "\n"))
    (when $last-toggled-item
      (goto-char (point-min))
      (when (re-search-forward (concat "\\_<"
                                       (format "%s" $last-toggled-item)
                                       "\\_>") nil t)
        (put-text-property (match-beginning 0) (match-end 0)
                           'face 'manage-minor-mode-face-changed)))
    ;; Adjust buffer
    (goto-char (point-min))
    (while (re-search-forward "^\\s-*|\\s-*$" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (align-regexp (point-min) (point-max) "\\(\\s-*\\)|")
    (goto-char (point-min))
    (read-only-mode)
    (use-local-map manage-minor-mode-map)))

(defvar manage-minor-mode-modes-before-bals nil)

(defun manage-minor-mode-bals ()
  "Eradicate all minor-modes in the current buffer.
This command may cause unexpected effect even to other buffers.
However, don't worry, restore command exists:
 `manage-minor-mode-restore-from-bals'."
  (interactive)
  (set (make-local-variable 'manage-minor-mode-modes-before-bals)
       (manage-minor-mode--active-list))
  (mapc (lambda ($m)
          (manage-minor-mode--disable $m))
        manage-minor-mode-modes-before-bals))

(defun manage-minor-mode-restore-from-bals ()
  "Restore minor modes before `manage-minor-mode-bals' happened"
  (interactive)
  (mapc (lambda ($m)
          (manage-minor-mode--enable $m))
        manage-minor-mode-modes-before-bals))

;; Mouse click minor-mode in mode-line to popup "Manage minor modes"
(defadvice popup-menu (before manage-minor-mode-add-for-popup-menu disable)
  (ad-set-arg 0 (append (ad-get-arg 0)
                        '((manage-minor-mode
                           menu-item "Manage minor modes"
                           (lambda () (interactive) (manage-minor-mode)))))))

(defadvice minor-mode-menu-from-indicator (around manage-minor-mode-add-mode-line-menu activate)
  (ad-enable-advice 'popup-menu 'before 'manage-minor-mode-add-for-popup-menu)
  (ad-activate 'popup-menu)
  ad-do-it
  (ad-disable-advice 'popup-menu 'before 'manage-minor-mode-add-for-popup-menu)
  (ad-activate 'popup-menu))

(provide 'manage-minor-mode)
;;; manage-minor-mode.el ends here
