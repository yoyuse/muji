;;; muji-setting.el --- toggle muji setting temporally  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  YUSE Yosihiro

;; Author: YUSE Yosihiro <yoyuse@gmail.com>
;; Keywords: modeless japanese input

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; C-u C-u C-j calls `muji-setting-temporally'.

;;; Code:

(require 'muji)

(defvar muji-setting-history nil)

(defun muji-setting-temporally ()
  "Toggle muji setting temporally."
  (interactive)
  (let* ((alist '(("muji-auto-nfer")
                  ("muji-comma-period")
                  ("muji-remove-space")
                  ("muji-use-double-n")
                  ("kkc-gtc-enable-gtc-p")
                  ("kkc-gtc-maze-enable-gtc-maze-p")
                  ("kkc-maze-enable-maze-p")))
         (setting (completing-read "Toggle muji setting: " alist
                                   nil t nil 'muji-setting-history
                                   (car muji-setting-history))))
    (let ((symbol (intern-soft setting)))
      (when (not symbol) (error "Invalid setting: %s" setting))
      (set symbol (not (symbol-value symbol)))
      (message "`%s' is now `%s'" setting (symbol-value symbol)))))

(defun muji-kkc-ad (orig-fn &optional arg)
  "\\[universal-argument] \\[universal-argument] \\[muji-kkc] calls `muji-setting-temporally'."
  (if (equal '(16) arg)
      (muji-setting-temporally)
    (apply orig-fn (list arg))))

(advice-add 'muji-kkc :around #'muji-kkc-ad)

;;; provide

(provide 'muji-setting)
;;; muji-setting.el ends here
