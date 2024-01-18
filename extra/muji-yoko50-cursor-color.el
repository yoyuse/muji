;;; muji-yoko50-cursor-color.el --- change cursor color for muji/yoko50 -*- lexical-binding: t; -*-

;; Copyright (C) 2024  YUSE Yosihiro

;; Author: YUSE Yosihiro <yoyuse@gmail.com>
;; Keywords: japanese input

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

;;; Code:

;; (require 'muji)
;; (require 'yoko50 nil t)

(defvar muji-yoko50-cursor-color-use-color t)

(defvar muji-yoko50-cursor-color-default
  (cdr (assq 'cursor-color (frame-parameters (selected-frame)))))

(defun muji-yoko50-cursor-color ()
  (cond
   ((equal current-input-method "yoko50")
    '("firebrick2" . "firebrick1"))
   ((equal current-input-method "yoko50-zenkaku")
    '("DarkGoldenrod2" . "gold"))
   ((equal current-input-method "yoko50-ascii")
    (if muji-mode
        '("SeaGreen3" . "SeaGreen2")
      '("ivory4" . "gray")))
   ((bound-and-true-p muji-mode)
    '("CornflowerBlue" . "SkyBlue1"))
   (t muji-yoko50-cursor-color-default)))

(defun muji-yoko50-cursor-color-set-color ()
  (when muji-yoko50-cursor-color-use-color
    (let* ((light-dark (frame-parameter nil 'background-mode))
           (light-mode (eq light-dark 'light))
           (color (muji-yoko50-cursor-color))
           (color (if (consp color)
                      (if light-mode (car color) (cdr color))
                    color)))
      (set-cursor-color color))))

;; (remove-hook 'post-command-hook 'muji-set-cursor-color)

(add-hook 'post-command-hook 'muji-yoko50-cursor-color-set-color)

(add-hook 'quail-yoko50-switch-package-hook 'muji-yoko50-cursor-color-set-color)

;;; provide

(provide 'muji-yoko50-cursor-color)
;;; muji-yoko50-cursor-color.el ends here
