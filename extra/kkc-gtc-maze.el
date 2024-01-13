;; kkc-gtc-maze.el --- Google Transliterate Conversion with mazegaki -*- lexical-binding: t; -*-

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

;; This package requires kkc-maze.el, which is included in yoyuse/ttt:
;; https://github.com/yoyuse/ttt

;;; Setup:

;; (require 'kkc-gtc-maze)
;; (setq kkc-gtc-maze-enable-gtc-maze-p t)

;;; Code:

(require 'kkc-gtc)
(require 'kkc-maze)

;; custom var

(defcustom kkc-gtc-maze-enable-gtc-maze-p nil
  "非 nil のとき Google Transliterate で交ぜ書き変換を行う."
  :group 'kkc-gtc-maze
  :type 'boolean)

;; main

(defun kkc-gtc-maze-lookup-key (maze len &optional postfix prefer-noun)
  "かな漢字交じり列 MAZE の長さ LEN 分の変換候補のリストを返す.
POSTFIX と PREFER-NOUN は無視される."
  (let* ((maze (concat maze))
         (ls (mapcar (lambda (cons)
                       (let ((seq (car cons))
                             (delta (cdr cons)))
                         ;; (skkdic-lookup-key seq (+ len delta)
                         ;;                    postfix prefer-noun)))
                         (kkc-gtc-lookup-key seq (+ len delta)
                                             postfix prefer-noun)))
                     ;;
                     (kkc-maze-lookup-yomi-with-delta (substring maze 0 len))))
         (ls (kkc-maze-flatten ls))
         (regexp (kkc-maze-regexp (substring maze 0 len)))
         (ls (kkc-maze-lookup-filter ls regexp))
         (ls (seq-uniq ls)))
    ls))

(defun kkc-gtc-maze-kkc-lookup-key (len &optional postfix prefer-noun)
  ;; <MODIFIED>
  ;; ;; At first, prepare cache data if any.
  ;; (unless kkc-init-file-flag
  ;;   (setq kkc-init-file-flag t
  ;;         kkc-lookup-cache nil)
  ;;   (add-hook 'kill-emacs-hook 'kkc-save-init-file)
  ;;   (if (file-readable-p kkc-init-file-name)
  ;;       (condition-case nil
  ;;           (load-file kkc-init-file-name)
  ;;         (kkc-error "Invalid data in %s" kkc-init-file-name))))
  ;; (or (and (nested-alist-p kkc-lookup-cache)
  ;;          (eq (car kkc-lookup-cache) kkc-lookup-cache-tag))
  ;;     (setq kkc-lookup-cache (list kkc-lookup-cache-tag)
  ;;           kkc-init-file-flag 'kkc-lookup-cache))
  ;; (let ((entry (lookup-nested-alist kkc-current-key kkc-lookup-cache len 0 t)))
  (let ((entry nil))
  ;; </MODIFIED>
    (if (consp (car entry))
	(setq kkc-length-converted len
	      kkc-current-conversions-width nil
	      kkc-current-conversions (car entry))
      ;; <MODIFIED>
      ;; (setq entry (skkdic-lookup-key kkc-current-key len postfix prefer-noun))
      ;; <MODIFIED>
      ;; (setq entry (kkc-gtc-lookup-key kkc-current-key len postfix prefer-noun))
      (setq entry (kkc-gtc-maze-lookup-key kkc-current-key len postfix prefer-noun))
      ;; </MODIFIED>
      ;; </MODIFIED>
      (if entry
	  (progn
	    (setq kkc-length-converted len
		  kkc-current-conversions-width nil
		  kkc-current-conversions (cons 1 entry))
	    ;; <MODIFIED>
            ;; (if postfix
	    ;;     ;; Store this conversions in the cache.
	    ;;     (progn
	    ;;       (set-nested-alist kkc-current-key kkc-current-conversions
	    ;;     		    kkc-lookup-cache kkc-length-converted)
	    ;;       (setq kkc-init-file-flag 'kkc-lookup-cache)))
            ;; </MODIFIED>
	    t)
	(if (= len 1)
	    (setq kkc-length-converted 1
		  kkc-current-conversions-width nil
		  kkc-current-conversions (cons 0 nil)))))))

;; advice

(defun kkc-gtc-maze-pre-kkc (from to)
  (when kkc-gtc-maze-enable-gtc-maze-p
    (advice-add 'kkc-gtc-kkc-lookup-key :override #'kkc-gtc-maze-kkc-lookup-key)))

(defun kkc-gtc-maze-post-kkc (from to)
  (when kkc-gtc-maze-enable-gtc-maze-p
    (advice-remove 'kkc-gtc-kkc-lookup-key #'kkc-gtc-maze-kkc-lookup-key)))

(advice-add 'kkc-region :before #'kkc-gtc-maze-pre-kkc)

(advice-add 'kkc-region :after #'kkc-gtc-maze-post-kkc)

;;; provide

(provide 'kkc-gtc-maze)
;;; kkc-gtc-maze.el ends here
