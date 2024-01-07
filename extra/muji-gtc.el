;;; muji-gtc.el --- Google Transliterate Conversion for muji  -*- lexical-binding: t; -*-

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

;;; Setup:

;; (require 'muji)
;; (require 'muji-gtc)
;; (setq muji-gtc-enable-gtc-p t)
;; (define-key global-map (kbd "C-x C-\\") 'global-muji-mode)

;;; Code:

(require 'url-http)
(require 'json)
(require 'kkc)

(require 'muji)

;; custom var

(defcustom muji-gtc-enable-gtc-p nil
  "非 nil のとき Google Transliterate で変換を行う."
  :group 'muji
  :type 'boolean)

;; main

(defun muji-gtc-google-transliterate (string)
  "ひらがな文字列 STRING を Google Transliterate して結果をリストにして返す.

- Google 日本語入力 - CGI API デベロッパーガイド
- https://www.google.co.jp/ime/cgiapi.html"
  (let* ((coding-system 'utf-8)
         (encoded (url-hexify-string (encode-coding-string string 'utf-8)))
         (url "http://www.google.com/transliterate?langpair=ja-Hira|ja&text=")
         (url (concat url encoded))
         (url-request-method "GET")
         (url-max-redirections 0)
         (buf (url-retrieve-synchronously url))
         (p (url-http-symbol-value-in-buffer 'url-http-end-of-headers buf))
         (json (unwind-protect
                   (when p
                     (with-current-buffer buf
                       (decode-coding-string
                        (buffer-substring (1+ p) (point-max)) coding-system)))
                 (when buf (kill-buffer buf))))
         (json (if json json "[[\"\", [\"\"]]]")) ; XXX
         (vector (json-read-from-string json)))
    ;; 関数 append はベクトルを同じ要素から成るリストへ変換する便利な方法
    (append (cl-map 'vector (lambda (v) (append v nil))
                    (cl-map 'vector (lambda (v) (aref v 1)) vector))
            nil)))

;; replacement for skkdic-lookup-key in kkc-lookup-key

(defun muji-gtc-lookup-key (key len &optional postfix prefer-noun)
  "かなの配列 KEY の長さ LEN 分の変換候補のリストを返す.
POSTFIX と PREFER-NOUN は無視される."
  (let* ((string (concat key))
         (string (substring string 0 len)))
    (car (muji-gtc-google-transliterate (concat string ",")))))

;; kkc-lookup-key from kkc.el.gz

;; Lookup Japanese dictionary to set list of conversions in
;; kkc-current-conversions for key sequence kkc-current-key of length
;; LEN.  If no conversion is found in the dictionary, don't change
;; kkc-current-conversions and return nil.
;; Postfixes are handled only if POSTFIX is non-nil.
(defun muji-gtc-kkc-lookup-key (len &optional postfix prefer-noun)
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
      (setq entry (muji-gtc-lookup-key kkc-current-key len postfix prefer-noun))
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

;; kkc-next from kkc.el.gz

(defun muji-gtc-kkc-next ()
  "Select the next candidate of conversion."
  (interactive)
  (let ((idx (1+ (car kkc-current-conversions))))
    (if (< idx 0)
	(setq idx 1))
    (if (>= idx (length kkc-current-conversions))
	(setq idx 0))
    (setcar kkc-current-conversions idx)
    ;; <MODIFIED>
    ;; (if (> idx 1)
    ;;     (progn
    ;;       (set-nested-alist kkc-current-key kkc-current-conversions
    ;;     		    kkc-lookup-cache kkc-length-converted)
    ;;       (setq kkc-init-file-flag 'kkc-lookup-cache)))
    ;; </MODIFIED>
    (if (or kkc-current-conversions-width
	    (>= kkc-next-count kkc-show-conversion-list-count))
	(kkc-show-conversion-list-update))
    (kkc-update-conversion)))

;; kkc-prev from kkc.el.gz

(defun muji-gtc-kkc-prev ()
  "Select the previous candidate of conversion."
  (interactive)
  (let ((idx (1- (car kkc-current-conversions))))
    (if (< idx 0)
	(setq idx (1- (length kkc-current-conversions))))
    (setcar kkc-current-conversions idx)
    ;; <MODIFIED>
    ;; (if (> idx 1)
    ;;     (progn
    ;;       (set-nested-alist kkc-current-key kkc-current-conversions
    ;;     		    kkc-lookup-cache kkc-length-converted)
    ;;       (setq kkc-init-file-flag 'kkc-lookup-cache)))
    ;; </MODIFIED>
    (if (or kkc-current-conversions-width
	    (>= kkc-prev-count kkc-show-conversion-list-count))
	(kkc-show-conversion-list-update))
    (kkc-update-conversion)))

;; advice

(defun muji-gtc-pre-kkc (&optional arg)
  (when muji-gtc-enable-gtc-p
    (advice-add 'kkc-lookup-key :override #'muji-gtc-kkc-lookup-key)
    (advice-add 'kkc-next :override #'muji-gtc-kkc-next)
    (advice-add 'kkc-prev :override #'muji-gtc-kkc-prev)))

(defun muji-gtc-post-kkc (&optional arg)
  (when muji-gtc-enable-gtc-p
    (advice-remove 'kkc-lookup-key #'muji-gtc-kkc-lookup-key)
    (advice-remove 'kkc-next #'muji-gtc-kkc-next)
    (advice-remove 'kkc-prev #'muji-gtc-kkc-prev)))

(advice-add 'muji-kkc :before #'muji-gtc-pre-kkc)

(advice-add 'muji-kkc :after #'muji-gtc-post-kkc)

;;; provide

(provide 'muji-gtc)
;;; muji-gtc.el ends here
