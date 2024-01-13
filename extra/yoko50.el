;;; yoko50.el --- Quail package for yoko50           -*- lexical-binding: t; -*-

;; Copyright (C) 2024  YUSE Yosihiro

;; Author: YUSE Yosihiro <yoyuse@gmail.com>
;; Keywords: multilingual, input method, Japanese

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

;; This file is based on japanese.el

;;; Setup:

;; (require 'yoko50)
;; (setq default-input-method "yoko50")

;;; Code:

(require 'quail)
(require 'kkc)

;; XXX
(defun quail-yoko50-update-translation (control-flag)
  ;; (message "<%S><%S><%S>" control-flag quail-current-key quail-current-str)
  (cond ((null control-flag)
         (setq quail-current-str (or quail-current-str quail-current-key)))
        ((integerp control-flag)
         (setq unread-command-events
               (append (substring quail-current-key control-flag)
                       unread-command-events)))
        ((eq t control-flag)
         (save-excursion
           ;; (setq quail-translating nil) ; XXX: ???
           (let* ((beg (overlay-start quail-conv-overlay))
                  (end (overlay-end quail-conv-overlay))
                  char)
             (cond ((and (< beg end) (string= "m" quail-current-key))
                    (goto-char (1- end))
                    (setq char (char-after))
                    (cond ((looking-at-p
                            "[うかきくけこさしすせそたちつてとはひふへほ]")
                           (setq quail-current-str "")
                           (insert (if (= ?う char) ?ヴ (1+ char))) ; 濁音化
                           (delete-char 1)
                           ;;
                           (setq quail-conversion-str
                                 (buffer-substring beg (point)))
                           ;;
                           )))
                   ((and (< (1+ beg) end) (string= "km" quail-current-key))
                    (goto-char (- end 2))
                    (setq char (char-after))
                    (cond ((looking-at-p "[はひふへほ]")
                           (setq quail-current-str "")
                           (insert (+ 2 char)) ; 半濁音化
                           (delete-char 2)
                           ;;
                           (setq quail-conversion-str
                                 (buffer-substring beg (point)))
                           ;;
                           )
                          ((looking-at-p "[かけ]")
                           (setq quail-current-str "")
                           (insert (if (= ?か char) ?ヵ ?ヶ))
                           (delete-char 2)
                           ;;
                           (setq quail-conversion-str
                                 (buffer-substring beg (point)))
                           ;;
                           )))
                   (t nil))
             )))
        (t nil))
  control-flag)

(defun quail-yoko50-toggle-kana ()
  (interactive)
  (setq quail-translating nil)
  (let ((start (overlay-start quail-conv-overlay))
	(end (overlay-end quail-conv-overlay)))
    (save-excursion
      (goto-char start)
      (if ;; (re-search-forward "\\cH" end t)
          (looking-at "\\cH")
          ;;
	  (japanese-katakana-region start end)
	(japanese-hiragana-region start end)))
    (setq quail-conversion-str
	  (buffer-substring (overlay-start quail-conv-overlay)
			    (overlay-end quail-conv-overlay)))
    ;; XXX
    (setq quail-current-key nil)
    (setq quail-current-str nil)
    ;;
    ))

(defun quail-yoko50-kanji-kkc ()
  (interactive)
  ;; (when (= (char-before (overlay-end quail-conv-overlay)) ?n)
  ;;   ;; The last char is `n'.  We had better convert it to `ん'
  ;;   ;; before kana-kanji conversion.
  ;;   (goto-char (1- (overlay-end quail-conv-overlay)))
  ;;   (insert ?ん)
  ;;   (delete-char 1))
  (let* ((from (copy-marker (overlay-start quail-conv-overlay)))
	 (len (- (overlay-end quail-conv-overlay) from)))
    (quail-delete-overlays)
    (setq quail-current-str nil)
    (unwind-protect
	(let ((result (kkc-region from (+ from len))))
	  (move-overlay quail-conv-overlay from (point))
	  (setq quail-conversion-str (buffer-substring from (point)))
	  (if (= (+ from result) (point))
	      (setq quail-converting nil))
	  (setq quail-translating nil))
      (set-marker from nil))))

(defun quail-yoko50-no-conversion ()
  "Terminate the translation. Do no conversion."
  (interactive)
  (setq quail-translating nil)
  (setq quail-converting nil))

(defvar quail-yoko50-transliteration-rules
  '(( "y" "あ") ( "i" "い") ( "u" "う") ( "p" "え") ( "o" "お")
    ( "q" "か") ( "w" "き") ( "t" "く") ( "e" "け") ( "r" "こ")
    ( "a" "さ") ( "s" "し") ( "g" "す") ("dd" "せ") ( "f" "そ")
    ( "z" "た") ( "x" "ち") ( "b" "つ") ( "c" "て") ( "v" "と")
    ("kq" "な") ("kw" "に") ("kt" "ぬ") ("ke" "ね") ("kr" "の")
    ("ka" "は") ("ks" "ひ") ("kg" "ふ") ("kd" "へ") ("kf" "ほ")
    ("kz" "ま") ("kx" "み") ("kb" "む") ("kc" "め") ("kv" "も")
    ("dh" "や")             ("dj" "ゆ")             ("dl" "よ")
    ("dy" "ら") ("di" "り") ("du" "る") ("dp" "れ") ("do" "ろ")
    ("dn" "わ") ("ds" "ゐ")             ("da" "ゑ") ( "h" "を")
    ( "n" "ん")
    ;; ("qm" "が") ("wm" "ぎ") ("tm" "ぐ") ("em" "げ") ("rm" "ご")
    ;; ("am" "ざ") ("sm" "じ") ("gm" "ず") ("ddm" "ぜ") ("fm" "ぞ")
    ;; ("zm" "だ") ("xm" "ぢ") ("bm" "づ") ("cm" "で") ("vm" "ど")
    ("dq" "ば") ("dw" "び") ("dt" "ぶ") ("de" "べ") ("dr" "ぼ")
    ("dz" "ぱ") ("dx" "ぴ") ("db" "ぷ") ("dc" "ぺ") ("dv" "ぽ")
    ("ky" "ぁ") ("ki" "ぃ") ("ku" "ぅ") ("kp" "ぇ") ("ko" "ぉ")
    ( "j" "っ") ("kh" "ゃ") ("kj" "ゅ") ( "l" "ょ") ("kn" "ゎ")
    ;; ("um" "ヴ") ("qkm" "ヵ") ("ekm" "ヶ")
    ("dm" "ー") ( "," "、") ( "." "。")
    ("d/" "・") ( "m" "゛") ("km" "゜")
    ;; ( "[" "「") ( "]" "」")
    ("d," "「") ("d." "」") ("k," "『") ("k." "』") ("k/" "/")

    ("/q" "○") ("//q" "●")
    ("/w" "▽") ("//w" "▼")
    ("/e" "△") ("//e" "▲")
    ("/r" "□") ("//r" "■")
    ("/t" "◇") ("//t" "◆")
    ("/y" "☆") ("//y" "★")
    ("/u" "◎") ("//u" "£")
    ("/i" "¢") ("//i" "×")
    ("/o" "≠") ("//o" "±")
    ("/p" "〒") ("//p" "∞")
    ("/a" "々") ("//a" "仝")
    ("/s" "ヽ") ("//s" "ヾ")
    ("/d" "ゝ") ("//d" "ゞ")
    ("/f" "〃") ("//f" "￥")
    ("/g" "‐") ("//g" "—")
    ("/h" "←") ("//h" "‘")
    ("/j" "↓") ("//j" "’")
    ("/k" "↑") ("//k" "“")
    ("/l" "→") ("//l" "”")
    ("/;" "＼") ("//;" "‖")
    ("/z" "《") ("//z" "〈")
    ("/x" "》") ("//x" "〉")
    ("/c" "〇") ("//c" "℃")
    ("/v" "※") ("//v" "÷")
    ("/b" "〆") ("//b" "§")
    ("/n" "〜") ("//n" "∴")
    ("/m" "　") ("//m" "〓")
    ("/," "‥") ("//," "【")
    ("/." "…") ("//." "】")
    ))

(quail-define-package
 "yoko50"                               ; name
 "Japanese"                             ; language
 "yoko50"                               ; title
 nil                                    ; guidance
 "yoko50"                               ; docstring
 nil                                    ; translation-keys
 t                                      ; forget-last-selection
 t                                      ; deterministic
 nil                                    ; kbd-translate
 nil                                    ; show-layout
 nil                                    ; create-decode-map
 nil                                    ; maximum-shortest
 nil                                    ; overlay-plist
 ;; nil                                    ; update-translation-function
 #'quail-yoko50-update-translation      ; update-translation-function
 ;; conversion-keys
 '(("K" . quail-yoko50-toggle-kana)
   ;; ("\C-k" . quail-yoko50-toggle-kana)
   (" " . quail-yoko50-kanji-kkc)
   ;; ("\C-m" . quail-no-conversion)
   ("\C-m" . quail-yoko50-no-conversion)
   ;; ([return] . quail-no-conversion)
   ;; ("\C-j" . quail-no-conversion)
   )
 t                                      ; simple
 )

(dolist (elt quail-yoko50-transliteration-rules)
  (quail-defrule (car elt) (nth 1 elt)))

;;; provide

(provide 'yoko50)
;;; yoko50.el ends here
