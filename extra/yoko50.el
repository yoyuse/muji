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
         ;; (setq quail-current-str (or quail-current-str quail-current-key)))
         (setq quail-current-str
               (if (/= (aref quail-current-key 0) ?\;)
                   (or quail-current-str quail-current-key)
                 "")))
        ;;
        ((integerp control-flag)
         (setq unread-command-events
               (append (substring quail-current-key control-flag)
                       unread-command-events)))
        ((eq t control-flag)
         (save-excursion
           ;; (setq quail-translating nil) ; XXX: ???
           (let* ((beg (overlay-start quail-conv-overlay))
                  (end (overlay-end quail-conv-overlay))
                  (point (point))
                  char)
             (cond ((and (number-or-marker-p beg) (number-or-marker-p end)
                         (< beg point) (string= "m" quail-current-key))
                    (goto-char (1- point))
                    (setq char (char-after))
                    (cond ((looking-at-p
                            "[うかきくけこさしすせそたちつてとはひふへほ]")
                           (setq quail-current-str "")
                           (insert (if (= ?う char) ?ヴ (1+ char))) ; 濁音化
                           (delete-char 1)
                           ;;
                           (setq quail-conversion-str
                                 (buffer-substring beg end))
                           ;;
                           )))
                   ((and (number-or-marker-p beg) (number-or-marker-p end)
                         (< (1+ beg) point) (string= "km" quail-current-key))
                    (goto-char (- point 2))
                    (setq char (char-after))
                    (cond ((looking-at-p "[はひふへほ]")
                           (setq quail-current-str "")
                           (insert (+ 2 char)) ; 半濁音化
                           (delete-char 2)
                           ;;
                           (setq quail-conversion-str
                                 (buffer-substring beg end))
                           ;;
                           )
                          ((looking-at-p "[かけ]")
                           (setq quail-current-str "")
                           (insert (if (= ?か char) ?ヵ ?ヶ))
                           (delete-char 2)
                           ;;
                           (setq quail-conversion-str
                                 (buffer-substring beg end))
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

(defvar quail-yoko50-switch-package-hook nil
  "Hook run by `quail-yoko50-switch-package'.")

(defvar quail-yoko50-switch-table
  '((?q . ("yoko50-ascii"))
    (?a . "yoko50-ascii")
    (?j . "yoko50")
    (?z . "yoko50-zenkaku")))

(defvar-local quail-yoko50-package-saved nil)
(put 'quail-yoko50-package-saved 'permanent-local t)

(defun quail-yoko50-switch-package (key idx)
  (quail-delete-region)
  (setq quail-current-str nil
        quail-converting nil
        quail-conversion-str "")
  (let ((pkg (cdr (assq (aref key (1- idx)) quail-yoko50-switch-table))))
    (if (null pkg)
        (quail-error "No package to be switched")
      (if (stringp pkg)
          ;; (activate-input-method pkg)
          (progn (if (and (null quail-yoko50-package-saved)
                          (not (string= pkg current-input-method)))
                     (setq quail-yoko50-package-saved current-input-method))
                 (activate-input-method pkg))
        ;;
        (if (string= (car pkg) current-input-method)
            (if quail-yoko50-package-saved
                (activate-input-method quail-yoko50-package-saved))
          (setq quail-yoko50-package-saved current-input-method)
          (activate-input-method (car pkg))))))
  (run-hooks 'quail-yoko50-switch-package-hook)
  (throw 'quail-tag nil))

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

    (";q" quail-yoko50-switch-package)
    (";a" quail-yoko50-switch-package)
    (";j" quail-yoko50-switch-package)
    (";z" quail-yoko50-switch-package)
    ))

(quail-define-package
 "yoko50"                               ; name
 "Japanese"                             ; language
 "横"                                   ; title
 nil                                    ; guidance
                                        ; docstring
 "Japanese input method by Yoko50 layout and Kana-Kanji conversion.

Special key bindings
--------------------
K	Change Hiragana to Katakana or Katakana to Hiragana.
;q	Toggle between this input method and the input method `yoko50-ascii'.
;a	Shift to the input method `yoko50-ascii'.
;j	Shift to the input method `yoko50'.
;z	Shift to the input method `yoko50-zenkaku'.
RET	Accept the current character sequence.
SPC	Proceed to the next stage, Kana-Kanji conversion.
"
 nil                                    ; translation-keys
 t                                      ; forget-last-selection
 t                                      ; deterministic
 nil                                    ; kbd-translate
 nil                                    ; show-layout
 nil                                    ; create-decode-map
 nil                                    ; maximum-shortest
 nil                                    ; overlay-plist
 #'quail-yoko50-update-translation      ; update-translation-function
                                        ; conversion-keys
 '(("K" . quail-yoko50-toggle-kana)
   (" " . quail-yoko50-kanji-kkc)
   ("\C-m" . quail-no-conversion)
   ([return] . quail-no-conversion))
 t                                      ; simple
 )

(dolist (elt quail-yoko50-transliteration-rules)
  (quail-defrule (car elt) (nth 1 elt)))

(quail-define-package
 "yoko50-ascii" "Japanese" "Aa"
 nil
 "Temporary ASCII input mode used within the input method `yoko50'.

Special key bindings
--------------------
;q	Go back to previous input method.
;a	Shift to the input method `yoko50-ascii'.
;j	Shift to the input method `yoko50'.
;z	Shift to the input method `yoko50-zenkaku'.
"
 nil t t)

(quail-define-rules
 (";q" quail-yoko50-switch-package)
 (";a" quail-yoko50-switch-package)
 (";j" quail-yoko50-switch-package)
 (";z" quail-yoko50-switch-package))

(quail-define-package
 "yoko50-zenkaku" "Japanese" "Ａ"
 nil
 "Japanese zenkaku alpha numeric character input method.

Special key bindings
--------------------
;q	Toggle between this input method and the input method `yoko50-ascii'.
;a	Shift to the input method `yoko50-ascii'.
;j	Shift to the input method `yoko50'.
;z	Shift to the input method `yoko50-zenkaku'.
"
 nil t t)

(quail-define-rules

 (" " "　") ("!" "！") ("\"" "″") ("#" "＃")
 ("$" "＄") ("%" "％") ("&" "＆") ("'" "′")
 ("(" "（") (")" "）") ("*" "＊") ("+" "＋")
 ("," "，") ("-" "−") ("." "．") ("/" "／")
 ("0" "０") ("1" "１") ("2" "２") ("3" "３")
 ("4" "４") ("5" "５") ("6" "６") ("7" "７")
 ("8" "８") ("9" "９") (":" "：") (";" "；")
 ("<" "＜") ("=" "＝") (">" "＞") ("?" "？")
 ("@" "＠") ("A" "Ａ") ("B" "Ｂ") ("C" "Ｃ")
 ("D" "Ｄ") ("E" "Ｅ") ("F" "Ｆ") ("G" "Ｇ")
 ("H" "Ｈ") ("I" "Ｉ") ("J" "Ｊ") ("K" "Ｋ")
 ("L" "Ｌ") ("M" "Ｍ") ("N" "Ｎ") ("O" "Ｏ")
 ("P" "Ｐ") ("Q" "Ｑ") ("R" "Ｒ") ("S" "Ｓ")
 ("T" "Ｔ") ("U" "Ｕ") ("V" "Ｖ") ("W" "Ｗ")
 ("X" "Ｘ") ("Y" "Ｙ") ("Z" "Ｚ") ("[" "［")
 ("\\" "￥") ("]" "］") ("^" "＾") ("_" "＿")
 ("`" "‘") ("a" "ａ") ("b" "ｂ") ("c" "ｃ")
 ("d" "ｄ") ("e" "ｅ") ("f" "ｆ") ("g" "ｇ")
 ("h" "ｈ") ("i" "ｉ") ("j" "ｊ") ("k" "ｋ")
 ("l" "ｌ") ("m" "ｍ") ("n" "ｎ") ("o" "ｏ")
 ("p" "ｐ") ("q" "ｑ") ("r" "ｒ") ("s" "ｓ")
 ("t" "ｔ") ("u" "ｕ") ("v" "ｖ") ("w" "ｗ")
 ("x" "ｘ") ("y" "ｙ") ("z" "ｚ") ("{" "｛")
 ("|" "｜") ("}" "｝") ("~" "〜")

 (";q" quail-yoko50-switch-package)
 (";a" quail-yoko50-switch-package)
 (";j" quail-yoko50-switch-package)
 (";z" quail-yoko50-switch-package)
 )

;;; provide

(provide 'yoko50)
;;; yoko50.el ends here
