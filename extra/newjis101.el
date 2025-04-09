;;; newjis101.el --- new JIS kana input on 101 keyboard  -*- lexical-binding: t; -*-

;; Copyright (C) 2023, 2025  YUSE Yosihiro

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

;; (require 'newjis101 nil t)
;;
;; (setq default-input-method "newjis101")
;;
;; (define-key kkc-keymap (kbd "H") nil)
;; (define-key kkc-keymap (kbd "K") nil)
;; (define-key kkc-keymap (kbd "O") nil)
;; (define-key kkc-keymap (kbd "I") nil)
;; (define-key kkc-keymap (kbd "l") nil)
;; (define-key kkc-keymap (kbd "L") nil)
;;
;; (define-key kkc-keymap (kbd "C-u") 'kkc-newjis101-ascii)
;; (define-key kkc-keymap (kbd "C-j") 'kkc-newjis101-hiragana)
;; (define-key kkc-keymap (kbd "C-k") 'kkc-katakana)
;; (define-key kkc-keymap (kbd "C-l") 'kkc-longer-phrase)
;; (define-key kkc-keymap (kbd "C-y") 'kkc-shorter-conversion)
;; (define-key kkc-keymap (kbd "C-v") 'kkc-show-conversion-list-or-next-group)
;; (define-key kkc-keymap (kbd "M-v") 'kkc-show-conversion-list-or-prev-group)

;;; Code:

(require 'quail)
(require 'kkc)

(defun quail-newjis101-update-translation (control-flag)
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
                  (point (point))
                  char)
             (cond ((and (number-or-marker-p beg) (number-or-marker-p end)
                         (< beg point) (string= "l" quail-current-key))
                    (goto-char (1- point))
                    (setq char (char-after))
                    (cond ((looking-at-p
                            "[うかきくけこさしすせそたちつてとはひふへほ]")
                           (setq quail-current-str "")
                           (insert (if (= ?う char) ?ヴ (1+ char))) ; 濁音化
                           (delete-char 1)
                           (setq quail-conversion-str
                                 (buffer-substring beg end)))))
                   ((and (number-or-marker-p beg) (number-or-marker-p end)
                         (< beg point) (string= "W" quail-current-key))
                    (goto-char (1- point))
                    (setq char (char-after))
                    (cond ((looking-at-p "[はひふへほ]")
                           (setq quail-current-str "")
                           (insert (+ 2 char)) ; 半濁音化
                           (delete-char 1)
                           (setq quail-conversion-str
                                 (buffer-substring beg end)))
                          ((looking-at-p "[わいえ]")
                           (setq quail-current-str "")
                           (insert (cond ((= ?わ char) ?ゎ)
                                         ((= ?い char) ?ゐ)
                                         ((= ?え char) ?ゑ)))
                           (delete-char 1)
                           (setq quail-conversion-str
                                 (buffer-substring beg end)))
                          ((looking-at-p "[かけ]")
                           (setq quail-current-str "")
                           (insert (if (= ?か char) ?ヵ ?ヶ))
                           (delete-char 1)
                           (setq quail-conversion-str
                                 (buffer-substring beg end)))))
                   (t nil)))))
        (t nil))
  control-flag)

(defun quail-newjis101-toggle-kana ()
  (interactive)
  (setq quail-translating nil)
  (let ((start (overlay-start quail-conv-overlay))
        (end (overlay-end quail-conv-overlay)))
    (save-excursion
      (goto-char start)
      (if (looking-at "\\cH")
          (japanese-katakana-region start end)
        (japanese-hiragana-region start end)))
    (setq quail-conversion-str
          (buffer-substring (overlay-start quail-conv-overlay)
                            (overlay-end quail-conv-overlay)))
    (setq quail-current-key nil)
    (setq quail-current-str nil)))

(defun quail-newjis101-kanji-kkc ()
  (interactive)
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

(defun quail-newjis101-no-conversion ()
  "Terminate the translation. Do no conversion."
  (interactive)
  (setq quail-translating nil)
  (setq quail-converting nil))

(defvar quail-newjis101-transliteration-rules
  '(( "b" "あ") ( "k" "い") ( "j" "う") ( "U" "え") ( "J" "お")
    ( "s" "か") ( ";" "き") ( "h" "く") ( "w" "け") ( "x" "こ")
    ( "v" "さ") ( "d" "し") ( "z" "す") ( "e" "せ") ( "q" "そ")
    ( "g" "た") ( "[" "ち") ( "y" "つ") ( "r" "て") ( "f" "と")
    ( "'" "な") ( "c" "に") ( "P" "ぬ") ( "V" "ね") ( "i" "の")
    ( "a" "は") ( "Y" "ひ") ( "R" "ふ") ( "S" "へ") ( "E" "ほ")
    ( "H" "ま") ( "I" "み") ( "N" "む") ( "T" "め") ( "K" "も")
    ( "O" "や")             ( ":" "ゆ")             ( "G" "よ")
    ( "D" "ら") ( "p" "り") ( "m" "る") ( "/" "れ") ( "M" "ろ")
    ( "L" "わ") ( "o" "を") ( "u" "ん")
    ( "Q" "ぁ") ( "A" "ぃ") ( "Z" "ぅ") ( "X" "ぇ") ( "C" "ぉ")
    ( "n" "っ") ( "B" "ゃ") ( "F" "ゅ") ( "t" "ょ")
    ( ">" "ー") ( "{" "「") ( "}" "」") ( "," "、") ( "." "。")
    ( "<" "・") ( "l" "゛") ( "W" "゜")
    ;; ("sl" "が") (";l" "ぎ") ("hl" "ぐ") ("wl" "げ") ("xl" "ご")
    ;; ("vl" "ざ") ("dl" "じ") ("zl" "ず") ("el" "ぜ") ("ql" "ぞ")
    ;; ("gl" "だ") ("[l" "ぢ") ("yl" "づ") ("rl" "で") ("fl" "ど")
    ;; ("al" "ば") ("Yl" "び") ("Rl" "ぶ") ("Sl" "べ") ("El" "ぼ")
    ;; ("aW" "ぱ") ("YW" "ぴ") ("RW" "ぷ") ("SW" "ぺ") ("EW" "ぽ")
    ;; ("LW" "ゎ") ("kW" "ゐ")             ("UW" "ゑ")
    ;; ("jl" "ヴ") ("sW" "ヵ") ("wW" "ヶ")

    ("$1" "○") ("$!" "●")
    ("$2" "▽") ("$@" "▼")
    ("$3" "△") ("$#" "▲")
    ("$4" "□") ("$$" "■")
    ("$5" "◇") ("$%" "◆")
    ("$6" "☆") ("$^" "★")
    ("$7" "◎") ("$&" "£")
    ("$8" "¢") ("$*" "×")
    ("$9" "♂") ("$(" "【")
    ("$0" "♀") ("$)" "】")
    ("$-" "〜") ("$_" "∴")
    ("$=" "≠") ("$+" "±")
    ("$\\" "＼") ("$|" "‖")
    ("$`" "´") ("$~" "¨")

    ("$q" "《") ("$Q" "〈")
    ("$w" "》") ("$W" "〉")
    ("$r" "々") ("$R" "仝")
    ("$t" "〆") ("$T" "§")
    ("$p" "〒") ("$P" "↑")
    ("$[" "『") ("${" "〔")
    ("$]" "』") ("$}" "〕")

    ("$s" "ヽ") ("$S" "ヾ")
    ("$d" "ゝ") ("$D" "ゞ")
    ("$f" "〃") ("$F" "→")
    ("$g" "‐") ("$G" "—")
    ("$h" "←")
    ("$j" "↓")
    ("$k" "↑")
    ("$l" "→")
    ("$;" "゛") ("$:" "゜")
    ("$'" "‘") ("$\"" "“")

    ("$x" "’") ("$X"  "”")            ; 全角引用符
    ("$c" "〇") ("$C" "℃")
    ("$v" "※") ("$V" "÷")
    ("$b" "°") ("$B" "←")
    ("$n" "′") ("$N" "↓")
    ("$m" "″") ("$M" "〓")
    ("$," "‥") ("$<" "≦")
    ("$." "…") ("$>" "≧")
    ("$/" "・") ("$?" "∞")
    ))

(quail-define-package
 "newjis101"
 "Japanese"
 "新JIS"
 nil
 "new JIS kana input on 101 keyboard.

そけせてょ つんのをりち    ぁ゜ほふめ ひえみやぬ「」
 はかしとた くうい゛きな    ぃへらゅよ まおもわゆ
  すこにさあ っる、。れ      ぅぇぉねゃ むろ・ー
"
 nil
 t
 t
 nil
 nil
 nil
 nil
 nil
 #'quail-newjis101-update-translation
 '(("\C-j" . quail-newjis101-toggle-kana)
   ("\C-k" . quail-newjis101-toggle-kana)
   ("\C-u" . quail-newjis101-ascii)
   (" " . quail-newjis101-kanji-kkc)
   ("\C-m" . quail-newjis101-no-conversion)
   ([return] . quail-newjis101-no-conversion))
 t)

(dolist (elt quail-newjis101-transliteration-rules)
  (quail-defrule (car elt) (nth 1 elt)))

;;; ascii conversion

(defvar newjis101-ascii-newjis101-transliteration-rules
  (append quail-newjis101-transliteration-rules
          '(("sl" "が") (";l" "ぎ") ("hl" "ぐ") ("wl" "げ") ("xl" "ご")
            ("vl" "ざ") ("dl" "じ") ("zl" "ず") ("el" "ぜ") ("ql" "ぞ")
            ("gl" "だ") ("[l" "ぢ") ("yl" "づ") ("rl" "で") ("fl" "ど")
            ("al" "ば") ("Yl" "び") ("Rl" "ぶ") ("Sl" "べ") ("El" "ぼ")
            ("aW" "ぱ") ("YW" "ぴ") ("RW" "ぷ") ("SW" "ぺ") ("EW" "ぽ")
            ("LW" "ゎ") ("kW" "ゐ")             ("UW" "ゑ")
            ("jl" "ヴ") ("sW" "ヵ") ("wW" "ヶ"))))

(defun newjis101-ascii (&optional current-key length-head)
  (interactive)
  (let* ((kkc-current-key (if current-key current-key kkc-current-key))
         (kkc-length-head (if length-head length-head kkc-length-head)))
    (mapconcat
     #'(lambda (c)
         (or (car (cl-rassoc (list (char-to-string c))
                             newjis101-ascii-newjis101-transliteration-rules
                             :test #'equal))
             (char-to-string c)))
     (string-to-list
      (substring (mapconcat #'(lambda (c) (char-to-string c))
                            kkc-current-key "")
                 0 kkc-length-head))
     "")))

(defun quail-newjis101-ascii ()
  (interactive)
  (setq quail-translating nil)
  (let* ((start (overlay-start quail-conv-overlay))
         (end (overlay-end quail-conv-overlay))
         (str (buffer-substring-no-properties start end))
         (current-key (string-to-list str))
         (length-head (length current-key)))
    (save-excursion
      (goto-char start)
      (japanese-replace-region start end
                               (newjis101-ascii current-key length-head))
    (setq quail-conversion-str
          (buffer-substring (overlay-start quail-conv-overlay)
                            (overlay-end quail-conv-overlay)))
    (setq quail-current-key nil)
    (setq quail-current-str nil))))

(defun kkc-newjis101-ascii ()
  "Convert to ASCII."
  (interactive)
  (newjis101-ascii-kkc-update-conversion))

(defun kkc-toggle-kana ()
  "Convert to Katakana/Hiragana."
  (interactive)
  (setcar kkc-current-conversions
          (if (/= -1 (car kkc-current-conversions)) -1 0))
  (kkc-update-conversion 'all))

(defun kkc-newjis101-hiragana ()
  "Convert to hiragana."
  (interactive)
  (setcar kkc-current-conversions 0)
  (kkc-update-conversion 'all))

(defun newjis101-ascii-kkc-update-conversion (&optional all)
  (goto-char (overlay-start kkc-overlay-head))
  (insert (newjis101-ascii))
  (delete-region (point) (overlay-end kkc-overlay-head))
  (unwind-protect
      (run-hook-with-args 'kkc-after-update-conversion-functions
                          (overlay-start kkc-overlay-head)
                          (overlay-end kkc-overlay-head))
    (goto-char (overlay-end kkc-overlay-tail))))

;;; provide

(provide 'newjis101)
;;; newjis101.el ends here
