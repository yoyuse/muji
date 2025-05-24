;;; kana101.el --- JIS kana input on 101 keyboard    -*- lexical-binding: t; -*-

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

;; (require 'kana101 nil t)
;;
;; (setq default-input-method "kana101")
;;
;; (define-key kkc-keymap (kbd "H") nil)
;; (define-key kkc-keymap (kbd "K") nil)
;; (define-key kkc-keymap (kbd "O") nil)
;; (define-key kkc-keymap (kbd "I") nil)
;; (define-key kkc-keymap (kbd "l") nil)
;; (define-key kkc-keymap (kbd "L") nil)
;;
;; (define-key kkc-keymap (kbd "C-u") 'kkc-kana101-ascii)
;; (define-key kkc-keymap (kbd "C-j") 'kkc-kana101-hiragana)
;; (define-key kkc-keymap (kbd "C-k") 'kkc-katakana)
;; (define-key kkc-keymap (kbd "C-l") 'kkc-longer-phrase)
;; (define-key kkc-keymap (kbd "C-y") 'kkc-shorter-conversion)
;; (define-key kkc-keymap (kbd "C-v") 'kkc-show-conversion-list-or-next-group)
;; (define-key kkc-keymap (kbd "M-v") 'kkc-show-conversion-list-or-prev-group)

;;; Code:

(require 'quail)
(require 'kkc)

(defun quail-kana101-update-translation (control-flag)
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
                         (< beg point) (string= "[" quail-current-key))
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
                         (< beg point) (string= "]" quail-current-key))
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

(defun quail-kana101-toggle-kana ()
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

(defun quail-kana101-hiragana ()
  (interactive)
  (setq quail-translating nil)
  (let ((start (overlay-start quail-conv-overlay))
        (end (overlay-end quail-conv-overlay)))
    (save-excursion
      (goto-char start)
      (japanese-hiragana-region start end))
    (setq quail-conversion-str
          (buffer-substring (overlay-start quail-conv-overlay)
                            (overlay-end quail-conv-overlay)))
    (setq quail-current-key nil)
    (setq quail-current-str nil)))

(defun quail-kana101-katakana ()
  (interactive)
  (setq quail-translating nil)
  (let ((start (overlay-start quail-conv-overlay))
        (end (overlay-end quail-conv-overlay)))
    (save-excursion
      (goto-char start)
      (japanese-katakana-region start end))
    (setq quail-conversion-str
          (buffer-substring (overlay-start quail-conv-overlay)
                            (overlay-end quail-conv-overlay)))
    (setq quail-current-key nil)
    (setq quail-current-str nil)))

(defun quail-kana101-kanji-kkc ()
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

(defun quail-kana101-no-conversion ()
  "Terminate the translation. Do no conversion."
  (interactive)
  (setq quail-translating nil)
  (setq quail-converting nil))

(defvar quail-kana101-transliteration-rules
  '(( "3" "あ") ( "e" "い") ( "4" "う") ( "5" "え") ( "6" "お")
    ( "t" "か") ( "g" "き") ( "h" "く") ( "'" "け") ( "b" "こ")
    ( "x" "さ") ( "d" "し") ( "r" "す") ( "p" "せ") ( "c" "そ")
    ( "q" "た") ( "a" "ち") ( "z" "つ") ( "w" "て") ( "s" "と")
    ( "u" "な") ( "i" "に") ( "1" "ぬ") ( "," "ね") ( "k" "の")
    ( "f" "は") ( "v" "ひ") ( "2" "ふ") ( "=" "へ") ( "-" "ほ")
    ( "j" "ま") ( "n" "み") ("\\" "む") ( "/" "め") ( "m" "も")
    ( "7" "や")             ( "8" "ゆ")             ( "9" "よ")
    ( "o" "ら") ( "l" "り") ( "." "る") ( ";" "れ") ( "`" "ろ")
    ( "0" "わ") ( ")" "を") ( "y" "ん")
    ( "#" "ぁ") ( "E" "ぃ") ( "$" "ぅ") ( "%" "ぇ") ( "^" "ぉ")
    ( "Z" "っ") ( "&" "ゃ") ( "*" "ゅ") ( "(" "ょ")
    ( "_" "ー") ( "{" "「") ( "}" "」") ( "<" "、") ( ">" "。")
    ( "?" "・") ( "[" "゛") ( "]" "゜")
    ;; ("t[" "が") ("g[" "ぎ") ("h[" "ぐ") ("'[" "げ") ("b[" "ご")
    ;; ("x[" "ざ") ("d[" "じ") ("r[" "ず") ("p[" "ぜ") ("c[" "ぞ")
    ;; ("q[" "だ") ("a[" "ぢ") ("z[" "づ") ("w[" "で") ("s[" "ど")
    ;; ("f[" "ば") ("v[" "び") ("2[" "ぶ") ("=[" "べ") ("-[" "ぼ")
    ;; ("f]" "ぱ") ("v]" "ぴ") ("2]" "ぷ") ("=]" "ぺ") ("-]" "ぽ")
    ;; ("0]" "ゎ") ("e]" "ゐ")             ("5]" "ゑ")
    ;; ("4[" "ヴ") ("t]" "ヵ") ("']" "ヶ")

    ("~1" "○") ("~!" "●")
    ("~2" "▽") ("~@" "▼")
    ("~3" "△") ("~#" "▲")
    ("~4" "□") ("~$" "■")
    ("~5" "◇") ("~%" "◆")
    ("~6" "☆") ("~^" "★")
    ("~7" "◎") ("~&" "£")
    ("~8" "¢") ("~*" "×")
    ("~9" "♂") ("~(" "【")
    ("~0" "♀") ("~)" "】")
    ("~-" "〜") ("~_" "∴")
    ("~=" "≠") ("~+" "±")
    ("~\\" "＼") ("~|" "‖")
    ("~`" "´") ("~~" "¨")

    ("~q" "《") ("~Q" "〈")
    ("~w" "》") ("~W" "〉")
    ("~r" "々") ("~R" "仝")
    ("~t" "〆") ("~T" "§")
                ("~Y" "￥")             ; 円記号
    ("~p" "〒") ("~P" "↑")
    ("~[" "『") ("~{" "〔")
    ("~]" "』") ("~}" "〕")

    ("~s" "ヽ") ("~S" "ヾ")
    ("~d" "ゝ") ("~D" "ゞ")
    ("~f" "〃") ("~F" "→")
    ("~g" "‐") ("~G" "—")
    ("~h" "←")
    ("~j" "↓")
    ("~k" "↑")
    ("~l" "→")
    ("~;" "゛") ("~:" "゜")
    ("~'" "‘") ("~\"" "“")

    ("~x" "’") ("~X" "”")             ; 全角引用符
    ("~c" "〇") ("~C" "℃")
    ("~v" "※") ("~V" "÷")
    ("~b" "°") ("~B" "←")
    ("~n" "′") ("~N" "↓")
    ("~m" "″") ("~M" "〓")
    ("~," "‥") ("~<" "≦")
    ("~." "…") ("~>" "≧")
    ;; ("~/" "・")
    ("~/" "?")                          ; 疑問符
    ("~?" "∞")))

(quail-define-package
 "kana101"
 "Japanese"
 "かな"
 nil
 "JIS-kana on 101 keyboard.

ぬふあうえ おやゆよわほへむろ    ! @ ぁぅぇ ぉゃゅょをｰ + | ~
たていすか んなにらせ゛゜        Q W ぃR T  Y U I O P 「」
ちとしはき くまのりれけ          A S D F G  H J K L : \"
つさそひこ みもねるめ            っX C V B  N M 、。・

○▽△□◇ ☆◎¢♂♀〜≠＼´    ●▼▲■◆ ★£×【】∴±‖¨
《》  々〆         〒『』        〈〉  仝§ ￥      ↑〔〕
  ヽゝ〃‐ ←↓↑→゛‘            ヾゞ→—         ゜“
  ’〇※° ′″‥…?               ”℃÷← ↓〓≦≧∞
"
 nil
 t
 t
 nil
 nil
 nil
 nil
 nil
 #'quail-kana101-update-translation
 '(("\C-j" . quail-kana101-hiragana)
   ("\C-k" . quail-kana101-katakana)
   ("\C-u" . quail-kana101-ascii)
   (" " . quail-kana101-kanji-kkc)
   ("\C-m" . quail-kana101-no-conversion)
   ([return] . quail-kana101-no-conversion))
 t)

(dolist (elt quail-kana101-transliteration-rules)
  (quail-defrule (car elt) (nth 1 elt)))

;;; ascii conversion

(defvar kana101-ascii-kana101-transliteration-rules
  (append quail-kana101-transliteration-rules
          '(("t[" "が") ("g[" "ぎ") ("h[" "ぐ") ("'[" "げ") ("b[" "ご")
            ("x[" "ざ") ("d[" "じ") ("r[" "ず") ("p[" "ぜ") ("c[" "ぞ")
            ("q[" "だ") ("a[" "ぢ") ("z[" "づ") ("w[" "で") ("s[" "ど")
            ("f[" "ば") ("v[" "び") ("2[" "ぶ") ("=[" "べ") ("-[" "ぼ")
            ("f]" "ぱ") ("v]" "ぴ") ("2]" "ぷ") ("=]" "ぺ") ("-]" "ぽ")
            ("0]" "ゎ") ("e]" "ゐ")             ("5]" "ゑ")
            ("4[" "ヴ") ("t]" "ヵ") ("']" "ヶ"))))

(defun kana101-ascii (&optional current-key length-head)
  (interactive)
  (let* ((kkc-current-key (if current-key current-key kkc-current-key))
         (kkc-length-head (if length-head length-head kkc-length-head)))
    (mapconcat
     #'(lambda (c)
         (or (car (cl-rassoc (list (char-to-string c))
                             kana101-ascii-kana101-transliteration-rules
                             :test #'equal))
             (char-to-string c)))
     (string-to-list
      (substring (mapconcat #'(lambda (c) (char-to-string c))
                            kkc-current-key "")
                 0 kkc-length-head))
     "")))

(defun quail-kana101-ascii ()
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
                               (kana101-ascii current-key length-head))
    (setq quail-conversion-str
          (buffer-substring (overlay-start quail-conv-overlay)
                            (overlay-end quail-conv-overlay)))
    (setq quail-current-key nil)
    (setq quail-current-str nil))))

(defun kkc-kana101-ascii ()
  "Convert to ASCII."
  (interactive)
  (kana101-ascii-kkc-update-conversion))

(defun kkc-toggle-kana ()
  "Convert to Katakana/Hiragana."
  (interactive)
  (setcar kkc-current-conversions
          (if (/= -1 (car kkc-current-conversions)) -1 0))
  (kkc-update-conversion 'all))

(defun kkc-kana101-hiragana ()
  "Convert to hiragana."
  (interactive)
  (setcar kkc-current-conversions 0)
  (kkc-update-conversion 'all))

(defun kana101-ascii-kkc-update-conversion (&optional all)
  (goto-char (overlay-start kkc-overlay-head))
  (insert (kana101-ascii))
  (delete-region (point) (overlay-end kkc-overlay-head))
  (unwind-protect
      (run-hook-with-args 'kkc-after-update-conversion-functions
                          (overlay-start kkc-overlay-head)
                          (overlay-end kkc-overlay-head))
    (goto-char (overlay-end kkc-overlay-tail))))

;;; cursor color

(defvar kana101-cursor-color-use-color t)

(defvar kana101-cursor-color-default
  (cdr (assq 'cursor-color (frame-parameters (selected-frame)))))

(defun kana101-cursor-color ()
  (cond
   ((equal current-input-method "kana101") '("firebrick2" . "firebrick1"))
   (t kana101-cursor-color-default)))

(defun kana101-cursor-color-set-color ()
  (when kana101-cursor-color-use-color
    (let* ((light-dark (frame-parameter nil 'background-mode))
           (light-mode (eq light-dark 'light))
           (color (kana101-cursor-color))
           (color (if (consp color)
                      (if light-mode (car color) (cdr color))
                    color)))
      (set-cursor-color color))))

(add-hook 'post-command-hook 'kana101-cursor-color-set-color)

;;; provide

(provide 'kana101)
;;; kana101.el ends here
