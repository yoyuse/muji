;;; muji-newjis101.el --- new JIS kana input for muji  -*- lexical-binding: t; -*-

;; Copyright (C) 2023, 2024  YUSE Yosihiro

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
;; (define-key global-map (kbd "C-x C-\\") 'global-muji-mode)
;; (load "muji-newjis101")

;;; Code:

(require 'muji)

(defgroup muji-newjis101 nil
  "new JIS kana input for muji."
  :group 'muji
  :prefix "muji-newjis101-")

(defcustom muji-newjis101-delimiter "~"
  "Delimiter between ASCII and roman strings."
  :type '(choice string (const nil))
  :group 'muji-newjis101)

(defcustom muji-newjis101-stop-chars "()"
  "String of characters that should not be icluded in roman string."
  :type '(choice string (const nil))
  :group 'muji-newjis101)

(defcustom muji-newjis101-phrase-separator "?"
  "Phrase separator used in `muji-kkc-phrases'."
  :type '(choice string (const nil))
  :group 'muji-newjis101)

(defcustom muji-newjis101-hiragana-nfer "#"
  "Hiragana conversion suffix."
  :type 'string
  :group 'muji-newjis101)

(defcustom muji-newjis101-katakana-nfer "*"
  "Katakana conversion suffix."
  :type 'string
  :group 'muji-newjis101)

(defvar muji-newjis101-transliteration-rules
  '(( "b" "あ") ( "k" "い") ( "j" "う") ( "U" "え") ( "J" "お")
    ( "s" "か") ( ";" "き") ( "h" "く") ( "w" "け") ( "x" "こ")
    ( "v" "さ") ( "d" "し") ( "z" "す") ( "e" "せ") ( "q" "そ")
    ( "g" "た") ( "[" "ち") ( "y" "つ") ( "r" "て") ( "f" "と")
    ( "'" "な") ( "c" "に") ( "P" "ぬ") ( "V" "ね") ( "i" "の")
    ( "a" "は") ( "Y" "ひ") ( "R" "ふ") ( "S" "へ") ( "E" "ほ")
    ( "H" "ま") ( "I" "み") ( "N" "む") ( "T" "め") ( "K" "も")
    ( "O" "や")             ( ":" "ゆ")             ( "G" "よ")
    ( "D" "ら") ( "p" "り") ( "m" "る") ( "/" "れ") ( "M" "ろ")
    ( "L" "わ") ("kl" "ゐ")             ("Ul" "ゑ") ( "o" "を")
    ( "u" "ん")
    ("sl" "が") (";l" "ぎ") ("hl" "ぐ") ("wl" "げ") ("xl" "ご")
    ("vl" "ざ") ("dl" "じ") ("zl" "ず") ("el" "ぜ") ("ql" "ぞ")
    ("gl" "だ") ("[l" "ぢ") ("yl" "づ") ("rl" "で") ("fl" "ど")
    ("al" "ば") ("Yl" "び") ("Rl" "ぶ") ("Sl" "べ") ("El" "ぼ")
    ("aW" "ぱ") ("YW" "ぴ") ("RW" "ぷ") ("SW" "ぺ") ("EW" "ぽ")
    ( "Q" "ぁ") ( "A" "ぃ") ( "Z" "ぅ") ( "X" "ぇ") ( "C" "ぉ")
    ( "n" "っ") ( "B" "ゃ") ( "F" "ゅ") ( "t" "ょ") ("LW" "ゎ")
    ("jl" "ヴ") ("sW" "ヵ") ("wW" "ヶ")
    ( ">" "ー") ( "{" "「") ( "}" "」") ( "," "、") ( "." "。")
    ( "<" "・") ( "l" "゛") ( "W" "゜")

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

    ;; "-" が "ー" になってしまって具合が悪い
    ;; ("$x" [":-"]) ("$X" [":-)"])
    ;; XXX: 全角引用符の救済策
    ("$x" "’") ("$X"  "”")

    ("$c" "〇") ("$C" "℃")
    ("$v" "※") ("$V" "÷")
    ("$b" "°") ("$B" "←")
    ("$n" "′") ("$N" "↓")
    ("$m" "″") ("$M" "〓")
    ("$," "‥") ("$<" "≦")
    ("$." "…") ("$>" "≧")
    ("$/" "・") ("$?" "∞")
    ))

(defvar muji-newjis101-roman-pattern
  (muji-make-roman-pattern muji-newjis101-transliteration-rules))

(defcustom muji-newjis101-active-cursor-color "coral"
  "Cursor color when muji mode is active."
  :type 'color
  :group 'muji-newjis101)

(defun muji-delimiter () muji-newjis101-delimiter)
(defun muji-stop-chars () muji-newjis101-stop-chars)
(defun muji-phrase-separator () muji-newjis101-phrase-separator)
(defun muji-hiragana-nfer () muji-newjis101-hiragana-nfer)
(defun muji-katakana-nfer () muji-newjis101-katakana-nfer)
(defun muji-transliteration-rules () muji-newjis101-transliteration-rules)
(defun muji-roman-pattern () muji-newjis101-roman-pattern)
(defun muji-preprocess (string) string)
(defun muji-active-cursor-color () muji-newjis101-active-cursor-color)

;;; provide

(provide 'muji-newjis101)
;;; muji-newjis101.el ends here
