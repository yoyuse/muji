;;; muji-yoko50.el --- yoko50 input for muji         -*- lexical-binding: t; -*-

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

;; yoko50 from:
;;
;; - 横五十音配列 (横50音配列)
;; - http://jgrammar.life.coocan.jp/ja/tools/imekeys.htm#Yoko50

;;; Setup:

;; (require 'muji)
;; (define-key global-map (kbd "C-x C-\\") 'global-muji-mode)
;; (load "muji-yoko50")

;;; Code:

(require 'muji)

(defgroup muji-yoko50 nil
  "yoko50 input for muji."
  :group 'muji
  :prefix "muji-yoko50-")

(defcustom muji-yoko50-delimiter ":"
  "Delimiter between ASCII and roman strings."
  :type '(choice string (const nil))
  :group 'muji-yoko50)

(defcustom muji-yoko50-stop-chars "(){}<>[]\""
  "String of characters that should not be icluded in roman string."
  :type '(choice string (const nil))
  :group 'muji-yoko50)

(defcustom muji-yoko50-phrase-separator ";"
  "Phrase separator used in `muji-kkc-phrases'."
  :type '(choice string (const nil))
  :group 'muji-yoko50)

(defcustom muji-yoko50-hiragana-nfer "d"
  "Hiragana conversion suffix."
  :type 'string
  :group 'muji-yoko50)

(defcustom muji-yoko50-katakana-nfer "k"
  "Katakana conversion suffix."
  :type 'string
  :group 'muji-yoko50)

(defvar muji-yoko50-transliteration-rules
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
    ("qm" "が") ("wm" "ぎ") ("tm" "ぐ") ("em" "げ") ("rm" "ご")
    ("am" "ざ") ("sm" "じ") ("gm" "ず") ("ddm" "ぜ") ("fm" "ぞ")
    ("zm" "だ") ("xm" "ぢ") ("bm" "づ") ("cm" "で") ("vm" "ど")
    ("dq" "ば") ("dw" "び") ("dt" "ぶ") ("de" "べ") ("dr" "ぼ")
    ("dz" "ぱ") ("dx" "ぴ") ("db" "ぷ") ("dc" "ぺ") ("dv" "ぽ")
    ("ky" "ぁ") ("ki" "ぃ") ("ku" "ぅ") ("kp" "ぇ") ("ko" "ぉ")
    ( "j" "っ") ("kh" "ゃ") ("kj" "ゅ") ( "l" "ょ") ("kn" "ゎ")
    ("um" "ヴ") ("qkm" "ヵ") ("ekm" "ヶ")
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
    ("/u" "◎") ("//u" "♪")
    ("/i" "∞") ("//i" "×")
    ("/o" "⇒") ("//o" "【")
    ("/p" "〒") ("//p" "】")
    ("/a" "々") ("//a" "仝")
    ("/s" "ヽ") ("//s" "ヾ")
    ("/d" "ゝ") ("//d" "ゞ")
    ("/f" "〃") ("//f" "￥")
    ("/g" "〆") ("//g" "§")
    ("/h" "←") ("//h" "‘")
    ("/j" "↓") ("//j" "“")
    ("/k" "↑") ("//k" "”")
    ("/l" "→") ("//l" "’")
    ("/;" "〜") ("//;" "∴")
    ("/z" "《") ("//z" "〈")
    ("/x" "》") ("//x" "〉")
    ("/c" "〇") ("//c" "℃")
    ("/v" "※") ("//v" "÷")
    ("/b" "≡") ("//b" "≒")
    ("/n" "≠") ("//n" "±")
    ("/m" "　") ("//m" "〓")
    ("/," "—") ("//," "≦")
    ("/." "…") ("//." "≧")
    ))

(defvar muji-yoko50-roman-pattern
  (muji-make-roman-pattern muji-yoko50-transliteration-rules))

(defcustom muji-yoko50-active-cursor-color "coral"
  "Cursor color when muji mode is active."
  :type 'color
  :group 'muji-yoko50)

(defun muji-delimiter () muji-yoko50-delimiter)
(defun muji-stop-chars () muji-yoko50-stop-chars)
(defun muji-phrase-separator () muji-yoko50-phrase-separator)
(defun muji-hiragana-nfer () muji-yoko50-hiragana-nfer)
(defun muji-katakana-nfer () muji-yoko50-katakana-nfer)
(defun muji-transliteration-rules () muji-yoko50-transliteration-rules)
(defun muji-roman-pattern () muji-yoko50-roman-pattern)
(defun muji-preprocess (string) string)
(defun muji-active-cursor-color () muji-yoko50-active-cursor-color)

;;; provide

(provide 'muji-yoko50)
;;; muji-yoko50.el ends here
