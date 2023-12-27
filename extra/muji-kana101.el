;;; muji-kana101.el --- JIS kana input for muji      -*- lexical-binding: t; -*-

;; Copyright (C) 2023  YUSE Yosihiro

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
;; (require 'muji-kana101)
;; (define-key global-map (kbd "C-x C-\\") 'global-muji-mode)

;;; Code:

(require 'muji)

;; (defgroup muji-kana101 nil
;;   "JIS kana input for muji."
;;   :group 'muji
;;   :prefix "muji-kana101-")

(setq muji-delimiter ":")

(setq muji-stop-chars nil)

(setq muji-transliteration-rules
      '(( "3" "あ") ( "e" "い") ( "4" "う") ( "5" "え") ( "6" "お")
        ( "t" "か") ( "g" "き") ( "h" "く") ( "'" "け") ( "b" "こ")
        ( "x" "さ") ( "d" "し") ( "r" "す") ( "p" "せ") ( "c" "そ")
        ( "q" "た") ( "a" "ち") ( "z" "つ") ( "w" "て") ( "s" "と")
        ( "u" "な") ( "i" "に") ( "1" "ぬ") ( "," "ね") ( "k" "の")
        ( "f" "は") ( "v" "ひ") ( "2" "ふ") ( "=" "へ") ( "-" "ほ")
        ( "j" "ま") ( "n" "み") ("\\" "む") ( "/" "め") ( "m" "も")
        ( "7" "や")             ( "8" "ゆ")             ( "9" "よ")
        ( "o" "ら") ( "l" "り") ( "." "る") ( ";" "れ") ( "`" "ろ")
        ( "0" "わ")                                     ( ")" "を")
        ( "y" "ん")
        ("t[" "が") ("g[" "ぎ") ("h[" "ぐ") ("'[" "げ") ("b[" "ご")
        ("x[" "ざ") ("d[" "じ") ("r[" "ず") ("p[" "ぜ") ("c[" "ぞ")
        ("q[" "だ") ("a[" "ぢ") ("z[" "づ") ("w[" "で") ("s[" "ど")
        ("f[" "ば") ("v[" "び") ("2[" "ぶ") ("=[" "べ") ("-[" "ぼ")
        ("f]" "ぱ") ("v]" "ぴ") ("2]" "ぷ") ("=]" "ぺ") ("-]" "ぽ")
        ( "#" "ぁ") ( "E" "ぃ") ( "$" "ぅ") ( "%" "ぇ") ( "^" "ぉ")
        ( "Z" "っ") ( "&" "ゃ") ( "*" "ゅ") ( "(" "ょ")
        ( "_" "ー") ( "{" "「") ( "}" "」") ( "<" "、") ( ">" "。")
        ( "?" "・") ( "[" "゛") ( "]" "゜")

        ("S1" "○") ("S!" "●")
        ("S2" "▽") ("S@" "▼")
        ("S3" "△") ("S#" "▲")
        ("S4" "□") ("S$" "■")
        ("S5" "◇") ("S%" "◆")
        ("S6" "☆") ("S^" "★")
        ("S7" "◎") ("S&" "£")
        ("S8" "¢") ("S*" "×")
        ("S9" "♂") ("S(" "【")
        ("S0" "♀") ("S)" "】")
        ("S-" "〜") ("S_" "∴")
        ("S=" "≠") ("S+" "±")
        ("S\\" "＼") ("S|" "‖")
        ("S`" "´") ("S~" "¨")

        ("Sq" "《") ("SQ" "〈")
        ("Sw" "》") ("SW" "〉")
        ("Sr" "々") ("SR" "仝")
        ("St" "〆") ("ST" "§")
        ("Sp" "〒") ("SP" "↑")
        ("S[" "『") ("S{" "〔")
        ("S]" "』") ("S}" "〕")

        ("Ss" "ヽ") ("SS" "ヾ")
        ("Sd" "ゝ") ("SD" "ゞ")
        ("Sf" "〃") ("SF" "→")
        ("Sg" "‐") ("SG" "—")
        ("Sh" "←")
        ("Sj" "↓")
        ("Sk" "↑")
        ("Sl" "→")
        ("S;" "゛") ("S:" "゜")
        ("S'" "‘") ("S\"" "“")

        ;; "-" が "ー" になってしまって具合が悪い
        ;; ("Sx" [":-"]) ("SX" [":-)"])
        ;; XXX: 全角引用符の救済策
        ("Sx" "’") ("SX"  "”")

        ("Sc" "〇") ("SC" "℃")
        ("Sv" "※") ("SV" "÷")
        ("Sb" "°") ("SB" "←")
        ("Sn" "′") ("SN" "↓")
        ("Sm" "″") ("SM" "〓")
        ("S," "‥") ("S<" "≦")
        ("S." "…") ("S>" "≧")
        ("S/" "・") ("S?" "∞")
        ))

(setq muji-roman-pattern (muji-make-roman-pattern muji-transliteration-rules))

(defun muji-normalize-n (string)
  string)

(defun muji-double-consonant-to-sokuon (string)
  string)

(setq muji-active-cursor-color "coral")

;;; provide

(provide 'muji-kana101)
;;; muji-kana101.el ends here
