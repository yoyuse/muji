;;; muji.el --- Modeless UX for Japanese Input       -*- lexical-binding: t; -*-

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

;;; Usage:

;; - Enter global-muji-mode with C-x C-\
;; - Write Japanese in Roman letters and type C-j

;;; Code:

(require 'japan-util)
(require 'kkc)

(defgroup muji nil
  "Modeless UX for Japanese Input."
  :group 'mule
  :prefix "muji-")

(defcustom muji-rK-trans-key (kbd "C-j")
  "Conversion key."
  :type 'key-sequence
  :group 'muji)

(defcustom muji-remove-space nil
  "Non-nil if a space between ASCII and Japanese strings should be removed."
  :type 'boolean
  :group 'muji)

(defcustom muji-remove-space-regexp "[a-zA-Z0-9/]"
  "Space after string ending with the regexp is removed when `muji-remove-space'"
  :type 'regexp
  :group 'muji)

(defcustom muji-delimiter ":"
  "Delimiter between ASCII and roman strings."
  :type '(choice string (const nil))
  :group 'muji)

(defcustom muji-stop-chars "(){}<>"
  "String of characters that should not be icluded in roman string."
  :type '(choice string (const nil))
  :group 'muji)

(defcustom muji-phrase-separator ";"
  "Phrase separator used in `muji-kkc-phrases'."
  :type '(choice string (const nil))
  :group 'muji)

(defcustom muji-hiragana-nfer "h"
  "Hiragana conversion suffix."
  :type 'string
  :group 'muji)

(defcustom muji-katakana-nfer "k"
  "Katakana conversion suffix."
  :type 'string
  :group 'muji)

(defcustom muji-comma-period nil
  "If nil, use \"、\" and \"。\". If t, use \"，\" and \"．\".
If cons of strings, use its car and cdr."
  :type '(choice boolean (cons string string))
  :group 'muji)

;; cf. quail-japanese-use-double-n
(defcustom muji-use-double-n nil
  "If non-nil, use type \"nn\" to insert ん."
  :type 'boolean
  :group 'muji)

;; cf. quail-japanese-transliteration-rules
(defvar muji-transliteration-rules
  '(( "a" "あ") ( "i" "い") ( "u" "う") ( "e" "え") ( "o" "お")
    ("ka" "か") ("ki" "き") ("ku" "く") ("ke" "け") ("ko" "こ")
    ("sa" "さ") ("si" "し") ("su" "す") ("se" "せ") ("so" "そ")
    ("ta" "た") ("ti" "ち") ("tu" "つ") ("te" "て") ("to" "と")
    ("na" "な") ("ni" "に") ("nu" "ぬ") ("ne" "ね") ("no" "の")
    ("ha" "は") ("hi" "ひ") ("hu" "ふ") ("he" "へ") ("ho" "ほ")
    ("ma" "ま") ("mi" "み") ("mu" "む") ("me" "め") ("mo" "も")
    ("ya" "や")             ("yu" "ゆ")             ("yo" "よ")
    ("ra" "ら") ("ri" "り") ("ru" "る") ("re" "れ") ("ro" "ろ")
    ("la" "ら") ("li" "り") ("lu" "る") ("le" "れ") ("lo" "ろ")
    ("wa" "わ") ("wi" "ゐ") ("wu" "う") ("we" "ゑ") ("wo" "を")
    ("n'" "ん")
    ("ga" "が") ("gi" "ぎ") ("gu" "ぐ") ("ge" "げ") ("go" "ご")
    ("za" "ざ") ("zi" "じ") ("zu" "ず") ("ze" "ぜ") ("zo" "ぞ")
    ("da" "だ") ("di" "ぢ") ("du" "づ") ("de" "で") ("do" "ど")
    ("ba" "ば") ("bi" "び") ("bu" "ぶ") ("be" "べ") ("bo" "ぼ")
    ("pa" "ぱ") ("pi" "ぴ") ("pu" "ぷ") ("pe" "ぺ") ("po" "ぽ")

    ("kya" ["きゃ"]) ("kyu" ["きゅ"]) ("kye" ["きぇ"]) ("kyo" ["きょ"])
    ("sya" ["しゃ"]) ("syu" ["しゅ"]) ("sye" ["しぇ"]) ("syo" ["しょ"])
    ("sha" ["しゃ"]) ("shu" ["しゅ"]) ("she" ["しぇ"]) ("sho" ["しょ"])
    ("cha" ["ちゃ"]) ("chu" ["ちゅ"]) ("che" ["ちぇ"]) ("cho" ["ちょ"])
    ("tya" ["ちゃ"]) ("tyu" ["ちゅ"]) ("tye" ["ちぇ"]) ("tyo" ["ちょ"])
    ("nya" ["にゃ"]) ("nyu" ["にゅ"]) ("nye" ["にぇ"]) ("nyo" ["にょ"])
    ("hya" ["ひゃ"]) ("hyu" ["ひゅ"]) ("hye" ["ひぇ"]) ("hyo" ["ひょ"])
    ("mya" ["みゃ"]) ("myu" ["みゅ"]) ("mye" ["みぇ"]) ("myo" ["みょ"])
    ("rya" ["りゃ"]) ("ryu" ["りゅ"]) ("rye" ["りぇ"]) ("ryo" ["りょ"])
    ("lya" ["りゃ"]) ("lyu" ["りゅ"]) ("lye" ["りぇ"]) ("lyo" ["りょ"])
    ("gya" ["ぎゃ"]) ("gyu" ["ぎゅ"]) ("gye" ["ぎぇ"]) ("gyo" ["ぎょ"])
    ("zya" ["じゃ"]) ("zyu" ["じゅ"]) ("zye" ["じぇ"]) ("zyo" ["じょ"])
    ("jya" ["じゃ"]) ("jyu" ["じゅ"]) ("jye" ["じぇ"]) ("jyo" ["じょ"])
    ( "ja" ["じゃ"]) ( "ju" ["じゅ"]) ( "je" ["じぇ"]) ( "jo" ["じょ"])
    ("bya" ["びゃ"]) ("byu" ["びゅ"]) ("bye" ["びぇ"]) ("byo" ["びょ"])
    ("pya" ["ぴゃ"]) ("pyu" ["ぴゅ"]) ("pye" ["ぴぇ"]) ("pyo" ["ぴょ"])

    ("kwa" ["くゎ"]) ("kwi" ["くぃ"]) ("kwe" ["くぇ"]) ("kwo" ["くぉ"])
    ("tsa" ["つぁ"]) ("tsi" ["つぃ"]) ("tse" ["つぇ"]) ("tso" ["つぉ"])
    ( "fa" ["ふぁ"]) ( "fi" ["ふぃ"]) ( "fe" ["ふぇ"]) ( "fo" ["ふぉ"])
    ("gwa" ["ぐゎ"]) ("gwi" ["ぐぃ"]) ("gwe" ["ぐぇ"]) ("gwo" ["ぐぉ"])

    ("dyi" ["でぃ"]) ("dyu" ["どぅ"]) ("dye" ["でぇ"]) ("dyo" ["どぉ"])
    ("xwi" ["うぃ"])                  ("xwe" ["うぇ"]) ("xwo" ["うぉ"])

    ("shi" "し") ("tyi" ["てぃ"]) ("chi" "ち") ("tsu" "つ") ("ji" "じ")
    ("fu"  "ふ")
    ("ye" ["いぇ"])

    ("va" ["ヴぁ"]) ("vi" ["ヴぃ"]) ("vu" "ヴ") ("ve" ["ヴぇ"]) ("vo" ["ヴぉ"])

    ("xa"  "ぁ") ("xi"  "ぃ") ("xu"  "ぅ") ("xe"  "ぇ") ("xo"  "ぉ")
    ("xtu" "っ") ("xya" "ゃ") ("xyu" "ゅ") ("xyo" "ょ") ("xwa" "ゎ")
    ("xka" "ヵ") ("xke" "ヶ")

    ;; ("1" "１") ("2" "２") ("3" "３") ("4" "４") ("5" "５")
    ;; ("6" "６") ("7" "７") ("8" "８") ("9" "９") ("0" "０")

    ;; ("!" "！") ("@" "＠") ("#" "＃") ("$" "＄") ("%" "％")
    ;; ("^" "＾") ("&" "＆") ("*" "＊") ("(" "（") (")" "）")
    ;; ("-" "ー") ("=" "＝") ("`" "｀") ("\\" "￥") ("|" "｜")
    ;; ("_" "＿") ("+" "＋") ("~" "￣") ("[" "「") ("]" "」")
    ;; ("{" "｛") ("}" "｝") (":" "：") (";" "；") ("\""  "”")
    ;; ("'" "’") ("." "。") ("," "、") ("<" "＜") (">" "＞")
    ;; ("?" "？") ("/" "／")

    ("-" "ー") ("[" "「") ("]" "」") ("." "。") ("," "、")

    ("z1" "○") ("z!" "●")
    ("z2" "▽") ("z@" "▼")
    ("z3" "△") ("z#" "▲")
    ("z4" "□") ("z$" "■")
    ("z5" "◇") ("z%" "◆")
    ("z6" "☆") ("z^" "★")
    ("z7" "◎") ("z&" "£")
    ("z8" "¢") ("z*" "×")
    ("z9" "♂") ("z(" "【")
    ("z0" "♀") ("z)" "】")
    ("z-" "〜") ("z_" "∴")
    ("z=" "≠") ("z+" "±")
    ("z\\" "＼") ("z|" "‖")
    ("z`" "´") ("z~" "¨")

    ("zq" "《") ("zQ" "〈")
    ("zw" "》") ("zW" "〉")
    ("zr" "々") ("zR" "仝")
    ("zt" "〆") ("zT" "§")
    ("zp" "〒") ("zP" "↑")
    ("z[" "『") ("z{" "〔")
    ("z]" "』") ("z}" "〕")

    ("zs" "ヽ") ("zS" "ヾ")
    ("zd" "ゝ") ("zD" "ゞ")
    ("zf" "〃") ("zF" "→")
    ("zg" "‐") ("zG" "—")
    ("zh" "←")
    ("zj" "↓")
    ("zk" "↑")
    ("zl" "→")
    ("z;" "゛") ("z:" "゜")
    ("z'" "‘") ("z\"" "“")

    ("zx" "’") ("zX"  "”")
    ("zc" "〇") ("zC" "℃")
    ("zv" "※") ("zV" "÷")
    ("zb" "°") ("zB" "←")
    ("zn" "′") ("zN" "↓")
    ("zm" "″") ("zM" "〓")
    ("z," "‥") ("z<" "≦")
    ("z." "…") ("z>" "≧")
    ("z/" "・") ("z?" "∞")
    ))

(defun muji-make-roman-pattern (rules)
  "Make `muji-roman-pattern' from RULES."
  ;; Sort in long order so that a longer Romaji sequence precedes
  (let* ((rules (sort (mapcar #'(lambda (elt) (car elt)) rules)
                      #'(lambda (a b) (< (length b) (length a))))))
    (mapconcat #'(lambda (elt) (regexp-quote elt)) rules "\\|")))

(defvar muji-roman-pattern (muji-make-roman-pattern muji-transliteration-rules)
  "Regexp that matches all roman patterns.")

(defun muji-delimiter () muji-delimiter)
(defun muji-stop-chars () muji-stop-chars)
(defun muji-phrase-separator () muji-phrase-separator)
(defun muji-hiragana-nfer () muji-hiragana-nfer)
(defun muji-katakana-nfer () muji-katakana-nfer)
(defun muji-transliteration-rules () muji-transliteration-rules)
(defun muji-roman-pattern () muji-roman-pattern)
(defun muji-active-cursor-color () muji-active-cursor-color)

;; Emacs 27 does not have `string-replace'
(defun muji-string-replace (from-string to-string in-string)
  (save-match-data
    (replace-regexp-in-string
     (regexp-quote from-string) to-string in-string t t)))

(defun muji-normalize-n (string)
  "Normalize spellings for \"ん\" to \"n'\"."
  (save-match-data
    (let* ((case-fold-search nil)
           (string (if muji-use-double-n (muji-string-replace "nn" "n'" string)
                     string))
           ;; XXX: hard coding
           (pattern "n\\([^'aiueoy]\\|$\\)"))
      (replace-regexp-in-string pattern "n'\\1" string))))

(defun muji-double-consonant-to-sokuon (string)
  "Replace double consonats in STRING e.g. \"kk\" to \"っk\" and return it."
  (save-match-data
    (let* ((case-fold-search nil)
           ;; XXX: hard coding
           (pattern "\\([bcdfghjklmpqrstvwxyz]\\)\\1"))
      (replace-regexp-in-string pattern "っ\\1" string))))

(defun muji-preprocess (string)
  "Preprocess STRING and return it."
  (muji-double-consonant-to-sokuon (muji-normalize-n string)))

(defun muji-punctuation (string)
  "Convert punctuations in STRING according to `muji-comma-period'."
  (cond ((eq t muji-comma-period)
         (muji-string-replace "。" "．" (muji-string-replace "、" "，" string)))
        ((consp muji-comma-period)
         (muji-string-replace "。" (cdr muji-comma-period)
                              (muji-string-replace "、" (car muji-comma-period)
                                                   string)))
        (t string)))

(defun muji-postprocess (string)
  "Postprocess STRING and return it."
  (muji-punctuation string))

(defun muji-roman-to-kana (string)
  "Convert roman string in STRING to kana and return it."
  (save-match-data
    (let* ((case-fold-search nil)
           (string (muji-preprocess string))
           (pattern (concat "\\(" (muji-roman-pattern) "\\).*\\'")))
      (while (string-match pattern string)
        (let* ((match (match-string 1 string))
               (kana (cadr (assoc match (muji-transliteration-rules))))
               (kana (if (vectorp kana) (aref kana 0) kana)))
          (setq string
                (replace-regexp-in-string pattern kana string nil nil 1))))
      (muji-postprocess string))))

(defun muji-backward-roman-to-kana (string)
  "Find roman string in STRING at its tail and return (roman . kana).
If no roman string found, return nil."
  (save-match-data
    (let* ((case-fold-search nil)
           (delimiter (or (muji-delimiter) ""))
           (stop-chars (or (muji-stop-chars) ""))
           (pattern
            (concat
             "\\(" (regexp-quote delimiter) "\\)?" "\\("
             (regexp-opt-charset
              (cl-remove-if
               (lambda (c)
                 (memq c (string-to-list (concat delimiter stop-chars))))
               (number-sequence ?! ?~)))
             "+\\)$")))
      (if (not (string-match pattern string))
          nil
        (let* ((delimiter (match-string 1 string))
               (roman (match-string 2 string))
               (kana (muji-roman-to-kana roman))
               (roman (if delimiter (concat delimiter roman) roman)))
          (cons roman kana))))))

(defun muji-nfer (kana)
  "If KANA ends with `muji-hiragana-nfer', return (t . hiragana).
Or if KANA ends with `muji-katakana-nfer', convert hiragana to katakana
and return (t . katakana). Or return (nil . kana)."
  (save-match-data
    (let* ((case-fold-search t)
           (re-hira (regexp-quote (muji-hiragana-nfer)))
           (re-kata (regexp-quote (muji-katakana-nfer))))
      (cond ((null kana) nil)
            ((string-match (concat "\\(.+\\)" re-hira "$") kana)
             (cons t (match-string 1 kana)))
            ((string-match (concat "\\(.+\\)" re-kata "$") kana)
             (cons t (japanese-katakana (match-string 1 kana))))
            (t (cons nil kana))))))

(defun muji-kkc-region (beg end)
  "Convert the region between BEG and END with kkc."
  (let* ((roman (buffer-substring beg end))
         (kana (muji-roman-to-kana roman))
         (parts (muji-split-phrases kana)))
    (deactivate-mark)
    (goto-char beg)
    (save-excursion
      (delete-region beg end)
      (dolist (part parts) (insert (cdr part))))
    ;; XXX: code copy
    (dolist (part parts)
      (let* ((nfer (car part))
             (phrase (cdr part)))
        (cond (nfer (forward-char (length phrase)))
              (t (kkc-region (point) (+ (point) (length phrase)))))))))

(defun muji-kkc-n (n)
  "Convert last N characters string with kkc."
  (let* ((beg (- (point) n))
         (end (point)))
    (muji-kkc-region beg end)))

(defun muji-split-phrases (kana)
  "Split string KANA into phrase list by `muji-phrase-separator'.
E.g. \"へんかん;するh\" → ((nil . \"へんかん\") (t . \"する\"))."
  (if (null (muji-phrase-separator))
      (list (cons nil kana))
    (let* ((sep (muji-phrase-separator))
           (hira (muji-hiragana-nfer))
           (kata (muji-katakana-nfer))
           (re1 (regexp-opt-charset (string-to-list (concat hira kata sep))))
           (re1 (concat "\\(" re1 "?\\)" (regexp-quote sep)))
           (kana (replace-regexp-in-string re1 "\\1\0" kana))
           (kana (muji-string-replace (concat sep "\0") sep kana))
           (re2 (regexp-opt-charset (string-to-list (concat hira kata))))
           (re2 (concat "\\(" re2 "\\)\0?"))
           (kana (replace-regexp-in-string re2 "\\1\0" kana)))
      (mapcar #'muji-nfer (split-string kana "\0" t)))))

(defun muji-kkc-phrases (&optional inverse-remove-space)
  "Convert the current phrases with kkc.
If INVERSE-REMOVE-SPACE is non-nil, inverse `muji-remove-space'."
  (let* ((muji-remove-space
          (if inverse-remove-space (not muji-remove-space) muji-remove-space))
         (line (buffer-substring-no-properties (point-at-bol) (point)))
         (roman-kana (muji-backward-roman-to-kana line)))
    (when roman-kana
      (let* ((case-fold-search t)
             (roman (car roman-kana))
             (kana (cdr roman-kana))
             (parts (muji-split-phrases kana))
             (beg (progn (goto-char (- (point) (length roman))) (point))))
        (save-excursion
          (dolist (part parts) (insert (cdr part)))
          (delete-region (point) (+ (point) (length roman))))
        ;; XXX: code copy
        (dolist (part parts)
          (let* ((nfer (car part))
                 (phrase (cdr part)))
            (cond (nfer (forward-char (length phrase)))
                  (t (kkc-region (point) (+ (point) (length phrase)))))))
        (when muji-remove-space
          (undo-boundary)
          (save-excursion
            (goto-char beg)
            (let* ((str (buffer-substring-no-properties
                         (max (point-at-bol) (- (point) 2))
                         (point))))
              (when (string-match-p (concat muji-remove-space-regexp " ") str)
                (backward-delete-char 1)))))))))

;;;###autoload
(defun muji-kkc (&optional arg)
  "Convert with kkc.
If region is active, convert the region with `muji-kkc-region'.
Or if ARG is a number, convert last ARG characters string with `muji-kkc-n'.
Or convert the current phrases with `muji-kkc-phrases',
in which case if ARG is non-nil, inverse `muji-remove-space'."
  (interactive "P")
  (cond ((use-region-p) (muji-kkc-region (region-beginning) (region-end)))
        ((numberp arg) (muji-kkc-n arg))
        (t (muji-kkc-phrases arg))))

;;;###autoload
(define-minor-mode
  muji-mode
  "Modeless UX for Japanese Input."
  :global nil
  :init-value nil
  :lighter " Muji"
  :keymap `((,muji-rK-trans-key . muji-kkc)))

;;;###autoload
(define-globalized-minor-mode
  global-muji-mode
  muji-mode
  muji-on)

(defun muji-on ()
  (muji-mode 1))

;;; cursor color

(defcustom muji-inactive-cursor-color
  (cdr (assq 'cursor-color (frame-parameters (selected-frame))))
  "Cursor color when muji mode is inactive."
  :type 'color :group 'muji)

(defcustom muji-active-cursor-color "SpringGreen3"
  "Cursor color when muji mode is active."
  :type 'color :group 'muji)

(defun muji-set-cursor-color ()
  "Set cursor color according to `muji-mode'."
  (when (muji-active-cursor-color)
    (set-cursor-color
     (if muji-mode (muji-active-cursor-color) muji-inactive-cursor-color))))

(add-hook 'post-command-hook 'muji-set-cursor-color)

;;; minibuffer

(defun muji-minibuffer-setup ()
  "Activate muji-mode in minibuffer if the mode is ON in original buffer."
  (muji-mode (if (with-minibuffer-selected-window muji-mode) 1 -1)))

(add-hook 'minibuffer-setup-hook 'muji-minibuffer-setup)

;;; provide

(provide 'muji)
;;; muji.el ends here
