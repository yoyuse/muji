;;; muji.el --- Modeless UX for Japanese Input       -*- lexical-binding: t; -*-

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

(defcustom muji-delimiter ";"
  "Delimiter between ASCII and roman strings."
  :type '(choice string (const nil))
  :group 'muji)

(defcustom muji-stop-chars "(){}<>"
  "String of characters that should not be icluded in roman string."
  :type '(choice string (const nil))
  :group 'muji)

(defcustom muji-a-la-mlh t
  "If non-nil, do conversion a la mlh."
  :type 'boolean
  :group 'muji)

;; (defcustom muji-a-la-mlh-separator "/"
;;   "Phrase separator used in `muji-a-la-mlh'."
;;   :type '(choice string (const nil))
;;   :group 'muji)

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

    ;; XXX: 個人的に数字・記号は半角が好み
    ;; ("1" "１") ("2" "２") ("3" "３") ("4" "４") ("5" "５")
    ;; ("6" "６") ("7" "７") ("8" "８") ("9" "９") ("0" "０")

    ;; XXX: 個人的に数字・記号は半角が好み
    ;; ("!" "！") ("@" "＠") ("#" "＃") ("$" "＄") ("%" "％")
    ;; ("^" "＾") ("&" "＆") ("*" "＊") ("(" "（") (")" "）")
    ;; ("-" "ー") ("=" "＝") ("`" "｀") ("\\" "￥") ("|" "｜")
    ;; ("_" "＿") ("+" "＋") ("~" "￣") ("[" "「") ("]" "」")
    ;; ("{" "｛") ("}" "｝") (":" "：") (";" "；") ("\""  "”")
    ;; ("'" "’") ("." "。") ("," "、") ("<" "＜") (">" "＞")
    ;; ("?" "？") ("/" "／")

    ;; "ー「」。、" は必要
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

    ;; "-" が "ー" になってしまって具合が悪い
    ;; ("zx" [":-"]) ("zX" [":-)"])
    ;; XXX: 全角引用符の救済策
    ("zx" "’") ("zX"  "”")

    ("zc" "〇") ("zC" "℃")
    ("zv" "※") ("zV" "÷")
    ("zb" "°") ("zB" "←")
    ("zn" "′") ("zN" "↓")
    ("zm" "″") ("zM" "〓")
    ("z," "‥") ("z<" "≦")
    ("z." "…") ("z>" "≧")
    ("z/" "・") ("z?" "∞")

    ;; ("\\\\" quail-japanese-self-insert-and-switch-to-alpha)
    ;; ("{{" quail-japanese-self-insert-and-switch-to-alpha)
    ;; ("}}" quail-japanese-self-insert-and-switch-to-alpha)

    ;; ("qq" quail-japanese-switch-package)
    ;; ("qz" quail-japanese-switch-package)
    ))

;;
(defvar muji-roman-pattern nil
  "Regexp that matches all roman patterns.")

(defun muji-make-roman-pattern (rules)
  "Make `muji-roman-pattern' from RULES."
  ;; Sort in long order so that a longer Romaji sequence precedes
  (let* ((rules (sort (mapcar (function (lambda (elt) (car elt))) rules)
                      (function (lambda (a b) (< (length b) (length a)))))))
    (setq muji-roman-pattern
          (mapconcat (function (lambda (elt) (regexp-quote elt)))
                     rules "\\|"))))

(muji-make-roman-pattern muji-transliteration-rules)

(defun muji-normalize-n (string)
  "Normalize spellings for \"ん\" to \"n'\"."
  (save-match-data
    (let* ((case-fold-search nil)
           (string (if muji-use-double-n (string-replace "nn" "n'" string)
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

(defun muji-roman-to-kana (string)
  "Convert roman string in STRING to kana and return it."
  (save-match-data
    (let* ((case-fold-search nil)
           (string (muji-normalize-n string))
           (string (muji-double-consonant-to-sokuon string))
           (pattern (concat "\\(" muji-roman-pattern "\\).*\\'")))
      (while (string-match pattern string)
        (let* ((match (match-string 1 string))
               (kana (cadr (assoc match muji-transliteration-rules)))
               (kana (if (vectorp kana) (aref kana 0) kana)))
          (setq string
                (replace-regexp-in-string pattern kana string nil nil 1))))
      string)))

(defun muji-backward-roman-to-kana (string)
  "Find roman string in STRING at its tail and return (roman . kana).
If no roman string found, return nil."
  (save-match-data
    (let* ((case-fold-search nil)
           (delimiter (or muji-delimiter ""))
           (stop-chars (or muji-stop-chars ""))
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

(defun muji-no-kkc (kana)
  "If kana ends with \"h\", return (t . hiragana).
Or if kana ends with \"k\", convert kana to katakana and return (t . katakana).
Or return (nil . kana)."
  (save-match-data
    (cond ((null kana) nil)
          ((string-match "\\(.+\\)h$" kana) (cons t (match-string 1 kana)))
          ((string-match "\\(.+\\)k$" kana) (cons t (japanese-katakana
                                                     (match-string 1 kana))))
          (t (cons nil kana)))))

(defun muji-kkc-region (beg end)
  "Convert the region between BEG and END with kkc."
  (let* ((roman (buffer-substring beg end))
         (kana (muji-roman-to-kana roman))
         ;;
         (no-kkc (muji-no-kkc kana))
         (kana (cdr no-kkc))
         (no-kkc (car no-kkc))
         ;;
         )
    (goto-char beg)
    (delete-region beg end)
    (insert kana)
    ;;
    ;; (kkc-region (- (point) (length kana)) (point))
    (when (not no-kkc) (kkc-region (- (point) (length kana)) (point)))
    ;;
    ))

(defun muji-kkc-n (n)
  "Convert last N characters string with kkc."
  (let* ((beg (- (point) n))
         (end (point)))
    (muji-kkc-region beg end)))

(defun muji-kkc-normal (&optional inverse-remove-space)
  "Convert the current word with kkc.
If INVERSE-REMOVE-SPACE is non-nil, inverse `muji-remove-space'."
  (let* ((muji-remove-space
          (if inverse-remove-space (not muji-remove-space) muji-remove-space))
         (line (buffer-substring-no-properties (point-at-bol) (point)))
         (roman-kana (muji-backward-roman-to-kana line))
         (roman (car roman-kana))
         (kana (cdr roman-kana))
         ;;
         (no-kkc (muji-no-kkc kana))
         (kana (cdr no-kkc))
         (no-kkc (car no-kkc))
         ;;
         beg)
    (when roman-kana
      (save-excursion
        (goto-char (- (point) (length roman)))
        (setq beg (point))
        (insert kana)
        (delete-region (point) (+ (point) (length roman))))
      ;;
      ;; (kkc-region (- (point) (length kana)) (point))
      (when (not no-kkc) (kkc-region (- (point) (length kana)) (point)))
      ;;
      (when muji-remove-space
        (undo-boundary)
        (save-excursion
          (goto-char beg)
          (let* ((str (buffer-substring-no-properties
                       (max (point-at-bol) (- (point) 2))
                       (point))))
            (when (string-match-p "\\w " str)
              (backward-delete-char 1))))))))

(defun muji-a-la-mlh (&optional inverse-remove-space)
  "Convert a la mlh."
  (interactive "P")
  (let* ((muji-remove-space
          (if inverse-remove-space (not muji-remove-space) muji-remove-space))
         (line (buffer-substring-no-properties (point-at-bol) (point)))
         (roman-kana (muji-backward-roman-to-kana line)))
    (when roman-kana
      (let* ((roman (car roman-kana))
             (kana (cdr roman-kana))
             ;; XXX: hard coding: slash
             (kana (replace-regexp-in-string "\\([hk/]?\\)/" "\\1 " kana))
             (kana (string-replace "/ " "/" kana))
             (parts (mapcar #'muji-no-kkc (split-string kana " " t)))
             (beg (progn (goto-char (- (point) (length roman)))
                         (point))))
        (save-excursion
          (dolist (part parts) (insert (cdr part)))
          (delete-region (point) (+ (point) (length roman))))
        (dolist (part parts)
          (let* ((no-kkc (car part))
                 (phrase (cdr part)))
            (cond (no-kkc (forward-char (length phrase)))
                  (t (kkc-region (point)
                                 (+ (point) (length phrase)))))))
        ;;
        (when muji-remove-space
          (undo-boundary)
          (save-excursion
            (goto-char beg)
            (let* ((str (buffer-substring-no-properties
                         (max (point-at-bol) (- (point) 2))
                         (point))))
              (when (string-match-p "\\w " str)
                (backward-delete-char 1)))))))))

;;;###autoload
(defun muji-kkc (&optional arg)
  "Convert with kkc.
If region is active, convert the region with `muji-kkc-region'.
Or if ARG is a number, convert last ARG characters string with `muji-kkc-n'.
Or convert the current word with `muji-kkc-normal',
in which case if ARG is non-nil, inverse `muji-remove-space'."
  (interactive "P")
  (cond ((use-region-p) (muji-kkc-region (region-beginning) (region-end)))
        ((numberp arg) (muji-kkc-n arg))
        (t (if muji-a-la-mlh (muji-a-la-mlh arg) (muji-kkc-normal arg)))))

;;;###autoload
(define-minor-mode
  muji-mode
  "Modeless UX for Japanese Input."
  :global nil
  :init-value nil
  :lighter " MJ"
  :keymap `((,muji-rK-trans-key . muji-kkc))
  )

;;;###autoload
(define-globalized-minor-mode
  global-muji-mode
  muji-mode
  muji-on
  )

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
  (set-cursor-color (if muji-mode muji-active-cursor-color
                      muji-inactive-cursor-color)))

(add-hook 'post-command-hook 'muji-set-cursor-color)

;;; minibuffer

(defun muji-minibuffer-setup ()
  "Activate muji-mode in minibuffer if the mode is ON in original buffer."
  ;; (when (with-minibuffer-selected-window muji-mode)
  ;;   (muji-mode))
  (muji-mode (if (with-minibuffer-selected-window muji-mode) 1 -1))
  )

(add-hook 'minibuffer-setup-hook 'muji-minibuffer-setup)

;;; provide

(provide 'muji)
;;; muji.el ends here
