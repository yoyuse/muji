# muji-gtc

## これは何?

kkc の代わりに [Google CGI API for Japanese Input](https://www.google.co.jp/ime/cgiapi.html) を利用してかな漢字変換を行うパッケージです。
要は、かな漢字変換を自前で行わずに Google のサーバーに丸投げします。
各文節の変換や文節の区切りのたびにサーバーと通信を行う富豪的アプローチです。

素の kkc に比べると、変換がずっと賢い印象です。

当然ながらネットワーク接続が必要です。
**外部 (google.com) との通信が発生しますのでご注意ください。**

## 使用法

muji-gtc.el を `load-path` の通ったところにおいて init.el に次のように書きます。

``` emacs-lisp
(require 'muji)
(require 'muji-gtc)
(setq muji-gtc-enable-gtc-p t)
(define-key global-map (kbd "C-x C-\\") 'global-muji-mode)
```

`C-x C-\` で muji グローバルマイナーモードを起動し `C-j` で変換する点は同じです。
念のため kkc の キャッシュファイル kkcrc のバックアップを取っておくことをおすすめします。

## 補足

kkc の内部関数 (`kkc-lookup-key` の中の `skkdic-lookup-key`) を一時的に置き換える方法で実装しています。
kkc のキー操作 (文節の区切り直し、ひらがな変換、かたかな変換、確定など) はそのまま使えます。

kkc のキャッシュファイル kkcrc は使わず (見ない、触らない)、
変換結果の学習はしません。

文節区切りの `h;`/`k;`/`;` は muji-gtc でも使えます。
逆に `;` などで明示的に区切らないと、すべての文節を区切らない、いわば「全文節変換」になります。
この場合でも `C-i` や `C-o` で区切り直すことは可能です。
