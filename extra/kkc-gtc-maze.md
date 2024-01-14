# kkc-gtc-maze

## これは何?

[kkc-gtc](kkc-gtc.md) で交ぜ書き変換を行うためのパッケージです。

## 使用法

kkc-gtc-maze.el は以下のパッケージに依存しています。

- kkc-gtc.el (kkc で [Google CGI API for Japanese Input](https://www.google.co.jp/ime/cgiapi.html) を利用するためのパッケージ。 [yoyuse/muji](https://github.com/yoyuse/muji) に含まれています)
- kkc-maze.el (kkc で交ぜ書き変換を行うためのパッケージ。 [yoyuse/ttt](https://github.com/yoyuse/ttt) に含まれています)

これらの 3 つのファイルを `load-path` の通ったところに置いて init.el に次のように書きます。

kkc のキャッシュファイル kkcrc のバックアップを取っておくことをおすすめします。

``` emacs-lisp
(require 'kkc-gtc-maze)
(setq kkc-gtc-maze-enable-gtc-maze-p t)
```

初回の読み込みには時間がかかります。
これは kkc-maze.el が「単漢字 → 読み」の逆引き辞書 kkc-maze-init.el を作成する初期化処理のためです。

例えば `れい和` に対して `kkc-region` を行うと `令和` などに変換されます。
