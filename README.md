# muji

## muji とは

muji は Emacs のためのモードレス日本語入力です。

muji には、日本語入力モードの概念がありません。
Alphabet やローマ字をべた打ちし、日本語にしたいところで `C-j` とタイプして変換します。

かな漢字変換には Emacs に標準添付の kkc.el を利用しています。
Emacs 内で完結しているので、他のソフトウェアを導入する必要がありません。

入力モードを切り替える手間がないので、和英交じり文を入力するのに適しているかもしれません。

kkc.el のかな漢字変換が賢くないのと、ローマ字の typo に気付きにくいのが弱みです。

## インストールとセットアップ

muji.el を `load-path` の通ったところに置いて init.el に次のように書きます。

``` emacs-lisp
(require 'muji)
(define-key global-map (kbd "C-x C-\\") 'global-muji-mode)
```

## 使用法

`C-x C-\` (`global-muji-mode`) で muji グローバルマイナーモードを起動します (モードラインに `MJ` と表示されます)。

ローマ字を入力して `C-j` (`muji-kkc`) と打つと、日本語に変換されます。

Alphabet の直後に日本語を入力するときは、間を半角空白で区切ります。
例: `Emacs kakuchou` → `Emacs 拡張`

和英間に空白を残したくない人は init.el に `(setq muji-remove-space t)` と書いてください。
変換後に空白が削除されます。

`C-j` の代わりに `C-u C-j` と打つことで、一時的に `muji-remove-space` を反転することができます。

和英間をセミコロン (`;`) で区切る方法もあります。
セミコロンは変換後に削除されます。
例: `Emacs;kakuchou` → `Emacs拡張`

区切り文字を変更するには init.el に `(setq muji-delimiter ":")` のように書いてください。

これと似た設定項目に `muji-stop-chars` (デフォルト値は `"(){}<>"`) があります。
`C-j` はカーソルの左のローマ字を変換しますが、この変数で指定された文字より左は変換対象としません。
例: `(paren)kakko` → `(paren)括弧`

なお `muji-remove-space`, `muji-delimiter`, `muji-stop-chars` の値にかかわらず、アクティブなリージョンがあれば、そのリージョンを変換します。
例: `C-SPC` `z(z)` `C-x C-x` `C-j` → `【】`

## ローマ字かな変換ルール

ローマ字かな変換ルールは Emacs の leim/quail/japanese.el 由来のものです。
ただし、全角の数字・記号は半角にしています (個人的な好み)。

「ん」は `n` または `n'` (`a i u e o y` が続く場合) で入力します。
例: `konna` → `こんな`; `ken'aku` → `けんあく`

`nn` で「ん」と入力するには init.el に `(setq muji-use-double-n t)` と書いてください。

| 入力  | かな   | 備考           |
|-------|--------|----------------|
| `tyi` | `てぃ` |                |
| `dyi` | `でぃ` |                |
| `dyu` | `どぅ` |                |
| `la`  | `ら`   | `l` は「ら行」 |
| `wi`  | `ゐ`   |                |
| `we`  | `ゑ`   |                |
| `xwi` | `うぃ` |                |
| `xwe` | `うぇ` |                |
| `xwo` | `うぉ` |                |
| `z/`  | `・`   | `/` は `/`     |

`zh zj zk zl` で `← ↓ ↑ →` のような変換もできます。

## かな漢字変換中のキー操作

kkc.el の変換中は、次のようなキー操作が可能です。

| キー           | 意味                                 |
|----------------|--------------------------------------|
| `C-n`, `SPC`   | 次の候補                             |
| `C-p`          | 前の候補                             |
| `l`            | 次の候補群                           |
| `L`            | 前の候補群                           |
| `0` .. `9`     | 候補群から選択                       |
| `H`            | ひらがなに変換                       |
| `K`            | かたかなに変換                       |
| `C-o`          | 文節を伸ばす                         |
| `C-i`, `TAB`   | 文節を縮める                         |
| `C-f`          | 文節を確定して次へ                   |
| `C-c`, `DEL`   | 変換をキャンセル                     |
| `C-m`, `RET`   | 変換を確定                           |
| `C-@`, `C-SPC` | 1 文字だけ確定                       |
| `C-h`          | ヘルプ                               |
| `O`            | 変換を変更せずに文節を伸ばす (?)     |
| `I`            | 文節を変更せずに変換範囲を縮める (?) |

連文節変換は可能ですが、単語単位や文節単位で入力・変換するほうが効率がよい印象です。

## Tips

isearch (`C-s`) 中に `C-j` は使えません。
ローマ字のまま日本語を検索できる [migemo.el](https://github.com/emacs-jp/migemo) を併用するとよいでしょう。
nonincremental search (`C-s RET`) では `C-j` が使えます。

~~kkc.el の辞書 leim/ja-dic/ja-dic.el は語彙が多くありません。~~
~~[emacs leimの日本語辞書ja-dic.elを改善する](http://maorigreen.html.xdomain.jp/memo_ja-dic_el.html) で入手できる [ja-dic.el](http://maorigreen.html.xdomain.jp/ja-dic.el) を利用する方法があります~~
~~(この場合 kkc.el のキャッシュファイル kkcrc をリネームするか退避しておく必要があるかと思います)。~~
この改善策は Emacs 29 以降不要になりました。
Emacs 29.1 の NEWS に `** Emacs no longer reduces the size of the Japanese dictionary.` との記述があります。

kkc.el の変換は賢くなくて、例えば「モードレス」と入力するのに `mo-doresu` `C-j` とタイプしても「モーどれす」となったりします。
そこで muji.el では、ローマ字の末尾に `k` を付けると、かたかなに変換確定する機能を入れました。
さきほどの例では `mo-doresuk` `C-j` で「モードレス」になります。
同様に `h` を付けると、ひらがなで確定します。

また、変数 `muji-a-la-mlh` (デフォルト値は `t`) が non-`nil` のとき、スラッシュ (`/`) で変換単位を明示した変換を行います。
上記の `h`/`k` と組み合せて使うと連文節の変換が効率よく行えます。
例: `mlh fuu/noh/mo-doresuk/henkan/housiki/desu.h` `C-j` → `mlh 風のモードレス変換方式です。`
