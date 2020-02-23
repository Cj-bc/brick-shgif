English: [README.md](README.md)


# brick-shgif

[Cj-bc/shgif](https://github.com/Cj-bc/shgif)の発想を[jtdaugherty/brick](https://github.com/jtdaugherty/brick)上で実装したモジュールです。  
オリジナルの[Cj-bc/shgif](https://github.com/Cj-bc/shgif)とは__違うフォーマット__を使用しています。  
全体の流れをここに載せますが、それぞれの使い方について詳しくはhaddockを参照してください(`stack haddock`で生成できます。)

# 例

### `stack run example/shgifs/face_blinking.yaml`

コードのサンプルは[example/codes/](example/codes/)以下に、
Shgifのサンプルは[example/shgifs/](example/shgifs/)以下にあります。

![face-blinking.gif](docs/img/face-blinking.gif)

# shgifViewerのビルドとインストール

```sh
# ビルドのみ
$ stack build
# インストールせずにviewerを使う
$ stack run
# `shgifViewer`バイナリーを`~/.local/bin`にインストールする
$ stack install
```

# usage

1. データを`Shgif`フォーマット([docs/JA_shgif-format.md](docs/JA_shgif-format.md))で書きます
2. `stack run <shgif-data-file>`


# Quick Tour: ライブラリとして使うには

## 追加されるモジュール群

現状追加されるモジュール群は以下の通りです。

```yaml
Brick:
  Extensions:
    Shgif:
      - Events
      - Widgets
Shgif:
  - Type
  - Loader
  - Updater
```

``

## 自分のAppで`shgif`ウィジェットを使う

`shgif`ウィジェットを使うには、以下の手順が必要です:

1. __Loader__を使って`Shgif`のデータを読み込みます
2. 一定時間毎に呼ばれるEvent(Tick)を作成する(特に何も使っていない場合、`TickEvent`が使用できます)
3. (もし`TickEvent`を使うならば) `App s e n`の`e`を`TickEvent`にする
4. `2.`で設定したイベントが呼ばれたときに__Updater__を`Shgif`データに適用する(これで`Shgif`のtickを操作します)
5. 描画部分で、`shgif` widgetに`Shgif`データを渡す


### 1. `Loader`を使ってshgifファイルを読む

始めに、`Shgif`型のデータをファイルや他のデータ型から読み込む必要があります。  
`Shgif.Loader`モジュールに様々な`Loader`があります。

```haskell
import Data.Yaml (ParseException)
import Shgif.Loader (fromFile)

main = do
  sgf <- fromFile "filename" :: IO (Either ParseException Shgif)
```


### 2. Tickイベントを走らせる

Tickイベントによって`Shgif`に経過時間を知らせ、描画フレームを更新させます。  
もし、既にTickとして使えるイベントがある場合、そのイベントを使うことができます。  
もしなければ、`Brick.Extensions.Shgif.Events.TickEvent`を使うことができます。  
他に`customMain`に対してすることがない場合(tickイベントを足さなければ`defaultMain`を使える場合)、
自動的に`TickEvent`を追加してくれる`Brick.Extensions.Shgif.Events.mainWithTick`を使うことができます。

```haskell
import Data.Yaml (decodeFileEither, ParseException)
import Brick.Extensions.Shgif.Events (mainWithTick)
import Brick (App)

data Name = SomeName -- `App s e n`のn

app :: App s TickEvent Name
app = App ...

main = do
  ...
  sgf <- decodeFileEither "filename" :: IO (Either ParseException Shgif)
  sgf' <- -- 内部に包まれている`Shgif`を取得します

  finalSate <- mainWithTick Nothing 1000 app sgf'
  ...
```


### 4. __Updater__を呼ぶ

`Shgif.Updater`内にある__Updater__は、フレーム描画に使用する`Shgif`内部のTickカウンターを更新します。  
型は`Shgif -> IO Shgif`なので、`EventM`モナド内から使うことができます。

```haskell
-- eHandler is used `appHandleEvent` for `App`
eHandler s (AppEvent Tick) = do
    let oldsgf = ... -- 現在のShgifをsから取り出す
    newsgf <- liftIO (updateShgif oldsgf)
    continue $ ... -- Shgifを新しい物に取り替えてcontinue
```

#### ループと反転

以下の関数は、ループと反転再生を操作します。

| `function` | 反転再生 | ループ |
|:-:|:-:|:-:|
| `updateShgif` | No  | Yes |
| `updateShgifNoLoop` | No  | No |
| `updateShgifReversed` | Yes  | Yes |
| `updateShgifReversedNoLoop` | Yes  | No |

#### 表示するフレームを操作する

`updateShgifTo`は引数として目的のtick数を受け取り、そのtickに近づくようにshgif内部のtickを更新します。  
もしも内部のtickと引数の数値が等しかった場合は何も更新しません。


### 5. `shgif` widgetを使う

`Brick.Extensions.Widget.Shgif`にあります。
他のwidgetと同様に使うことができます。

```haskell
ui s = [border $ shgif sgf]
  where
    sgf = ... -- get Shgif data from s
```
