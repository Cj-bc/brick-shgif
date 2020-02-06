English: [README.md](README.md)


# brick-shgif

[Cj-bc/shgif](https://github.com/Cj-bc/shgif)の発想を[jtdaugherty/brick](https://github.com/jtdaugherty/brick)上で実装したモジュールです。  
オリジナルの[Cj-bc/shgif](https://github.com/Cj-bc/shgif)とは**違うフォーマット**を使用しています。

# 例

### `stack run example/shgifs/face_blinking.yaml`

![face-blinking.gif](docs/img/face-blinking.gif)

# Build

`stack build`

# usage

1. データを`Shgif`フォーマット([docs/shgif-format.md](docs/shgif-format.md))で書きます
2. `stack run <shgif-data-file>`


# ライブラリとして使うには

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
```

## 自分のAppで`shgif`ウィジェットを使う

`shgif`ウィジェットを使うには、以下の手順が必要です:

1. `Data.Yaml.decode*`を使ってshgifファイルを読み、`Shgif`型のデータを生成する
2. 一定時間毎に呼ばれるEvent(Tick)を作成する(特に何も使っていない場合、`TickEvent`が使用できます)
3. (もし`TickEvent`を使うならば) `App s e n`の`e`を`TickEvent`にする
4. `2.`で設定したイベントが呼ばれたときに`updateShgif`を`Shgif`データに適用する(これでshgifのtickを増やします)
5. 描画部分で、`shgif` widgetに`Shgif`データを渡す


### 1. shgifファイルを読む

始めに、`Shgif`型のデータをファイルから読み込む必要があります。  
`decodeFileEither`や`decodeFileThrow`などの`Data.Yaml`のデコーダーを使ってください。  
個人的には`decodeFileEither`を使用しています。  

```haskell
import Data.Yaml (decodeFileEither, ParseException)

main = do
  sgf <- decodeFileEither "filename" :: IO (Either ParseException Shgif)
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


### 4. `updateShgif`を呼ぶ

`updateShgif`は、フレーム描画に使用する`Shgif`内部のTickカウンターを更新します。
`updateShgif`の型は`Shgif -> IO Shgif`なので、`EventM`モナド内から使うことができます。

```haskell
-- eHandler is used `appHandleEvent` for `App`
eHandler s (AppEvent Tick) = do
    let oldsgf = ... -- 現在のShgifをsから取り出す
    newsgf <- liftIO (updateShgif oldsgf)
    continue $ ... -- Shgifを新しい物に取り替えてcontinue
```


### 5. `shgif` widgetを使う

他のwidgetと同様に使うことができます。

```haskell
ui s = [border $ shgif sgf]
  where
    sgf = ... -- get Shgif data from s
```
