# Shgif フォーマット仕様

## 仕様書のバージョン

この仕様書で定義されているのは`1.0.0`です

## バージョニングについて

[Semantic versioning v2.0.0](https://semver.org/spec/v2.0.0.html)を使用しています。

## ライブラリ実装のバージョンについて

できる限り最新のバージョンが実装されますが、実際に実装されているバージョンは`Shgif.Type.Internal.version`で確認できます。

---

ShgifのデータはYAMLで保存されます。  
[Cj-bc/shgif](https://github.com/Cj-bc/shgif)とは全く別のフォーマットです。

以下に完全な例を載せます

```yaml
title: shgifのタイトル
author: shgifの著作者
format: Page # 現在は'Page'のみ
width: 20 # shgifの横幅
height: 3 # shgifの高さ
version: 1.0.0 # shgif formatバージョン
data: # shgifフレームのリスト
  - timestamp: 0 # timestampは0から開始する必要があります。
    # 'contents'の一行目は__アンカー__です。
    # これはYamlパーサにどの高さが行の始めなのかを伝える役割を果たします。
    # これはAAの一部ではないので、再生時には取り除かれます。
    # なんの文字を使っても大丈夫です。
    contents: |-
              anchor
              first line here
                spaces are ok
              also work
    # ある程度間を開けた方が良いです。
  - timestamp: 1000
    contents: |-
              a
              second frame
              hoge
              hoo
    # この最後のフレームがないと、前のフレームは1msのみ表示されます。
  - timestamp: 2000
    contents: |-
              anchor
              second frame
              hoge
              hoo
```

これは`stack run example/shgifs/example-in-shgif-format.yaml`で実行され、結果は以下のようになります。

[![asciicast](https://asciinema.org/a/288971.svg)](https://asciinema.org/a/288971)


## 大切なこと

- エスケープは必要ありません
  - バックスラッシュも含めて必要ありません

# Quick Tour: Shgifファイルを作成する

## 普通のGifアニメーションを作成する

### 1. AAのフレームを作成する

このAAは[example/shgifs/face_blinking.yaml](../example/shgifs/face_blinkig.yaml)にあります

```
\_____       _____/
 , |0| `-   -' |0| `
 ' \_/         \_/ '
          .

        -----
        \___/
```

```
                  ,
`------.   .------
' '-'  `   '  '-' '
         .

       -----
       \___/
```

```

`______     ______,
       `          '
         .

       -----
       \___/
```

```
                  ,
`------.   .------
' '-'  `   '  '-' '
         .

       -----
       \___/
```

```
                  ,
`------.   .------
' '-'  `   '  '-' '
         .

       -----
       \___/
```


### 2. メタデータを追加する

必要なメタデータは以下です

| メタデータ | 説明 |
| :-:|:-:|
| title  | Shgifのタイトル |
| author | Shgifの作者 |
| format | Shgifのフォーマット。_現在は'Page'のみ_ |
| width  | Shgifの横幅 |
| height | Shgifの高さ |
| version | 使用しているフォーマットのバージョン |


### 3. timestampを決める

`timestamp`は、そのフレームがいつ表示されるかを指定します。  
shgifViewerを使っている場合、単位はミリ秒です。  
(自分でプログラムを組む場合、調節が可能です)

_重要なこと_: timestampが0のフレームが一枚必要です。
              ない場合、最初には何も表示されません。


### 4. Yamlで書く

あとは以下のテンプレートを埋めるだけです:

```
title: <タイトル>
author: <作者名>
format: Page
width: <Shgifの横幅>
height: <Shgifの高さ>
version: <バージョン番号>
data:
  - timestamp: 0
    contents: |-
              anchor
              <timestamp 0のAA>
    ...
```

_<timestamp 0のAA>の前にあるanchor_を忘れないようにしてください。
すべてのフレームのAAの前に必要です。
