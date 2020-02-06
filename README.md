日本語: [README-JA](README-JA.md)


# brick-shgif

Implement [Cj-bc/shgif](https://github.com/Cj-bc/shgif) on [jtdaugherty/brick](https://github.com/jtdaugherty/brick)  
The shgif **format is changed** so that doesn't support original [Cj-bc/shgif](https://github.com/Cj-bc/shgif) format.

# Examples

### `stack run example/shgifs/face_blinking.yaml`

![face-blinking.gif](docs/img/face-blinking.gif)


# Build


`stack build` to build


# usage

1. write data by using `shgif` format (document on [docs/shgif-format.md](docs/shgif-format.md))
2. execute `stack run <file>`


# How to use as Library

## Modules which are exposed

Currently, this provides those modules:

```yaml
Brick:
  Extensions:
    Shgif:
      - Events
      - Widgets
Shgif:
  - Type
```

## Use shgif widget in your Brick app

To use `shgif` Widget, you should do:

1. Read shgif file by using `Data.Yaml.decode*`, and make `Shgif` data
2. Create `customMain` that adds some event that will be called by fixed time(if nothing you have, use `TickEvent`)
3. Use `TickEvent` as `e` of `App s e n` (if you choose to use it)
4. Call `updateShgif` each time `2.` event occurs (This will update shgif's internal tick counter)
5. Call `shgif` widget with `Shgif` object


### 1. Read shgif file

Firstly, you should get `Shgif` data from files.  
To do so, you need to use `Data.Yaml`'s decoders, like `decodeFileEither`/`decodeFileThrow`.
I use `decodeFileEither` personally.

```haskell
import Data.Yaml (decodeFileEither, ParseException)

main = do
  sgf <- decodeFileEither "filename" :: IO (Either ParseException Shgif)
```


### 2. Create `customMain` that adds tick event

The tick event tell Shgif how much time has been left, and which frame to render.  
If you already have some custom event that'll do the same job, you can use them.  
If not, you can use `TickEvent` found in `Brick.Extensions.Shgif.Events`  
If you don't need to do other jobs in main, you can use `mainWithTick` function to automatically
adding `TickEvent` to your app.


```haskell
import Data.Yaml (decodeFileEither, ParseException)
import Brick.Extensions.Shgif.Events (mainWithTick)
import Brick (App)

data Name = SomeName -- you should define n of `App s e n`

app :: App s TickEvent Name
app = App ...

main = do
  ...
  sgf <- decodeFileEither "filename" :: IO (Either ParseException Shgif)
  sgf' <- -- do some job to get inner `Shgif`

  finalSate <- mainWithTick Nothing 1000 app sgf'
  ...
```


### 4. Call `updateShgif`

The function will update `Shgif`'s internal tick state, which will affect frame rendering.  
As `updateShgif` has type `Shgif -> IO Shgif`, it can be called inside brick's `EventM` monad.

```haskell
-- eHandler is used `appHandleEvent` for `App`
eHandler s (AppEvent Tick) = do
    let oldsgf = ... -- get old Shgif data
    newsgf <- liftIO (updateShgif oldsgf)
    continue $ ... -- continue with replacing oldsgf with newsgf
```


### 5. Use `shgif` widget

You can use it the same way as other widgets

```haskell
ui s = [border $ shgif sgf]
  where
    sgf = ... -- get Shgif data from s
```
