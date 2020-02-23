日本語: [README-JA](README-JA.md)


# brick-shgif

Implement [Cj-bc/shgif](https://github.com/Cj-bc/shgif) on [jtdaugherty/brick](https://github.com/jtdaugherty/brick)  
The shgif __format is changed__ so that doesn't support original [Cj-bc/shgif](https://github.com/Cj-bc/shgif) format.  
I write quick tour here, but for detailed informations for each module,
please refer to haddock(you can generate haddock files by doing `stack haddock`).

# Examples

You can find some code examples under [example/codes/](example/codes/).  
Example Shgif files are under [example/shgifs/](example/shgifs/).


### `stack run example/shgifs/face_blinking.yaml`

![face-blinking.gif](docs/img/face-blinking.gif)


# Build & install shgif viewer

```sh
# only build
$ stack build
# execute viewer without installing
$ stack run
# install binary `shgifViewer` to `~/.local/bin`
$ stack install
```

# usage

1. write data by using `shgif` format (document on [docs/shgif-format.md](docs/shgif-format.md))
2. execute `stack run <file>`


# Quick Tour: How to use as Library

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
  - Loader
  - Updater
```

## Use shgif widget in your Brick app

To use `shgif` Widget, you should do:

1. Load `Shgif` by __Loader__
2. Create `customMain` that adds some event that will be called by fixed time(if nothing you have, use `TickEvent`)
3. Use `TickEvent` as `e` of `App s e n` (if you choose to use it)
4. Call __Updater__ each time `2.` event occurs (This will update shgif's internal tick counter)
5. Call `shgif` widget with `Shgif` object


### 1. Use `Loader` to load from file/Other data

Firstly, you should load `Shgif` data from files or other data.  
`Shgif.Loader` module provides lots of ways to do this.

```haskell
import Data.Yaml (ParseException)
import Shgif.Loader (fromFile)

main = do
  sgf <- fromFile "filename" :: IO (Either ParseException Shgif)
```


### 2. Create `customMain` that adds tick event

The tick event tell Shgif how much time has been left, and which frame to render.  
If you already have some custom event that'll do the same job, you can use them.  
If not, you can use `TickEvent` found in `Brick.Extensions.Shgif.Events`  
If you don't need to do other jobs in main, you can use `mainWithTick` function to automatically
adding `TickEvent` to your app.


```haskell
import Brick.Extensions.Shgif.Events (mainWithTick)
import Brick (App)
import Brick.Loader (fromFile)
import Data.Yaml (ParseException)

data Name = SomeName -- you should define n of `App s e n`

app :: App s TickEvent Name
app = App ...

main = do
  ...
  sgf <- fromFile "filename"
  sgf' <- -- do some job to get inner `Shgif`

  finalSate <- mainWithTick Nothing 1000 app sgf'
  ...
```


### 4. Call __Updater__

The __Updater__ function in `Shgif.Updater` module will update `Shgif`'s internal tick state,
which will affect frame rendering.  
As all of them has type `Shgif -> IO Shgif`, it can be called inside brick's `EventM` monad.

```haskell
-- eHandler is used `appHandleEvent` for `App`
eHandler s (AppEvent Tick) = do
    let oldsgf = ... -- get old Shgif data
    newsgf <- liftIO (updateShgif oldsgf)
    continue $ ... -- continue with replacing oldsgf with newsgf
```

#### Looping and Reversing

Those functions below will control looping and reversing

| `function` | reversed | looped |
|:-:|:-:|:-:|
| `updateShgif` | No  | Yes |
| `updateShgifNoLoop` | No  | No |
| `updateShgifReversed` | Yes  | Yes |
| `updateShgifReversedNoLoop` | Yes  | No |

#### Control more flexibly

The function `updateShgifTo` will update internal tick to make it closer to given number.  
If internal tick is equal to the given number, it won't change anything.


### 5. Use `shgif` widget

It's in `Brick.Extensions.Widget.Shgif`.  
You can use it the same way as other widgets

```haskell
ui s = [border $ shgif sgf]
  where
    sgf = ... -- get Shgif data from s
```
