# Shgif format definition

## Current version

This document define Shgif format `1.0.0`

## About versioning

Using [Semantic versioning v2.0.0](https://semver.org/spec/v2.0.0.html)

## About format version used in the library

I try to implement the latest format.
`Shgif.Type.Internal.version` specify which version is it.

---

Shgif data is written in YAML.
Different format from original([Cj-bc/shgif](https://github.com/Cj-bc/shgif))

The code below describes all things about current shgif format

```yaml
title: Title of the shgif
author: Author of the shgif
format: Page # Currently only 'Page' work
width: 20 # width of your shgif
height: 3 # height of your shgif
version: 1.0.0 # shgif format version
data: # list of shgif frame
  - timestamp: 0 # timestamp should be start from 0
    # The first line of 'contents' is __anchor__
    # This tells YAML parser where is the beginning of the line.
    # It's not a part of the AA, so that it'll be removed at play time
    # You can use any letter for anchor
    contents: |-
              anchor
              first line here
                spaces are ok
              also work
  - timestamp: 1000 # better to have some interval
    contents: |-
              a
              second frame
              hoge
              hoo
  - timestamp: 2000 # if this isn't exist, second frame will appear only one ms
    contents: |-
              anchor
              second frame
              hoge
              hoo
```

This example can be run by `stack run example/shgifs/example-in-shgif-format.yaml` and result in:

[![asciicast](https://asciinema.org/a/288971.svg)](https://asciinema.org/a/288971)


## Important things here

- No escaping is required
  - Even if backslash


# Quick Tour for making Shgif file

## Create Regular Gif animation

### 1. Create AA frames

Those AAs are from [example/shgifs/face_blinking.yaml](../example/shgifs/face_blinkig.yaml)

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

### 2. Add metadata

Required metadata are:

| metadata | description |
| :-:|:-:|
| title | title for this Shgif |
| author | Author of this Shgif |
| format | Shgif format. _Currently only 'Page' is available_ |
| width  | width of this Shgif |
| height | height of this Shgif |
| version | version number you use |


### 3. Define Timestamp

`timestamp` specify when the frame is rendered.  
The Unit is 'milliseconds' in case of shgifViewer.  
(You can adjust that value)

_important_: Frame with timestamp 0 should be exist.
             Otherwise, nothing will be rendered at the beginning.


### 4. Write into yaml

What you should do is fill the template below:

```
title: <your-title>
author: <author-name>
format: Page
width: <width-of-Shgif>
height: <height-of-Shgif>
version: <version-number>
data:
  - timestamp: 0
    contents: |-
              anchor
              <your AA for timestamp 0>
    ...
```

Please notice _anchor befor <your AA for timestamp 0>_.
It's required before each frame's AA.
