# Shgif format definition

Shgif data is written in YAML.
**Different format from original([Cj-bc/shgif](https://github.com/Cj-bc/shgif))**

The code below describes all things about current shgif format

```yaml
title: Title of the shgif
author: Author of the shgif
format: Page # Currently only 'Page' work
width: 20 # width of your shgif
height: 3 # height of your shgif
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
