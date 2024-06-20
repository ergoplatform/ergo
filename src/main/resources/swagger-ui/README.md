## Introduction
Ergo uses a fork of swagger-ui with support for large integers. This repository can be found [here](https://github.com/SethDusek/swagger-ui).

You can update this swagger-ui file by first pulling from upstream in the swagger-ui repository. Then in the root of ergo project run the following:

``` shell
$ rsync -av --existing ../swagger-ui/dist/ src/main/resources/swagger-ui/
```
