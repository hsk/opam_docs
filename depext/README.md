# depext

opam-depextはコマンドラインツールです。
depexts fieldのopam外部のライブラリのインストールをやってくれるようです。
現時点では、開発途中のようです。

## インストール

	$ opam install csv

## 使い方

```
$ opam-depext
# Detecting depexts using flags: x86_64 osx homebrew
# The following system packages are needed:
#  - pcre
#  - pkg-config
Warning: pkg-config-0.28 already installed
Error: pcre-8.35 already installed
To install this version, first `brew unlink pcre'
opam-depext: internal error, uncaught exception:
             Failure("OS package installation failed")
```

と,pcreがbrewでどうのと言われます。
             
```
$ brew unlink pcre
Unlinking /usr/local/Cellar/pcre/8.35... 133 symlinks removed
```

unlinkしてもう一度実行

```
$ opam-depext
# Detecting depexts using flags: x86_64 osx homebrew
# The following system packages are needed:
#  - pcre
#  - pkg-config
Warning: pkg-config-0.28 already installed
==> Downloading https://downloads.sf.net/project/machomebrew/Bottles/pcre-8.36.m
######################################################################## 100.0%
==> Pouring pcre-8.36.mavericks.bottle.tar.gz
🍺  /usr/local/Cellar/pcre/8.36: 146 files, 5.9M
# OS packages installation successful
```

brewを動かしてくれましたよ。

```
$ opam-depext
# Detecting depexts using flags: x86_64 osx homebrew
# The following system packages are needed:
#  - pcre
#  - pkg-config
Warning: pcre-8.36 already installed
Warning: pkg-config-0.28 already installed
# OS packages installation successful
```

もう一回。何やらインストールされたようです。


Makefile

```
default: install

install:
	opam install depext
uninstall:
	opam uninstall depext
```

## 参考URL

https://github.com/OCamlPro/opam-depext

