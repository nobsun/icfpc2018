# ICFPc2018

This is Team Sampou's repository for [ICFP Programming Contest 2018](https://icfpcontest2018.github.io/).

## To build/run solution

### Develop Environment

Debian GNU/Linux (amd64) (unstable)

### Installing build tool

haskell-stack https://docs.haskellstack.org/en/stable/README/

### Problem files

put problem files under /home/icfpc2018-data/problems/F/

### Build program

```
% stack update
% stack setup
% make install
```

### Generate solution traces

to generate naive traces

```
% mkdir output
% ./naive-traces output
```

to generate optimized traces

```
% ./naive-traces output inv
```

## Our approach

### FA and FR

1. generate naive traces.
2. check generated traces on simulator with flipping to Low harmonics for grounded state. (optimization)

### FD

1. generate assemble (not disassemble) naive traces.
2. check generated traces on simulator with flipping to Low harmonics for grounded state. (optimization)
3. invert generated assemble traces to disassemble traces.


## Members

* [Chikanobu Toyofuku](https://github.com/itto100pen)
* [Katsutoshi Ito](https://github.com/cutsea110)
* [Kei Hibino](https://github.com/khibino)
* [Masahiro Sakai](https://github.com/msakai)
* [Nobuo Yamashita](https://github.com/nobsun)
* [Yasuyuki Ogawa](https://github.com/oganet)
