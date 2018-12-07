#!/usr/bin/env bash

sed -i "s/#\([0-9]\+\)/[#\1](https:\/\/github.com\/input-output-hk\/cardano-wallet\/issues\/\1)/g" $1
