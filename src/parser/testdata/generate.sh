#!/usr/bin/env bash
set -euo pipefail

memeasm -fno-martyrdom -S -o alphabet.S alphabet.memeasm
