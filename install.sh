#!/bin/bash
set -eux
ocamlfind install "$@" vdoml lib/vdoml/*
