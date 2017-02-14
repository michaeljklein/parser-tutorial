#!/bin/bash
set -ex

stack haddock && yes | cp -rf .stack-work/install/*/*/*/doc/ .
