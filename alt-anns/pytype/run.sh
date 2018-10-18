#!/bin/sh

pytype -V 2.7 plain.py
cp pytype_output/plain.pyi .
pytype -V 2.7 klass.py
cp pytype_output/klass.pyi .
