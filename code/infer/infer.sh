#!/bin/sh

lein typed infer-type infer.core :infer-opts "{:out-dir \"out\"}"
