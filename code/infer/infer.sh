#!/bin/sh

lein typed infer-type infer.core :infer-opts "{:fuel 0 :debug :all :out-dir \"out\" :preserve-unknown true}"
