#!/bin/sh

# nodes (naive)
#lein typed infer-type infer.nodes :infer-opts "{:fuel 0 :debug :all :out-dir \"out\" :preserve-unknown true}"
# nodes (squashLocal)
#lein typed infer-type infer.nodes :infer-opts "{:fuel 1 :debug :all :out-dir \"out\" :preserve-unknown true}"
# nodes (squashGlobal 1: remove most names)
#lein typed infer-type infer.nodes :infer-opts "{:fuel 2 :debug :all :out-dir \"out\" :preserve-unknown true}"
# nodes (squashGlobal 2: unique names for each HMap)
#lein typed infer-type infer.nodes :infer-opts "{:debug #{:iterations :squash-vertically} :out-dir \"out\" :preserve-unknown true}"

# visit-leaf
lein typed infer-type infer.visit-leaf :infer-opts "{:debug #{:iterations :squash-vertically} :out-dir \"out\" :preserve-unknown true}"
