#!/bin/sh

cp plain.ts plain-out.ts
typewiz-node plain-out.ts

cp plain-deep.ts plain-deep-out.ts
typewiz-node plain-deep-out.ts

cp plain-deep-balanced.ts plain-deep-balanced-out.ts
typewiz-node plain-deep-balanced-out.ts

cp klass.ts klass-out.ts
typewiz-node klass-out.ts

cp klass-deep.ts klass-deep-out.ts
typewiz-node klass-deep-out.ts

cp klass-left.ts klass-left-out.ts
typewiz-node klass-left-out.ts
