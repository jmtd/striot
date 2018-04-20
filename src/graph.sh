#!/bin/bash
set -u
set -e

# generate a crude inter-dependency graph for .hs files

echo "digraph {"
for i in *hs Striot/*hs; do
  mod1=${i%.hs}
  for j in *hs Striot/*hs; do
      if [ "$i" = "$j" ]; then
        continue
      fi
      mod2=${j%.hs}
      if grep -q "${mod2/\//.}" "$i"; then
          echo "  ${mod1/\//_} -> ${mod2/\//_};"
      fi
  done
done
echo "}"
