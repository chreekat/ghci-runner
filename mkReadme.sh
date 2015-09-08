#!/bin/bash

echo -e "# ghci-runner\n" > README.md
awk 'NR>1 && !/^-}/ {print}; /^-}/ {exit}' src/Dev/Runner.hs \
    | sed -e 's/@/`/g' \
    >> README.md
