#!/usr/bin/env bash
# [[file:../../Desktop.org::*pomm][pomm:1]]
if ps -e | grep emacs >> /dev/null; then
    emacsclient --eval "(if (boundp 'pomm-current-mode-line-string) pomm-current-mode-line-string \"\") " | xargs echo -e
fi
# pomm:1 ends here
