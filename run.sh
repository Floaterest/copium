#!/bin/bash

# set copy and paste command
case "$XDG_SESSION_TYPE" in
    x11)
        xc() { xclip -selection clipboard; }
        xo() { xclip -selection clipboard -o; };;
    wayland)
        xc() { wl-copy; }
        xo() { wl-paste -t 'text/plain;charset=utf-8'; };;
    *) echo "Error: Unsupported session type '$XDG_SESSION_TYPE'." ;;
esac

# detect project (Rust or Haskell) and source code file
if [ -f "package.yaml" ]; then
    run() { stack run; }
    code='src/Main.hs'
elif [ -f "Cargo.toml" ]; then
    run() { cargo run; }
    code='src/main.rs'
else
    echo 'No project found' 1>&2;
fi

input=$1
while [[ "$input" != 'q' ]]; do
    case "$input" in
        q) # quit
            exit 0 ;;
        r) # run with input.txt
            bat input.txt
            run < input.txt | bat --file-name output.txt ;;
        c) # copy code to clipboard
            xc < $code
            echo "Copied $code to clipboard" ;;
        p) # paste input from clipboard
            xo > input.txt
            bat input.txt ;;
        t) # run with input from clipboard
            xo > input.txt
            bat input.txt
            run < input.txt | bat --file-name output.txt ;;
    esac
    printf '>>> '
    read input
done
