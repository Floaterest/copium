#!/bin/bash

run() {
    bat input.txt
	if [ -f "package.yaml" ]; then
		stack run
	elif [ -f "Cargo.toml" ]; then
		cargo run
	else
		echo 'No project found' 1>&2;
	fi < input.txt | bat --file-name output.txt
}

input=$1
while [[ "$input" != 'q' ]]; do
    case "$input" in
        q)
            exit 0 ;;
        r)
            run ;;
    esac
    printf '>>> '
    read input
done