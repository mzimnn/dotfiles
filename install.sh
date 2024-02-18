#!/bin/bash

# files which should not be symlinked
IGNORED_FILES=(
    install.sh
    README.org
)

is_file_ignored () {
    for ignored_file in "${IGNORED_FILES[@]}"
    do
        if [ "$ignored_file" = "$1" ]
        then
            return 0
        fi
    done

    return 1
}

base_dir="$(dirname "$(realpath "$0")")"

readarray -t files < <(git -C "$base_dir" ls-files)

for file in "${files[@]}"
do
    if is_file_ignored "$file"
    then
        continue
    fi

    source="$base_dir/$file"
    target="$HOME/$file"

    mkdir -p "$(dirname "$target")"

    if [ "$(readlink "$target")" = "$source" ]
    then
        # file is already symlinked
        continue
    fi

    ln -s -i "$source" "$target"
done
