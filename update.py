#!/usr/bin/python3

import os
import argparse
from codecs import open

COMMENT = '// %s\n'


def convert(url: str):
    parts = url.removeprefix('https://').removesuffix('/').split('/')
    domain = parts[0]
    match parts:
        case ['codeforces.com', _, contest, _, task]:
            return [domain, contest, f'{contest}_{task}']
        case ['atcoder.jp', _, contest, _, task]:
            return [domain, contest, task]
        case ['leetcode.com', _, problem]:
            return [domain, '', problem]
        case _:
            exit('Unknown url: ' + '/'.join(parts))


def write(src: str, dest: str, url: str):
    with open(src, 'r', 'utf8') as fi, open(dest, 'w', 'utf8') as fo:
        fo.write(COMMENT % url)
        fo.write(fi.read())


def main(src: str, url: str, comment: str):
    domain, contest, task = convert(url)
    dest = os.path.join(domain, contest, task) + os.path.splitext(src)[1]
    if os.path.exists(dest):
        act, copy = 'update', '\033[91mOverride\033[00m'
    else:
        act, copy = 'add', '\033[92mCopy\033[00m'

    # copy file
    os.makedirs(os.path.dirname(dest), exist_ok=True)
    print(copy, src, '-->', dest)
    write(src, dest, url)

    # git
    os.system(f'git add {dest}')
    domain = domain.split(".")[0]
    cmd = f'git commit -m "{act}({domain}) {contest} {task} {comment}" -m "{url}"'
    os.system(cmd)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('src', type=str, help='path to submission code')
    parser.add_argument('url', type=str, help='task url')
    parser.add_argument('comment', nargs='*', default='', help='comments')

    args = parser.parse_args()
    main(args.src, args.url.lower(), ' '.join(args.comment))
