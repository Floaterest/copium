import os
import json
import argparse
from codecs import open

COMMENT = '// %s\n'
CONVERT = {
    'codeforces.com': lambda parts: url[31:].replace('/problem', ''),
    'atcoder.jp': lambda parts: parts[-1],
}


def split_url(url: str) -> str:
    """
    >>> get_domain('https://codeforces.com/contest/1598/problem/A')
    'codeforces.com'
    >>> get_domain('https://atcoder.jp/contests/abc066/tasks/abc066_b')
    'atcoder.jp'
    """
    return url.lower().removeprefix('https://').split('/')


def write(src:str, dest:str, url:str):
    with open(src, 'r', 'utf8') as fsrc, open(dest, 'w', 'utf8') as fdest:
        fdest.write(COMMENT % url)
        fdest.write(fsrc.read())


def main(src:str, url: str, comment: str):
    parts = split_url(url)
    domain = parts[0]

    """
    if src is .rs:

    from https://atcoder.jp/contests/abc066/tasks/abc066_b
    to   path/to/repo/atcoder.jp/abc066/abc066_b.rs

    from https://codeforces.com/contest/1598/problem/A
    to   path/to/repo/codeforces.com/1598/a.rs
    """
    dest = os.path.join(*parts[::2]) + os.path.splitext(src)[1]
    if os.path.exists(dest):
        action, copy = 'update', 'Override'
    else:
        action, copy = 'add', 'Copy'

    # copy file
    os.makedirs(os.path.dirname(dest), exist_ok=True)
    print(copy, src, '->', dest)
    write(src, dest, url)

    # git
    os.system(f'git add {dest}')
    #                                                             contest     task letter
    os.system(f'git commit -m "{action}({domain.split(".")[0]}) {parts[-3]} {parts[-1][-1]} {comment}" -m "{url}"')


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('src', type=str)
    parser.add_argument('url', type=str)
    parser.add_argument('comment', nargs=argparse.ZERO_OR_MORE, default='')

    args = parser.parse_args()
    main(args.src, args.url.lower(), ' '.join(args.comment))
