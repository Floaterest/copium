import argparse
import json
import os
import shutil

CONVERT = {
    'codeforces.com': lambda parts: url[31:].replace('/problem', ''),
    'atcoder.jp': lambda parts: parts[-1],
}


def config() -> (str, str):
    with open(os.path.splitext(__file__)[0] + '.json', 'r') as f:
        return map(json.load(f).get, ('git', 'src'))


def split_url(url: str) -> str:
    """
    >>> get_domain('https://codeforces.com/contest/1598/problem/A')
    'codeforces.com'
    >>> get_domain('https://atcoder.jp/contests/abc066/tasks/abc066_b')
    'atcoder.jp'
    """
    return url.lower().removeprefix('https://').split('/')


def main(url: str, comment: str):
    git, src = config()
    parts = split_url(url)
    domain = parts[0]

    """
    if src is .rs:

    from https://atcoder.jp/contests/abc066/tasks/abc066_b
    to   path/to/repo/atcoder.jp/abc066/abc066_b.rs

    from https://codeforces.com/contest/1598/problem/A
    to   path/to/repo/codeforces.com/1598/a.rs
    """
    dest = os.path.join(git, *parts[::2]) + os.path.splitext(src)[1]
    action = 'update' if os.path.exists(dest) else 'add'

    # copy file
    os.makedirs(os.path.dirname(dest), exist_ok=True)
    print('Copy', src, '->', dest)
    shutil.copy(src, dest)

    # git
    os.chdir(git)
    os.system(f'git add {dest}')
    #                                                             contest     task letter
    os.system(f'git commit -m "{action}({domain.split(".")[0]}) {parts[-3]} {parts[-1][-1]} {comment}" -m "{url}"')


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('url', type=str)
    parser.add_argument('comment', nargs=argparse.ZERO_OR_MORE, default='')

    args = parser.parse_args()
    main(args.url.lower(), ' '.join(args.comment))
