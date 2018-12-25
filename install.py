#!/usr/bin/env python3

import os
from os import path
from os.path import (realpath, dirname, expanduser)
from typing import List, Tuple

FilePath = str


def _split_directories(fp: FilePath) -> List[FilePath]:
    return [part for part in fp.split(path.sep) if part]


def _find_dotfiles_links(
        home: FilePath,
        dotfiles_dir: FilePath,
        _path: FilePath = "",
) -> List[Tuple[FilePath, FilePath]]:
    files: List[Tuple[FilePath, FilePath]] = []
    for fp in os.listdir(path.join(dotfiles_dir, _path)):
        fp = path.join(_path, fp)
        if path.isdir(realpath(os.path.join(dotfiles_dir, fp))):
            files += _find_dotfiles_links(home, dotfiles_dir, fp)
        else:
            src = path.join(dotfiles_dir, fp)
            dst = path.join(home, "." + fp)
            files.append((src, dst))

    return files


def symlink(src: FilePath, dst: FilePath, force=True):
    mkdir(dirname(dst))
    if force and path.exists(dst):
        os.remove(dst)
    print("Symlinking", src, "to", dst)
    os.link(src, dst)


def mkdir(dp: FilePath):
    _path = path.sep
    for parts in _split_directories(dp):
        _path = path.join(_path, parts)
        if not path.exists(_path):
            os.mkdir(_path)


def install_dotfiles(home_dir: FilePath,
                     dotfiles_dir: FilePath,
                     method=symlink):
    for (src, dst) in _find_dotfiles_links(home_dir, dotfiles_dir):
        method(src, dst)


if __name__ == "__main__":
    home_dir = expanduser("~")
    dotfiles_dir = path.join(dirname(realpath(__file__)), "dotfiles")
    install_dotfiles(home_dir, dotfiles_dir)
