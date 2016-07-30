import os
from contextlib import contextmanager
from os.path import realpath, dirname, expanduser


def symlink(src=None, dst=None):
    """Symlinks files"""
    assert src and dst
    ensure_available_path(dirname(dst))
    if not os.path.exists(dst):
        os.symlink(src, dst)
        print("Linking '{}' -> '{}'".format(src, dst))
    else:
        print("Skipping linking '{src}' -> '{dst}'.  Already exists".format(
            src=src, dst=dst))


def ensure_available_path(path):
    """Make sure that a directory exists"""
    if not os.path.exists(path):
        print('Created directory {}'.format(path))
        os.makedirs(path)


def install_configs(config_path, home, method=symlink):
    """Links and creates all needed directories based on the directory
    structure under config"""
    def _install(root, path=''):
        for name in os.listdir(root):
            src = os.path.join(root, name)
            if path:
                path_ = os.path.join(path, name)
            else:
                path_ = name
            if os.path.isfile(src):
                dst = os.path.join(home, '.{}'.format(path_))
                method(src, dst)
            elif os.path.isdir(src):
                _install(src, path_)

    _install(config_path)


def install_i3_configs():
    """Installs the proper i3 config based on the environment"""


def install_vundle():
    """Installs vundle for vim"""


def install_emacs():
    """Installs my emacs configurations"""


@contextmanager
def dry_run():
    """Disables and operations to the os"""
    print('dry_run')
    makedirs = os.makedirs
    symlink = os.symlink
    os.makedirs = lambda path: None
    os.symlink = lambda src, dst: None
    yield
    os.symlink = symlink
    os.makedirs = makedirs


def main():
    """Installs everything."""
    dotfiles = dirname(realpath(__file__))
    configs = os.path.join(dotfiles, 'config')
    home = expanduser('~')
    install_configs(configs, home)


if __name__ == '__main__':
    import sys
    sys.exit(main())
