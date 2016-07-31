import os
import stat
from contextlib import contextmanager
from os.path import realpath, dirname, expanduser


@contextmanager
def dry_run():
    """Disables and operations to the os"""
    print('dry_run')
    _makedirs = os.makedirs
    _symlink = os.symlink
    os.makedirs = lambda path: None
    os.symlink = lambda src, dst: None
    yield
    os.makedirs = _makedirs
    os.symlink = _symlink


def symlink(src=None, dst=None):
    """Symlinks files"""
    assert src and dst
    ensure_available_path(dirname(dst))
    if not os.path.exists(dst):
        os.symlink(src, dst)
        print("Linking '{}' -> '{}'".format(src, dst))


def mark_as_exec(bin_name):
    """Marks a file as an executable.  Same as chmod +x BIN_NAME"""
    status = os.stat(bin_name)
    os.chmod(bin_name, status.st_mode | stat.S_IEXEC)


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


def install_bin(src_dir, dst_dir, method=symlink):
    """Installs all in binary to a location"""
    for name in os.listdir(src_dir):
        bin_file = os.path.join(src_dir, name)
        mark_as_exec(bin_file)
        dst = os.path.join(dst_dir, name)
        method(bin_file, dst)


def install_i3_configs():
    """Installs the proper i3 config based on the environment"""


def install_vundle(home):
    """Installs vundle for vim"""


def install_emacs(emacs_config, home, method=symlink):
    """Installs my emacs configurations"""
    dst = os.path.join(home, '.emacs.d')
    method(emacs_config, dst)


def main():
    """Installs everything."""
    dotfiles = dirname(realpath(__file__))
    configs = os.path.join(dotfiles, 'config')
    home = expanduser('~')

    install_configs(configs, home)

    bin_location = os.path.join(home, '.local', 'bin')
    install_bin(os.path.join(dotfiles, 'bin'), bin_location)

    emacs_config = os.path.join(dotfiles, 'emacs.d')
    install_emacs(emacs_config, home)

    install_vundle(home)


if __name__ == '__main__':
    import sys
    sys.exit(main())
