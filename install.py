import os
import stat
import logging
from subprocess import call
from contextlib import contextmanager
from os.path import realpath, dirname, expanduser

log = logging.getLogger(__name__)


@contextmanager
def dry_run():
    """Disables and operations to the os"""
    log.debug('Starting dry run')
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
        log.info("Linking '{}' -> '{}'".format(src, dst))


def mark_as_exec(bin_name):
    """Marks a file as an executable.  Same as chmod +x BIN_NAME"""
    status = os.stat(bin_name)
    os.chmod(bin_name, status.st_mode | stat.S_IEXEC)


def ensure_available_path(path):
    """Make sure that a directory exists"""
    if not os.path.exists(path):
        log.info('Created directory {}'.format(path))
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


def install_oh_my_zsh(home_dir, dotfiles):
    """Installs oh my zsh and my custom modules for it.
    """
    oh_my_zsh_dir = os.path.join(home_dir, '.oh-my-zsh')
    if not os.path.exists(oh_my_zsh_dir):
        call(['sh', '-c',
              '"$(curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"'])

    my_theme = 'jlrickert.zsh-theme'
    my_theme_path = os.path.join(dotfiles, 'themes', my_theme)
    themes_dir = os.path.join(oh_my_zsh_dir, 'themes')
    theme_dst = os.path.join(themes_dir, my_theme)
    if not os.path.exists(theme_dst):
        symlink(my_theme_path, theme_dst)


def main(dotfiles=dirname(realpath(__file__)),
         home=expanduser('~')):
    """Installs everything."""
    config_dir = os.path.join(dotfiles, 'config')

    install_oh_my_zsh(home, dotfiles)
    install_configs(config_dir, home)

    bin_location = os.path.join(home, '.local', 'bin')
    install_bin(os.path.join(dotfiles, 'bin'), bin_location)

    emacs_config = os.path.join(dotfiles, 'emacs.d')
    install_emacs(emacs_config, home)

    install_vundle(home)


if __name__ == '__main__':
    import sys
    home = expanduser('~')
    dotfiles = dirname(realpath(__file__))
    sys.exit(main(home=home, dotfiles=dotfiles))
