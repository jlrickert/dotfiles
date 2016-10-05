"""Install dotfiles to the system.

Will move anything that insnt a symlink into your tmp dir
"""
import sys
import os
import logging
from os.path import (
    realpath,
    dirname,
    expanduser
)

# makes sure python can find my custom libraries
sys.path.insert(0, os.path.join(dirname(realpath(__file__)),
                                'config', 'local', 'lib', 'python'))
from fileutils import (
    mark_as_exec,
    run_script,
    symlink
)

HOME = expanduser('~')
DOTFILES = dirname(realpath(__file__))

logging.basicConfig()
_log = logging.getLogger('installer')
_log.setLevel(logging.INFO)


def install_oh_my_zsh(home=HOME, dotfiles=DOTFILES, method=symlink):
    """Installs oh my zsh and my custom modules for it.
    """
    script = os.path.join(dotfiles, 'lib', 'install_oh_my_zsh.sh')
    oh_my_zsh_dir = os.path.join(home, '.oh-my-zsh')
    if not os.path.exists(oh_my_zsh_dir):
        run_script(script)

    my_theme = 'jlrickert.zsh-theme'
    my_theme_path = os.path.join(dotfiles, 'themes', my_theme)
    themes_dir = os.path.join(oh_my_zsh_dir, 'themes')
    theme_dst = os.path.join(themes_dir, my_theme)
    if not os.path.exists(theme_dst):
        method(my_theme_path, theme_dst)


def install_configs(home=HOME, dotfiles=DOTFILES, method=symlink):
    """Links and creates all needed directories based on the directory
    structure under config"""
    config_dir = os.path.join(dotfiles, 'config')

    def _install(root, path=''):
        for name in os.listdir(root):
            src = os.path.join(root, name)
            _path = os.path.join(path, name) if path else name
            if os.path.isfile(src):
                dst = os.path.join(home, '.{}'.format(_path))
                method(src, dst)
            elif os.path.isdir(src):
                _install(src, _path)

    _install(config_dir)


def install_i3_configs(home=HOME, dotfiles=DOTFILES):
    """Installs the proper i3 config based on the environment"""


def install_bin(home=HOME, dotfiles=DOTFILES, method=symlink):
    """Installs all in binary to a location"""
    bin_src = os.path.join(dotfiles, 'bin')
    bin_dir = os.path.join(home, '.local', 'bin')

    for name in os.listdir(bin_src):
        bin_file = os.path.join(bin_src, name)
        mark_as_exec(bin_file)
        dst = os.path.join(bin_dir, name)
        method(bin_file, dst)


def install_emacs(home=HOME, dotfiles=DOTFILES, method=symlink):
    """Installs my emacs configurations"""
    src = os.path.join(dotfiles, 'emacs.d')
    dst = os.path.join(home, '.emacs.d')
    method(src, dst)


def install_vundle(home=HOME, dotfiles=DOTFILES):
    """Installs vundle for vim"""


def install_pyenv(home=HOME, dotfiles=DOTFILES):
    """Installs pyenv"""

    script = os.path.join(dotfiles, "lib", "install_pyenv.sh")
    pyenv_dir = os.path.join(home, ".pyenv")
    if not os.path.exists(pyenv_dir):
        run_script(script)


def install_rbenv(home=HOME, dotfiles=DOTFILES):
    """Installs rbenv"""

    script = os.path.join(dotfiles, "lib", "install_rbenv.sh")
    rbenv_dir = os.path.join(home, ".pyenv")
    if not os.path.exists(rbenv_dir):
        run_script(script)


def install_everything(home=HOME, dotfiles=DOTFILES):
    """Installs everything."""
    install_oh_my_zsh(home, dotfiles)
    install_configs(home, dotfiles)
    install_i3_configs(home, dotfiles)
    install_bin(home, dotfiles)
    install_emacs(home, dotfiles)
    install_vundle(home, dotfiles)
    install_rbenv(home, dotfiles)
    install_pyenv(home, dotfiles)


if __name__ == '__main__':
    sys.exit(install_everything())
