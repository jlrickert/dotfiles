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
import fileutils as fu
from fileutils import (
    mark_as_exec,
    run_script,
    symlink
)

HOME = expanduser('~')
DOTFILES = dirname(realpath(__file__))

logger = logging.getLogger(__name__)
# logger.setLevel(logging.NOTSET)


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


def install_i3_configs(home=HOME, dotfiles=DOTFILES, method=symlink):
    """Installs the proper i3 config based on the environment"""
    core_src = os.path.join(dotfiles, "i3", "config")
    core_dst = os.path.join(home, ".config", "i3", "config")
    method(core_src, core_dst)

    msg = """
Please input your device type number:
1) desktop
2) labtop
3) server
Number: """

    dev_type = None
    while dev_type is None:
        try:
            dev_type = int(input(msg))
        except ValueError as e:
            print(e)
            print(msg)

    bar_src = None
    bar_dst = os.path.join(home, ".config", "i3status", "config")
    if dev_type == 1:  # desktop
        bar_src = os.path.join(dotfiles, "i3", "i3desktop-bar")
    elif dev_type == 2:  # labtop
        bar_src = os.path.join(dotfiles, "i3", "i3labtop-bar")
    elif dev_type == 3:  # server
        bar_src = None
        method = lambda x, y: None

    method(bar_src, bar_dst)


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

    try:
        log_level = os.environ["LOGGING"]
        if log_level == "CRITICAL":
            log_level = logging.CRITICAL
        elif log_level == "WARNING":
            log_level = logging.WARNING
        elif log_level == "DEBUG":
            log_level = logging.DEBUG
        elif log_level == "INFO":
            log_level = logging.INFO
        else:
            log_level = logging.NOTSET
        logging.basicConfig(level=log_level)
    except KeyError:
        pass

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
