"""Install dotfiles to the system.

Will move anything that is not a symlink into a tmp dir
"""
from os.path import basename, dirname, expanduser, realpath
from subprocess import call
import logging
import os
import random
import shutil
import socket  # this is for getting the hostname of the device
import stat
import string
import sys
import errno

HOME = expanduser('~')
DOTFILES = dirname(realpath(__file__))

logger = logging.getLogger(__name__)


def symlink(src=None, dst=None):
    """Symlinks files"""
    mkdir(dirname(dst))
    logger.info("Linking '{}' -> '{}'".format(src, dst))
    try:
        os.symlink(src, dst)
    except OSError as e:
        if e.errno == errno.EEXIST:
            os.remove(dst)
            os.symlink(src, dst)


def rm(src, backup=True):
    """Removes a file and places it into a backup directory."""
    if not os.path.exists(src):
        pass
    elif os.path.islink(src):
        os.remove(src)
        logger.debug('Removing symlink {}'.format(src))
    elif backup:
        tmp_dir = os.environ.get('TMPDIR', '/tmp')
        rand_id = ''.join(random.SystemRandom().choice(string.ascii_uppercase +
                                                       string.digits)
                          for _ in range(5))
        backup_file = rand_id + "-" + basename(src)
        shutil.move(src, os.path.join(tmp_dir, backup_file))
        logger.debug('Backup up {} to {}'.format(src, backup_file))
    else:
        os.remove(src)
        logger.debug('Removing {}'.format(src))


def mark_as_exec(bin_name):
    """Marks a file as an executable.  Same as chmod +x BIN_NAME"""
    status = os.stat(bin_name)
    os.chmod(bin_name, status.st_mode | stat.S_IEXEC)


def mkdir(path):
    """Make sure that a directory exists"""
    if not os.path.exists(path):
        logger.debug('Creating directory {}'.format(path))
        os.makedirs(path)


def run_script(exe, *args):
    """Calls a script with the given args."""
    logger.debug("running {} {}".format(exe, " ".join(args)))
    call(["sh", exe] + list(args))


def setup_shell(home=HOME, dotfiles=DOTFILES, method=symlink):
    """Setup everything related to zsh"""
    zshrc_file = os.path.join(dotfiles, "shell", "zshrc")
    zshrc_dst = os.path.join(home, ".zshrc")
    method(zshrc_file, zshrc_dst)

    script = os.path.join(dotfiles, 'lib', 'install_oh_my_zsh.sh')
    oh_my_zsh_dir = os.path.join(home, '.oh-my-zsh')
    if not os.path.exists(oh_my_zsh_dir):
        run_script(script)


def setup_vundle(home=HOME, dotfiles=DOTFILES):
    """Installs vundle for vim"""
    script = os.path.join(dotfiles, 'lib', 'install_vundle.sh')
    vundle_dir = os.path.join(home, ".vim", "bundle", "Vundle.vim")
    if not os.path.exists(vundle_dir):
        run_script(script)
    else:
        logger.debug("SKIPPING :: {} already exists".format(vundle_dir))


def setup_spacemacs(home=HOME, dotfiles=DOTFILES, method=symlink):
    """Installs my emacs configurations"""
    script = os.path.join(dotfiles, 'lib', 'install_spacemacs.sh')
    emacs_dir = os.path.join(home, '.emacs.d')
    if not os.path.exists(emacs_dir):
        run_script(script)
    else:
        logger.debug("SKIPPING :: {} already exists".format(emacs_dir))


def setup_config(home=HOME, dotfiles=DOTFILES, method=symlink):
    """Links and creates all needed directories based on the directory structure
    under config.

    """
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


def setup_i3_configs(home=HOME, dotfiles=DOTFILES, method=symlink):
    """Installs the proper i3 config based on the environment"""
    core_src = os.path.join(dotfiles, "i3", "config")
    core_dst = os.path.join(home, ".config", "i3", "config")
    method(core_src, core_dst)
    hostname = socket.gethostname()

    status_bar_src = None
    bar_dst = os.path.join(home, ".config", "i3status", "config.toml")
    if hostname == "myelin":
        status_bar_src = os.path.join(dotfiles, "i3", "i3desktop-bar.toml")
    elif hostname == "cortex":
        status_bar_src = os.path.join(dotfiles, "i3", "i3labtop-bar.toml")
    else:
        status_bar_src = None

        def method(x, y):
            return None

    method(status_bar_src, bar_dst)


def setup_vscode_extensions(home=HOME, dotfiles=DOTFILES):
    """Installs visual studio code extensions"""
    script = os.path.join(dotfiles, "lib", "vscode_extensions.sh")
    run_script(script)


def setup_everything(home=HOME, dotfiles=DOTFILES):
    """Installs everything."""
    setup_shell(home, dotfiles)
    setup_vundle(home, dotfiles)
    setup_spacemacs(home, dotfiles)
    setup_config(home, dotfiles)
    setup_i3_configs(home, dotfiles)


def _set_log_level():
    logging.basicConfig(level=logging.INFO)


def main():
    _set_log_level()
    setup_everything()


if __name__ == '__main__':
    sys.exit(main())
