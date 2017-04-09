"""Install dotfiles to the system.

Will move anything that insnt a symlink into your tmp dir
"""
import logging
import os
import random
import shutil
import stat
import string
import sys
from os.path import basename, dirname, expanduser, realpath
from subprocess import call

# makes sure python can find my custom libraries
sys.path.insert(0,
                os.path.join(
                    dirname(realpath(__file__)), 'config', 'local', 'lib',
                    'python'))

HOME = expanduser('~')
DOTFILES = dirname(realpath(__file__))

logger = logging.getLogger(__name__)


def symlink(src=None, dst=None, force=True):
    """Symlinks files"""
    assert src and dst
    mkdir(dirname(dst))
    if os.path.exists(dst):
        if force:
            rm(dst)
            symlink(src=src, dst=dst)
    else:
        logger.debug("Linking '{}' -> '{}'".format(src, dst))
        os.symlink(src, dst)


def rm(src, backup=True):
    """Removes a file and places it into a backup directory."""
    if not os.path.exists(src):
        pass
    elif os.path.islink(src):
        os.remove(src)
        logger.debug('Removing symlink {}'.format(src))
    elif backup:
        tmp_dir = os.environ['TMPDIR']
        rand_id = ''.join(random.SystemRandom().choice(
            string.ascii_uppercase + string.digits) for _ in range(5))
        backup_file = rand_id+"-"+basename(src)
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
    call([exe] + list(args))


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


def install_i3_configs(dev_type, home=HOME, dotfiles=DOTFILES, method=symlink):
    """Installs the proper i3 config based on the environment"""
    core_src = os.path.join(dotfiles, "i3", "config")
    core_dst = os.path.join(home, ".config", "i3", "config")
    method(core_src, core_dst)

    status_bar_src = None
    bar_dst = os.path.join(home, ".config", "i3status", "config")
    if dev_type == "desktop":
        status_bar_src = os.path.join(dotfiles, "i3", "i3desktop-bar")
    elif dev_type == "labtop":
        status_bar_src = os.path.join(dotfiles, "i3", "i3labtop-bar")
    else:
        status_bar_src = None
        method = lambda x, y: None

    method(status_bar_src, bar_dst)


def install_bin(home=HOME, dotfiles=DOTFILES, method=symlink):
    """Installs all in binary to a location"""
    bin_src = os.path.join(dotfiles, 'bin')
    bin_dir = os.path.join(home, '.local', 'bin')

    for name in os.listdir(bin_src):
        bin_file = os.path.join(bin_src, name)
        mark_as_exec(bin_file)
        dst = os.path.join(bin_dir, name)
        method(bin_file, dst)


def install_spacemacs(home=HOME, dotfiles=DOTFILES, method=symlink):
    """Installs my emacs configurations"""
    script = os.path.join(dotfiles, 'lib', 'install_spacemacs.sh')
    emacs_dir = os.path.join(home, '.emacs.d')
    if not os.path.exists(emacs_dir):
        run_script(script)
    else:
        logger.debug("SKIPPING :: {} already exists".format(emacs_dir))


def install_vundle(home=HOME, dotfiles=DOTFILES):
    """Installs vundle for vim"""
    script = os.path.join(dotfiles, 'lib', 'install_vundle.sh')
    vundle_dir = os.path.join(home, ".vim", "bundle", "Vundle.vim")
    if not os.path.exists(vundle_dir):
        run_script(script)
    else:
        logger.debug("SKIPPING :: {} already exists".format(vundle_dir))


def install_pyenv(home=HOME, dotfiles=DOTFILES):
    """Installs pyenv"""
    script = os.path.join(dotfiles, "lib", "install_pyenv.sh")
    pyenv_dir = os.path.join(home, ".pyenv")
    if not os.path.exists(pyenv_dir):
        run_script(script)
    else:
        logger.debug("SKIPPING :: {} already exists".format(pyenv_dir))


def install_rbenv(home=HOME, dotfiles=DOTFILES):
    """Installs rbenv"""
    script = os.path.join(dotfiles, "lib", "install_rbenv.sh")
    rbenv_dir = os.path.join(home, ".rbenv")
    if not os.path.exists(rbenv_dir):
        run_script(script)
    else:
        logger.debug("SKIPPING :: {} already exists".format(rbenv_dir))


def _set_log_level():
    try:
        log_level = os.environ["LOG_LEVEL"]
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


def _get_dev_type():
    dev_types = [
        "desktop",
        "labtop",
        "other",
    ]
    msg = """
Please input your device type number:
{}
Number: """.format(
        "\n".join(
            ["{}) {}".format(i+1, dev) for i, dev in enumerate(dev_types)]))

    args = sys.argv
    if len(args) == 2:
        input_dev = args[1]
        if args[1] in dev_types:
            dev_type = input_dev
        else:
            dev_type = input_dev
            raise ValueError("{} is not a valid device type".format(dev_type))
    else:
        dev_type = None
        while dev_type is None:
            try:
                user_input = int(input(msg))
                dev_type = dev_types[user_input - 1]
            except IndexError as e:
                print(e)
                print(msg)


def install_everything(home=HOME, dotfiles=DOTFILES):
    """Installs everything."""

    _set_log_level()
    dev_type = _get_dev_type()

    install_oh_my_zsh(home, dotfiles)
    install_i3_configs(dev_type, home, dotfiles)
    install_bin(home, dotfiles)
    install_spacemacs(home, dotfiles)
    install_vundle(home, dotfiles)
    install_rbenv(home, dotfiles)
    install_pyenv(home, dotfiles)
    install_configs(home, dotfiles)  # this must be last


if __name__ == '__main__':
    sys.exit(install_everything())
