import logging
import os
import random
import stat
import string
from subprocess import call
from os.path import dirname, basename

_log = logging.getLogger('fileutils')
_log.setLevel(logging.INFO)


def symlink(src=None, dst=None, force=True):
    """Symlinks files"""
    assert src and dst
    mkdir(dirname(dst))
    if os.path.exists(dst):
        if force:
            rm(dst)
            symlink(src=src, dst=dst)
    else:
        _log.info("Linking '{}' -> '{}'".format(src, dst))
        os.symlink(src, dst)


def rm(file, backup=True):
    """Removes a file and places it into a backup directory."""
    if not os.path.exists(file):
        pass
    elif os.path.islink(file):
        os.remove(file)
        _log.info('Removing symlink {}'.format(file))
    elif backup:
        tmp_dir = os.environ['TMPDIR']
        rand_id = ''.join(random.SystemRandom().choice(
            string.ascii_uppercase + string.digits) for _ in range(5))
        backup_file = rand_id+basename(file)
        os.rename(file, os.path.join(tmp_dir, backup_file))
        _log.info('Backup up {} to {}'.format(file, backup_file))
    else:
        os.remove(file)
        _log.info('Removing {}'.format(file))


def mark_as_exec(bin_name):
    """Marks a file as an executable.  Same as chmod +x BIN_NAME"""
    status = os.stat(bin_name)
    os.chmod(bin_name, status.st_mode | stat.S_IEXEC)


def mkdir(path):
    """Make sure that a directory exists"""
    if not os.path.exists(path):
        _log.info('Created directory {}'.format(path))
        os.makedirs(path)


def run_script(file_path, *args):
    """Calls a script with the given args."""
    call([file_path] + args)
