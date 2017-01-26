import logging
import os
import random
import shutil
import stat
import string
from subprocess import call
from os.path import dirname, basename

logger = logging.getLogger(__name__)
logger.setLevel(logging.NOTSET)


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
