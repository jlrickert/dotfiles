import json
from pac.shell import shell


def brew_install(packages: str):
    command = ["brew", "install"]
    for package in packages:
        command.append(package)
    result = shell(command)
    return result


def brew_info(package: str):
    result = brew("info", package)
    stdout = result.stdout
    stderr = result.stderr
    dump = brew("info", package, "--json")
    if dump.stdout.find("Not installed") >= 0:
        return None
    data = json.loads(dump.stdout)[0]
    version: str = data["installed"][0]["version"]
    desc = data["desc"]

    d = {}
    d["version"] = version
    d["desc"] = desc
    d["stdout"] = stdout
    d["stderr"] = stderr
    return d


def brew_is_installed(package: str):
    result = brew("info", package)
    return not (result.stdout).find("Not installed")


def brew(*args):
    return shell(["brew", *args])
