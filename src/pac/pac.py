from pac.gitPackage import GitPackage
from pac.package import Package
from pac.systemPackage import SystemPackage


PACKAGES: list[Package] = [
    SystemPackage(
        name="git",
        description="git package",
        packageMap={"brew": "git", "arch": "git", "ubuntu": "git"},
    ),
    GitPackage(
        name="oh-my-zsh",
        # target="~/.local/oh-my-zsh",
        target="/tmp/oh-my-zsh",
        url="https://github.com/ohmyzsh/ohmyzsh.git",
    ),
    SystemPackage("ansible"),
    SystemPackage("gh"),
    SystemPackage("jq"),
    SystemPackage("neovim"),
    SystemPackage("yq"),
    SystemPackage("tmux"),
    SystemPackage("shfmt"),
    SystemPackage("go"),
]

allPackages = Package("all", dependencies=[package.name for package in PACKAGES])
PACKAGES.append(allPackages)


class Pac:
    packages: dict[str, Package] = {}
    state: dict[Package, any] = {}

    def __init__(self) -> None:
        self.packages = {}
        self.state = {}
        for package in PACKAGES:
            self.packages[package.name] = package

    def install(self, packages: str):
        results = []
        for package in packages:
            p = self.packages.get(package)
            if p is None:
                continue
            for dep in p.dependencies:
                for result in self.install(dep):
                    results.push(result)
            result = p.install()
            results.append(result)
        return results

    def remove(self, packages: str):
        pass

    def info(self, package: str):
        p = self.packages.get(package)
        if not p:
            return None
        return p.info()

    def list(self) -> list[Package]:
        return PACKAGES
