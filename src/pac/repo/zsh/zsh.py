from pac.package import Package, SystemPackage


class ZshPackage(Package):
    name = "zsh"
    dependencies = [SystemPackage("git")]
    version = "0.1.0"

    def __init__(self, target: str):
        super().__init__(target)

    def install(self) -> bool:
        return False

    def check(self) -> bool:
        return False

    def uninstall(self) -> bool:
        return False
