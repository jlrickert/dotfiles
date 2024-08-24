from cleo.application import Application

from cleo.commands.command import Command
from cleo.helpers import argument

from pac.pac import PACKAGES, Pac


packagesArgument = argument("pkgs", multiple=True, description="Package")


class AddCommand(Command):
    name = "add"
    description = "Install packages"
    arguments = [argument(name="packages", description="Packages", multiple=True)]
    usages: ["a", "b", "c"]

    def __init__(self, pac: Pac) -> None:
        self.pac = pac
        super().__init__()

    def handle(self):
        packages = self.argument("packages")

        results = self.pac.install(packages)
        print(results)
        for result in results:
            if result.stdout:
                self.line(result.stdout)
            self.line(f"Installing packages: {result}")


class RemoveCommand(Command):
    name = "remove"
    description = "Remove package(s)"
    arguments = [packagesArgument]

    def __init__(self, pac: Pac) -> None:
        self.pac = pac
        super().__init__()

    def handle(self):
        packages = self.argument("pkgs")

        self.pac.remove(packages)
        self.line(f"Installing packages: {packages}")


class InfoCommand(Command):
    name = "info"
    description = "Show package info"
    arguments = [argument("package", description="Shows information about packages")]

    def __init__(self, pac: Pac) -> None:
        self.pac = pac
        super().__init__()

    def handle(self) -> int:
        package = self.argument("package")
        info = self.pac.info(package)
        if info is None:
            self.line_error(f"{package} is not found")
            return 1

        stdout = info.get("stdout", None)
        if stdout is not None:
            self.line(stdout)


class ListCommand(Command):
    name = "list"
    description = "List all packages"

    def __init__(self, pac: Pac) -> None:
        self.pac = pac
        super().__init__()

    def handle(self) -> int:
        packages = self.pac.list()
        for package in packages:
            if package.version:
                self.line(f"{package.name} -> {package.version}")
            else:
                self.line(f"{package.name}")


def main():
    pac = Pac()
    application = Application("pac", "0.1.0 (alpha)")
    application.add(AddCommand(pac))
    application.add(RemoveCommand(pac))
    application.add(InfoCommand(pac))
    application.add(ListCommand(pac))
    application.run()
