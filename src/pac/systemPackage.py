from platform import platform
from pac.brew import brew_info, brew_install
from pac.package import Package


class SystemPackage(Package):
    def __init__(
        self,
        name: str,
        version: str | None = None,
        description: str | None = None,
        dependencies: list[str] = [],
        packageMap: dict[str, str] = {},
    ) -> None:
        super().__init__(
            name=name,
            version=version,
            description=description,
            dependencies=dependencies,
        )
        self.isMacOS = platform(terse=True).find("macOS") >= 0

    def _install(self):
        if self.isMacOS:
            result = brew_install(packages=self.name)
            return {
                "name": self.name,
                "installed": result.returncode == 0,
                "stdout": result.stdout,
            }

    def _remove(self):
        pass

    def _info(self):
        if self.isMacOS:
            result = brew_info(self.name)
            if result is None:
                return {"installed": False}
            return {
                "installed": True,
                "stdout": result.get("stdout", None),
                "installedVersion": result.get("version"),
            }
        return {
            "installed": False,
        }
