from pac.shell import shell
from pac.brew import brew_info, brew_install


class Package:
    name: str
    version: str | None = None
    description: str | None = None
    dependencies: list[str] = []

    def __init__(
        self,
        name: str,
        version: str | None = None,
        description: str = None,
        dependencies: list[str] = [],
    ):
        self.name = name
        self.version = version
        self.description = description
        self.dependencies = dependencies
        self.state = {}

    def getVersion(self) -> str:
        if self.version is None:
            return "latest"
        return self.version

    def install(self):
        if self.isInstalled():
            return {"name": self.name, "installed": True}
        result = self._install()
        self.state["info"] = self._info()
        return result

    def info(self):
        if not self.state.get("info", None):
            info = self._info()
            self.state["info"] = info
            self.state["installed"] = info.get("installed")
            self.state['stdout'] = info.get("stdout")
        return self.state.get("info")

    def remove(self):
        pass

    def isInstalled(self) -> bool:
        self.__update_info()
        return self.state.get("installed", False)

    def _install(self):
        raise NotImplementedError()

    def _update(self):
        raise NotImplementedError()

    def _remove(self):
        raise NotImplementedError()

    def _info(self):
        raise NotImplementedError()

    def __update_info(self):
        info = self.info()
        self.state["info"] = info
        self.state["installed"] = info.get("installed", False)
