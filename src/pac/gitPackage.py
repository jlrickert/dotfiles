from pac.shell import shell
from pac.package import Package


class GitPackage(Package):
    def __init__(
        self,
        name: str,
        target: str,
        url: str = None,
        tag: str | None = None,
        dependencies: list[str] = [],
    ) -> None:
        super().__init__(name=name, version=tag, dependencies=dependencies)
        self.name = name
        self.target = target
        self.url = url
        self.tag = tag

    def install(self) -> bool:
        command = ["git", "clone"]
        result = shell()
        try:
            result = subprocess.run(
                command,
                text=True,
                check=True,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
            )
            return result
        except subprocess.CalledProcessError as e:
            print(f"Command failed with return code {e.returncode}:\n{e.stderr}")
        except FileNotFoundError:
            print("Command not found or executable.")

    def _info(self):
        result = shell(["git", "status"], cwd=self.target)
        print(result)

        return
