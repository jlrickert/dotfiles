import subprocess


def shell(command: list[str], cwd: str | None = None):
    result = subprocess.run(
        command,
        text=True,
        check=True,
        cwd=cwd,
        capture_output=True,
    )
    return result
