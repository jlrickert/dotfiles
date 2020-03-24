My personal dotfiles
====================

My preferences and configuration for my current working environment.

Installation
------------


1. Download chezmoi at at https://github.com/twpayne/chezmoi/releases. Install
   deb file with `sudo dpkg -i chezmoi_x.x.x-xxx_linux_amd64.deb`
2. Run `git clone https://github.com/jlrickert/dotfiles.git ~/.local/share/chezmoi`
3. Create ~/.config/chezmoi/chezmoi.toml and fill in the config example
3. Run `chezmoi apply`

Config Example
--------------

```toml
[data]
  git_name = "Your Username"
  git_email = "youremail@example.com"
  git_signing_key = "4AFA345DE57133F261C2780FA895D72DD1ED47C4"
  git_signing_key = "DEADBEEFDEADBEEFDEADBEEFDEADBEEFDEADBEEF"

  i3_ethernet_interfaces = [
        "enp0s1",
  ]

  i3_wireless_interfaces = [
        "wlp0s1",
  ]
```

