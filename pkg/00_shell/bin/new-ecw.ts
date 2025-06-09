#!/usr/bin/env -S deno run --allow-all
import { Denzai } from "../repos/github.com/jlrickert/denzai/mod.ts";

const productCmd: Denzai.Cmd;

const catalogCmd: Denzai.Cmd<any> = {
  name: "catalog",
  completer: {
    complete(...args) {
      return [];
    },
  },
};

const configEditCmd: Denzai.Cmd<any> = {
  name: "edit",
};

const configCmd: Denzai.Cmd<any> = {
  name: "config",
  completer: {
    complete(...args) {},
  },
  children: [configEditCmd],
};

const cmd: Denzai.Cmd<any> = {
  name: "ecw",
  short: "",
  long: "",
  children: [catalogCmd],
  init(ctx, ...args) {
    return null;
  },
};

if (import.meta.main) {
  await Denzai.run(cmd);
}
