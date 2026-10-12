// elmux status reporter for pi.
//
// Install with `M-x elmux-install-pi-hooks`, which copies this file to
// ~/.pi/agent/extensions/elmux-status.ts where pi auto-loads it.
//
// pi has no JSON hooks file like claude and codex; state is reported from
// an extension instead.  The wire format is identical: a single word of
// "working" | "waiting" | "done" written to $HOME/.cache/elmux/$ELMUX_SESSION,
// which elmux.el sets in the vterm environment when it launches the session.
//
// pi runs its built-in tools without permission prompts and emits no event
// when an extension opens a confirm dialog, so only "working" and "done" are
// ever reported.  Without ELMUX_SESSION in the environment (a plain `pi` run
// outside elmux) the extension registers no handlers and does nothing.

import { mkdirSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

const SESSION = process.env.ELMUX_SESSION;
const STATE_DIR = join(homedir(), ".cache", "elmux");

export default function (pi: ExtensionAPI) {
  if (!SESSION) return;

  let lastState: string | null = null;

  function report(state: string) {
    if (state === lastState) return;
    try {
      mkdirSync(STATE_DIR, { recursive: true });
      writeFileSync(join(STATE_DIR, SESSION), state + "\n");
      lastState = state;
    } catch {
      // Status reporting must never take the session down.
    }
  }

  pi.on("agent_start", async () => report("working"));
  pi.on("tool_execution_end", async () => report("working"));
  pi.on("agent_end", async () => report("done"));
}
