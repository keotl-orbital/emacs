// elmux status reporter for opencode.
//
// Install with `M-x elmux-install-opencode-hooks`, which copies this file to
// ~/.config/opencode/plugins/elmux-status.js where opencode auto-loads it.
//
// opencode has no JSON hooks file like claude and codex; state is reported
// from a plugin instead.  The wire format is identical: a single word of
// "working" | "waiting" | "done" written to $HOME/.cache/elmux/$ELMUX_SESSION,
// which elmux.el sets in the vterm environment when it launches the session.
//
// Without ELMUX_SESSION in the environment (a plain `opencode` run outside
// elmux) the plugin registers no hooks and does nothing.

import { mkdirSync, writeFileSync } from "node:fs"
import { homedir } from "node:os"
import { join } from "node:path"

const SESSION = process.env.ELMUX_SESSION
const STATE_DIR = join(homedir(), ".cache", "elmux")

let lastState = null

function report(state) {
  if (state === lastState) return
  try {
    mkdirSync(STATE_DIR, { recursive: true })
    writeFileSync(join(STATE_DIR, SESSION), state + "\n")
    lastState = state
  } catch {
    // Status reporting must never take the session down.
  }
}

export const ElmuxStatus = async () => {
  if (!SESSION) return {}

  return {
    "chat.message": async () => report("working"),
    "tool.execute.after": async () => report("working"),

    // permission.ask fires before opencode decides whether the user is
    // actually prompted, so the authoritative signal is permission.asked
    // on the event stream below.  This is kept as a fast path.
    "permission.ask": async () => report("waiting"),

    event: async ({ event }) => {
      switch (event.type) {
        case "session.idle":
          report("done")
          break
        case "permission.asked":
          report("waiting")
          break
        case "permission.replied":
          report("working")
          break
      }
    },
  }
}
