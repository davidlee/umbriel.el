# umbriel client

umbriel is a Wayland compositor. This package lets Emacs talk to it over its
IPC (inter-process communication) socket, from these files:

| File | Owns |
| --- | --- |
| `umbriel.el` | the package entry: loads the rest |
| `umbriel-ipc.el` | the socket client: requests, the event stream, held state, failure kinds |
| `umbriel-model.el` | pure functions: grouping, labels, workspace selectors |
| `umbriel-columns.el` | pure layout of cells into aligned columns |
| `umbriel-buffer.el` | the `*umbriel*` buffer, row actions, global commands |
| `umbriel-focus.el` | focusing the frame `emacsclient` reuses |

Only `umbriel-ipc.el` touches the socket. The buffer and the focus mode are its
two consumers.

Records cited below (`DEC-n` for a decision, `IDE-n` for a backlog idea)
are the author's decision and backlog records (their `emacs-config:` prefix names the config repo that holds them).

## The socket

Every message is one JSON object on one line. The server answers
`{"ok": …}` or `{"err": "…"}`. There are three kinds of connection, all to the
same socket:

```
                       umbriel socket
 query      ──▶ {"cmd":"windows"}                 ──▶ {"ok":[…]}   closes
 action     ──▶ {"cmd":"action","action":"…",…}   ──▶ {"ok":…}     closes
 subscribe  ──▶ {"cmd":"subscribe","events":      ──▶ one snapshot per family,
                 ["windows","workspaces"]}             then one per change; stays open
```

The path is resolved from the environment, as the `umbriel` CLI does:

1. `UMBRIEL_SOCKET`, when set and non-empty (the override);
2. otherwise `$XDG_RUNTIME_DIR/umbriel-$WAYLAND_DISPLAY.sock`.

When neither resolves, the failure names the unset variables.

### Reachability from the agent environment

The agent environment can reach this socket from the session shell. Read-only
probes (queries) are fine. Mutating actions (focus, kill, switch, layout) are
the user's to take. Sandboxed (jailed) workers lack the socket. An earlier
emacs-config:SL-016 premise, that no compositor was reachable from the agent environment,
was wrong.

### Why the socket, not the CLI

- Nothing is executed. There is no missing `umbriel` binary, and a dev-shell
  `LD_LIBRARY_PATH` cannot break a subprocess (the dev shell's library path reaches child processes).
- Action fields are typed JSON, not command-line words.
- Errors arrive parsed, as `{"err": …}`, not as an exit status.

All requests are asynchronous: callers pass a callback and nothing blocks.

## The snapshot rule

Both families (`windows`, `workspaces`) arrive as full snapshots. Emacs
replaces the held copy wholesale and never merges, so there is no drift to
reconcile.

```
 compositor ──snapshot──▶ held state ──hook──▶ repaint timer (0.1 s) ──▶ *umbriel*
                       (umbriel-ipc.el)     coalesces bursts          redraws only while
                                                                         a window shows it
```

- A repaint is scheduled 0.1 s after a change, so a burst becomes one redraw.
  A buffer nobody displays is marked stale and redrawn when next shown. Point
  and folds survive the redraw.
- The stream ends when the compositor restarts, or when Emacs stops draining
  it and the backlog passes 256 KiB (the compositor drops the connection).
- Nothing reconnects by itself. A closed stream stays closed, so a stale view
  is visible, not silent. The header line says which: `umbriel: stream open`,
  `umbriel: stream never opened`, or the failure message. A count of skipped
  lines (unparseable, an unrequested family, or an event with no data) is
  appended when non-zero.
- `g` in `*umbriel*` reconnects. With the stream open it only repaints.
- Killing `*umbriel*` closes the stream.

Actions do not edit the held state. After a successful action, Emacs waits for
the stream to deliver the effect. With the stream closed it re-queries instead.

## Failure kinds

Every request ends as `(:ok VALUE)` or `(:failure KIND DETAIL)`. Echoed
failures read `umbriel: KIND — DETAIL (remedy)`.

| Kind | Meaning | Remedy |
| --- | --- | --- |
| `no-path` | the socket path cannot be resolved | set `UMBRIEL_SOCKET` |
| `absent` | no socket at that path | is umbriel running in this session? |
| `refused` | a socket, but nothing accepting (stale) | restart umbriel |
| `closed` | the connection ended before a reply | retry; `g` in `*umbriel*` reconnects |
| `rejected` | the compositor answered `{"err": …}` | the compositor refused the request; the detail has its text |
| `malformed` | the reply was not a JSON object | unexpected reply; report upstream |

A multi-step action stops at its first failure.

## The `*umbriel*` buffer

`M-x umbriel` (or `C-c u u`) opens it, creating the buffer and the stream
on first use. It is a `magit-section` tree: one section per workspace, its
windows beneath, then one section per scratchpad holding windows, by name, and a
final `Unassigned` section for windows that join neither. Sections fold as usual.
A pad window stays in its pad's section while the pad is shown, and umbriel reports
no workspace for it either way. An empty pad has no section.

Workspaces are ordered by output, then index. Tiled windows sort by column,
row, tab index, then title, before floating ones. Every compositor-supplied
string (app id, title, name) has newlines and tabs replaced by spaces, so a
row is always one line. Example, with invented names:

```
HDMI-1 1 main scrolling 2 windows active focused
  foot ~/src tile 0,1 active focused visible
  firefox Docs float visible
HDMI-1 2 scrolling 0 windows
```

- Window rows show app id, title, placement (`tile COLUMN,ROW`, bare `tile`
  while the column is unplaced, `float`, `tab INDEX`), then a glyph for each flag
  that is set. `visible`, `active` and `focused` have a column each, so they line
  up; `minimized`, `fullscreen`, `urgent` and `xwayland` share the last one.
- Flags are Nerd Font icons where a font has them and letters otherwise
  (`umbriel-buffer-flags`). A terminal names no font, so it always gets letters.
  A layout without its icon is just its name. Prompts keep the words. The header
  line explains the flags in use, aligned right after the status, and a long
  status pushes it out of view.
- Workspace rows show output, index, name (named workspaces only), the window
  count, layout, then the `active`, `focused` and `urgent` glyphs in the rows'
  flag columns.
- The window count is the number of windows joined under the workspace, not the
  wire's `window_count`, so the label always matches the rows beneath it.
- Columns line up across the whole buffer, headings and window rows each on
  their own stops, except that a heading's flags sit over the rows'
  unless the heading's own columns run past them, as they can over short titles. Over-long app ids and titles are cut with an ellipsis
  (`umbriel-buffer-column-limits`). Titles take the width the other columns
  leave in the narrowest window showing the buffer, down to
  `umbriel-buffer-title-min-width`, and resizing that window repaints. Alignment is display only: the buffer's
  text is still its cells joined by single spaces, and prompts show the full label.
- `active` and `focused` differ. `active` is seat-global (the window holding
  the keyboard); `focused` is the workspace's remembered focus. A layer
  surface or scratchpad holding the keyboard makes them disagree.
- Window rows are coloured to match: `umbriel-buffer-active-window` (inherits
  `isearch`) on the `active` row, else `umbriel-buffer-focused-window`
  (inherits `highlight`) on a `focused` row. Both span the full line and stay
  visible under magit's point highlight.
- An `active` workspace is shaded darker than `default`:
  `umbriel-buffer-active-workspace-window` on its rows,
  `umbriel-buffer-active-workspace` a step darker on its heading. The shades
  can be derived from `default` by the user's config (the package ships no shades), so they follow the theme, and
  the point highlight and the row colours above show over them.
- Window columns take their colour from `umbriel-buffer-window-app`,
  `-placement` and `-flags`, which inherit theme faces; the `active` and
  `focused` row colours replace them on those rows. An `active` workspace's
  output and index use `umbriel-buffer-active-workspace-id`, bold white.
- A heading's window count is right-justified to the title column's right edge,
  and its layout sits over the rows' placement, which it can widen.
- A workspace name and a layout are padded pills, overlays that stay shown under
  the point highlight. A name uses `umbriel-buffer-active-workspace-name` when
  active, otherwise `umbriel-buffer-workspace-name`, a muted shade a
  user's config may derive.
- A layout shows a Nerd Font icon and its face, `umbriel-buffer-layout-scrolling`,
  `-dwindle`, `-master`, `-stacking` or `-toggle`, listed in
  `umbriel-buffer-layouts`. Each inherits the shared background of
  `umbriel-buffer-layout`, which an unknown layout shows alone, without an icon.
- `visible` means: on the active workspace, not a hidden tab, not minimized.
  It does not mean "on screen"; such a window may be scrolled out of view.

### Keys

In `*umbriel*`, on the row at point:

| Key | Command | Action sent | Needs |
| --- | --- | --- | --- |
| `RET` | `umbriel-focus` | `window-focus` | window row |
| `s` | `umbriel-switch` | `workspace-switch` | workspace or window row; prompts on a pad window, which has no workspace |
| `m` | `umbriel-move-to-workspace` | `window-move-to-workspace-silent` | window row (\*); prompts for the workspace |
| `t` | `umbriel-toggle-floating` | `window-toggle-floating` | window row |
| `c` | `umbriel-center` | `window-center` | window row (\*) |
| `e` / `E` | `umbriel-cycle-extent` / `-back` | `window-cycle-primary-extent[-back]` | window row (\*) |
| `M-<left>` or `<` | `umbriel-column-left` | `column-move-left` | window row (\*) |
| `M-<right>` or `>` | `umbriel-column-right` | `column-move-right` | window row (\*) |
| `k` | `umbriel-kill` | `window-kill` | window row; asks to confirm |
| `L` | `umbriel-set-layout` | `workspace-set-layout` | workspace or window row; prompts for the layout |
| `v` | `umbriel-toggle-scratchpad` | `scratchpad-toggle` | pad heading or a window row in a pad |
| `P` | `umbriel-send-to-pad` | `window-move-to-scratchpad` | workspace window row on the output under the pointer; prompts for the pad |
| `g` | `umbriel-buffer-refresh` | none | reconnects or repaints |
| `q` | `quit-window` | none | |

(\*) On a workspace row these act on whatever window the compositor has
focused, with no focus step. `RET`, `t` and `k` refuse a workspace row
(`Not a window`), and so does `P`. A pad heading has no window, so every key but
`v` refuses it (`Not a window`).

`P` offers the pads that hold windows, plus `umbriel-buffer-known-pads`
(`main`), because the IPC lists no empty pads. Any other empty pad must be typed.
Pads exist only as `[[scratchpad]]` entries in umbriel's config, so `P` cannot
create one, and umbriel rejects a name it does not know. That failure comes after
the focus step, so focus stays on the row's window.

There are no scroll keys: strip scrolling is a viewport action, not an action
on the row at point (emacs-config:DEC-026). Use umbriel's own bindings.

Global commands. The package binds no keys; the author's config binds them
under `C-c u` (and `SPC u` on its meow leader):

| Key | Command | Does |
| --- | --- | --- |
| `C-c u u` | `umbriel` | open `*umbriel*` |
| `C-c u w` | `umbriel-switch-workspace` | switch workspace, chosen with `completing-read` |
| `C-c u l` | `umbriel-set-active-layout` | set the layout of the active workspace on the output under the pointer |

`C-c u w` prompts at once when the stream is open and workspaces are held.
Otherwise it refreshes first and prompts when the reply lands. The workspace
and layout prompts list in display order (workspaces by output, then index;
layouts in a fixed order); recency sorting is switched off for them.

## Addressing

Two kinds of identifier, not interchangeable:

- A **window** is addressed by its opaque `id`.
- A **workspace** is addressed by a selector: its `name` when it is named
  (even if the name is all digits), else its `index`, always with its
  `output`. A window joins its workspace through the workspace's opaque `id`,
  never through the selector.

Some actions take a window id. Others act only on whatever the compositor has
focused. The buffer makes "the row at point" mean what it says by planning the
steps first. The row is captured before any prompt, and a refusal comes before
the prompt, so a refused row never asks anything.

| Addressing | Rows | Requests sent |
| --- | --- | --- |
| window id | `RET`, `t`, `k` | the action, with the row's window id |
| focused | `c`, `e`, `E`, `M-<left>`, `M-<right>`, `m` | `window-focus` row, the action, then `window-focus` back |
| workspace | `s` | `workspace-switch` to the row's workspace |
| active workspace | `L` | `workspace-switch` first, unless already focused, then `workspace-set-layout` |
| scratchpad | `v` | `scratchpad-toggle` with the row's pad name |
| pointer window | `P` | as focused, for a workspace window on the output under the pointer |

- **Focus, act, restore.** For a focused-only action on a window that is not
  the focused one, Emacs focuses it, acts, then refocuses the window the
  focused workspace held before (normally this Emacs frame), when that is known
  (emacs-config:DEC-025). Expect a brief flicker. The previous focus comes from held state
  and can be stale if the buffer lags; a vanished window makes the restore
  fail and the failure is echoed. A failed action stops the sequence, so no
  restore is sent. Moving a window to a workspace uses the `-silent` action,
  so focus does not follow it.
- **Layout acts on the output under the pointer.** `workspace-set-layout`
  changes the active workspace of that output. `L` refuses a row on any other
  output. It switches to the row's workspace first only when that workspace is
  not already the focused one: switching to the active workspace would toggle
  back to the previous one under umbriel's `back_and_forth` setting.
- **A send to a pad acts on the workspace under the pointer.**
  `window-move-to-scratchpad` takes the remembered focus of the active workspace
  on the pointer's output, whatever holds the keyboard, and `window-focus` does
  not move the pointer. On a pad window or a row on another output it would send
  a different window, often this Emacs, so `P` refuses both. To move a window
  between pads, take it out with `m`, then send it.
- **Refusals.** A heading, an empty buffer or an entry no longer held gives
  `No umbriel row at point`. A window with no workspace cannot be switched to.
  `L` refuses a pad window, which has no layout, and points to `C-c u l`.

### `window-kill` kills the owning process

`k` sends `window-kill`, which terminates the process that owns the window, not
just the window. It asks first, naming the row and the pid. It refuses at once,
before any prompt, if the window is this Emacs; use `delete-frame` there.

## Frame focus

Wayland raises a window only for a client holding an activation token (a
one-time permission the compositor hands out). `emacsclient` brings none, so
when it reuses a frame the server's `select-frame-set-input-focus` can leave
that frame behind the terminal. Only the compositor can focus it.

`umbriel-focus-mode`, a global minor mode, runs `umbriel-focus-frame` on
`server-switch-hook` (the command is also callable with `M-x`), for
`pgtk` frames only:

```
 server-switch-hook ──▶ query "windows" ──▶ pick window by (this pid, frame title)
                                        ──▶ action "window-focus" <id>
```

It makes two one-off requests and does not use the held stream, so it works with
no `*umbriel*` buffer. A frame with no match is silent; a failure is echoed.

### Frame identity limits

The match is on pid and title. Two frames showing the same buffer share a
title, so the first match wins and the other frame may not be the one focused.
A reliable fix, giving each frame a unique token title through a creation
handshake, is recorded as emacs-config:IDE-003 (a backlog idea).

## Emacs holds no layout authority

Emacs shows what the compositor reports and sends requests. It keeps no layout
state of its own and applies no effect locally. Whether a request had an effect
is known only when the next snapshot says so.

## Records

- emacs-config:DEC-016: row actions address by id or selector; focus first where an action
  takes only the focused window.
- emacs-config:DEC-024: key choices. emacs-config:DEC-025: restore previous focus. emacs-config:DEC-026: scroll
  actions dropped.
