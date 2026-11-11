# Changelog

## 0.1.0 — 2026-11-11

First release.

- `M-x umbriel`: workspaces, scratchpads and windows in a `magit-section`
  buffer, kept current by the compositor's event stream, with row actions
  (`umbriel-buffer-mode-map`, `?` lists them).
- `umbriel-switch-workspace`, `umbriel-set-active-layout`, and
  `umbriel-command-map` to bind them under one prefix.
- `umbriel-focus-mode`: focus the frame an `emacsclient` reuses.
- Options: `umbriel-buffer-known-pads`, `umbriel-buffer-column-limits`,
  `umbriel-buffer-title-min-width`.
