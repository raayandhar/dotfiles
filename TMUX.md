# tmux Reference Guide

## What is tmux?

tmux is a terminal multiplexer - it lets you run multiple terminal sessions in one window, detach/reattach sessions, and keep programs running when you disconnect.

## Installation

```bash
# Arch Linux
sudo pacman -S tmux

# Install config
ln -sf ~/Github/dotfiles/.tmux.conf ~/.tmux.conf
```

## Core Concepts

- **Session**: A collection of windows (like a workspace)
- **Window**: A full screen view (like a tab)
- **Pane**: A split section of a window (like vim splits)
- **Prefix**: Key combination that activates tmux commands (C-a in our config)

## Essential Keybindings

**Prefix Key: `C-a`** (Control + a)

### Session Management

| Command                     | Description                    |
|----------------------------|--------------------------------|
| `tmux`                     | Start new session              |
| `tmux new -s name`         | Start named session            |
| `tmux ls`                  | List sessions                  |
| `tmux attach -t name`      | Attach to session              |
| `tmux kill-session -t name`| Kill session                   |
| `C-a d`                    | Detach from session            |
| `C-a $`                    | Rename session                 |

### Window Management

| Keys    | Action                         |
|---------|--------------------------------|
| `C-a c` | Create new window              |
| `C-a ,` | Rename current window          |
| `C-a n` | Next window                    |
| `C-a p` | Previous window                |
| `C-a l` | Last window                    |
| `C-a w` | List windows                   |
| `C-a &` | Kill current window            |
| `M-1`   | Go to window 1                 |
| `M-2`   | Go to window 2 (etc.)          |

### Pane Management

| Keys      | Action                       |
|-----------|------------------------------|
| `C-a \|`  | Split horizontally (custom)  |
| `C-a -`   | Split vertically (custom)    |
| `C-a h`   | Move to pane left            |
| `C-a j`   | Move to pane down            |
| `C-a k`   | Move to pane up              |
| `C-a l`   | Move to pane right           |
| `C-a H`   | Resize pane left             |
| `C-a J`   | Resize pane down             |
| `C-a K`   | Resize pane up               |
| `C-a L`   | Resize pane right            |
| `C-a x`   | Kill current pane            |
| `C-a z`   | Toggle pane zoom             |
| `C-a {`   | Move pane left               |
| `C-a }`   | Move pane right              |

### Copy Mode (Scrollback)

| Keys    | Action                         |
|---------|--------------------------------|
| `C-a [` | Enter copy mode                |
| `v`     | Begin selection (in copy mode) |
| `y`     | Copy selection                 |
| `q`     | Exit copy mode                 |
| `C-a ]` | Paste buffer                   |

**In copy mode:**
- Use `h`, `j`, `k`, `l` for navigation (vim-style)
- Use `/` to search
- Use `C-u` / `C-d` for page up/down

### Miscellaneous

| Keys      | Action                       |
|-----------|------------------------------|
| `C-a ?`   | List all keybindings         |
| `C-a :`   | Enter command mode           |
| `C-a r`   | Reload config (custom)       |
| `C-a t`   | Show clock                   |

## Common Workflows

### Basic Session Workflow

```bash
# Start a new session
tmux new -s work

# Do some work...

# Detach (keeps running in background)
C-a d

# Later, reattach
tmux attach -t work
```

### Multi-Pane Development

```bash
# Start session
tmux new -s dev

# Split into 3 panes
C-a |     # Split vertical
C-a -     # Split horizontal

# Navigate between panes
C-a h/j/k/l

# Run different things in each pane:
# Pane 1: vim/emacs
# Pane 2: terminal for commands
# Pane 3: test runner / logs
```

### Working with Multiple Projects

```bash
# Create sessions for different projects
tmux new -s project1
C-a d

tmux new -s project2
C-a d

# List all sessions
tmux ls

# Switch between them
tmux attach -t project1
tmux attach -t project2
```

## Tips & Tricks

### 1. Mouse Support
Our config enables mouse support. You can:
- Click panes to switch
- Drag pane borders to resize
- Scroll with mouse wheel
- Select text with mouse

### 2. Stay Organized
- Name your sessions meaningfully: `tmux new -s web-dev`
- Name your windows: `C-a ,` then type name
- Close unused panes/windows to reduce clutter

### 3. Persistent Sessions
tmux sessions survive:
- Terminal close
- SSH disconnects
- System restarts (with tmux-resurrect plugin)

### 4. Sharing Sessions
Multiple people can attach to the same session:
```bash
# Person 1
tmux new -s shared

# Person 2 (on same machine or SSH)
tmux attach -t shared
```

### 5. Scripting tmux
Create a startup script:
```bash
#!/bin/bash
tmux new-session -d -s dev
tmux split-window -h
tmux select-pane -t 0
tmux send-keys 'vim' C-m
tmux select-pane -t 1
tmux send-keys 'git status' C-m
tmux attach -t dev
```

### 6. Copy to System Clipboard
On Linux with xclip:
```bash
# Add to .tmux.conf
bind -T copy-mode-vi y send-keys -X copy-pipe-and-cancel 'xclip -in -selection clipboard'
```

## Theme Colors (Matching Emacs)

Our tmux theme matches the Emacs custom-dark theme:
- Background: `#1c1c1c` (dark gray)
- Foreground: `#dddddd` (light gray)
- Active window: `#d0c4e0` (mellow lavender)
- Borders: `#d0c4e0` (active), `#888888` (inactive)

## Troubleshooting

### Colors look wrong
```bash
# Check terminal supports 256 colors
echo $TERM

# Should be screen-256color in tmux
# If not, add to .tmux.conf:
set -g default-terminal "screen-256color"
```

### Config not loading
```bash
# Reload config
C-a r

# Or restart tmux
tmux kill-server
tmux
```

### Can't scroll
```bash
# Enter copy mode first
C-a [
# Then use mouse wheel or j/k
```

## Advanced: TPM (Plugin Manager)

Install plugins for extra functionality:

```bash
# Install TPM
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

# Uncomment plugin lines in .tmux.conf

# Install plugins
C-a I
```

**Useful plugins:**
- `tmux-sensible`: Sensible defaults
- `tmux-resurrect`: Save/restore sessions
- `tmux-continuum`: Auto-save sessions
- `tmux-yank`: Better clipboard integration

## Quick Reference Card

```
Sessions:          Windows:           Panes:
  new -s name        c create           | split-h
  ls                 , rename           - split-v
  attach -t          n next             h/j/k/l navigate
  d detach           p previous         H/J/K/L resize
  $ rename           & kill             x kill
                     M-1..9 goto        z zoom

Copy mode:         Other:
  [ enter            ? help
  v select           : command
  y copy             r reload
  ] paste            t clock
```

## Resources

- Official docs: https://github.com/tmux/tmux/wiki
- Cheat sheet: https://tmuxcheatsheet.com/
- Book: "tmux 2: Productive Mouse-Free Development"
