# dotfiles
Holds my dotfiles and settings for my emacs setup and kitty.

# emacs notes
## Programming Languages
### `CUDA`
Wacky config put together using tree-sitter + LSP + clangd.
- Automatically detected from `.cu` and `.cuh` files
- Auto-formats with `clang-format` on save
- CUDA builtins (`__global__`) are highlighted
- LSP (clangd as C++):
  - needs `compile_commands.json` (CMake: `cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ...`)
- Currently somewhat broken: `M-x run-cuda-files`
### `C/C++`
- clang-format:
  - `C-c f` to format buffer
  - `C-c r` to format region
  - Runs on save
- cmake-ide auto-configures LSP using `compile_commands.json`
- projectile for project management:
  - `C-c p f` to find file in project
  - `C-c p p` to switch project
- Currently somewhat broken: `M-x run-cpp-17` compiles `clang++ -std=c++17 -O2` and runs
### `Python`
- Make sure to `npm install pyright`, we are using lsp-mode + lsp-pyright
- The current Python environment should be displayed using `doom-modeline`
- Can try `M-x run-python-file` or `C-c r`
#### Environment management
pyvenv:
- We can activate venv manually:
  - `M-x pyvenv-activate` to choose the path to venv
  - `M-x pyvenv-workon` select from `$WORKON_HOME` (you set to `~/.virtualenvs`)
  - Deactivate with `M-x pyvenv-deactivate`
- Should automatically activate if a project has a `venv/` or `.venv/` in root
uv
- Requires `uv` CLI
  - Init: `M-x uv-init`
  - Install deps: `M-x uv-add`
  - Remove depts: `M-x uv-remove`
  - Run: `M-x uv-run`
  - Update lock-file: `M-x uv-lock`
  - Sync: `M-x uv-sync`
- Requires tree-sitter `toml` (`pyproject.toml`)
## Miscellaneous and QOL
### vertico
(vertical completion UI, as usual)
- Try `M-x`, `C-x b`, `C-x f`
- History cycling: `M-p` / `M-n`, rest as usual
### savehist
- `C-r` in minibuffer to search history
### consult
- `C-M-s` for better search of current buffer
- `C-x b` for better buffer
- `M-y` browse kill-ring
- `C-x C-r` to see recent files
- `C-c f` to find files in project/dir
- `M-x consult-ripgrep` project grep with ripgrep
### embark
(contextual actions on minibuffer candidates or symbols at point)
- `C-.` to do actions on thing at point
- `C-;` do what I mean
- `C-h B` list all keybindings active here
### avy
- `C-:` jump to character in view
- `C-'` jump tot two character sequence
- `M-g f` jump to a line
- `M-g w` jump to word starting with cahr
### which-key
(shows you available keybindings dynamically)
- Toggle with `M-x which-key-mode`
- Will automatically appear when you pause after a prefix
- Can manually trigger with `M-x which-key-show-top-level`
- Can search with `M-x which-key-show-major-mode` for bindings relevant to current major mode
### crux
- `C-c o` open file externally
- `C-c d` duplicate line/region
- `C-c k` kill other buffers
- `C-c f` recentf find file
### expand-region
- `C-=` expand
- `C--` contract
### multiple-cursors
- `C-s-c C-s-c` edit lines
- `C->` / `C-<` mark next/prev like this
- `C-c C-<` mark all like this
### quickrun
- `C-c r` run current file in buffer's languages
### dumb-jump
- `M-g j` go
- `M-g b` back
- `M-g q` quick look
### deadgrep
- `C-c s` search project
### vterm
- `M-x vterm`
### vlf
(view large files)
- `M-x vlf` to select a huge file
- Move around chunks:
  - `n` next chunk
  - `p` previous chunk
  - `j` jump to chunk by number
- `M-x vlf-re-search-forward` search chunk by chunk
## Git
### git-gutter
(show changes vs. Git HEAD in the fringe)
- `M-x git-gutter-mode` enable/disable
- `+` for added, `~` for modified, `-` for deleted
- `M-x git-gutter:next-hunk` jump to next change
- `M-x git-gutter:previous-hunk` jump to previous change
- `M-x git-gutter:revert-hunk` undo change at point
- `M-x git-gutter:stage-hunk` stage just this hunk
### Git Timemachine
(step through file history line by line)
- `M-x git-timemachine` put buffer in timemachine mode
- Navigation:
  - `p` previous revision
  - `n` next revision
  - `q` quit
- Info:
  - `b` show commit message and hash
  - `w` copy commit hash to kill-ring
### Magit
(full Git porcelain in Emacs)
- Enter with `C-x g`, `TAB` on recent commits to see
- Staging and committing:
  - `s` stage file/hunk
  - `u` unstage file/hunk
  - `c c` commit
  - `C-c C-c` finalize commit
- Branching and checkout
  - `b b` checkout branch
  - `b c` create branch
- Pushing and pulling:
  - `P p` push current branch
  - `F p` pull from remote
- Diffing:
  - `d d` diff working tree vs. HEAD
  - `d u` diff unstaged
  - `d s` diff staged
- Logs / blame:
  - `l l` logs for current branch
  - `l o` log for other refs
  - `b l` blame file





