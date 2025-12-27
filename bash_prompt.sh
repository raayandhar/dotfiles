# Colorful Bash Prompt
# Add to your ~/.bashrc: source ~/Github/dotfiles/bash_prompt.sh

# Color codes
RESET='\[\033[0m\]'
CYAN='\[\033[0;36m\]'
BLUE='\[\033[0;34m\]'
GREEN='\[\033[0;32m\]'
MAGENTA='\[\033[0;35m\]'
YELLOW='\[\033[0;33m\]'
RED='\[\033[0;31m\]'
BOLD_CYAN='\[\033[1;36m\]'
BOLD_GREEN='\[\033[1;32m\]'
ITALIC='\[\033[3m\]'
PASTEL_BLUE='\[\033[38;2;150;180;220m\]'

# Git branch in prompt (if in git repo)
parse_git_branch() {
    git branch 2>/dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

# Show virtual environment name (if activated)
show_venv() {
    if [ -n "$VIRTUAL_ENV" ]; then
        # Use VIRTUAL_ENV_PROMPT if set (respects --prompt flag), otherwise use basename
        if [ -n "$VIRTUAL_ENV_PROMPT" ]; then
            # Strip parentheses and spaces from VIRTUAL_ENV_PROMPT
            local venv_name=$(echo "$VIRTUAL_ENV_PROMPT" | sed 's/[() ]//g')
            echo "($venv_name) "
        else
            echo "($(basename $VIRTUAL_ENV)) "
        fi
    fi
}

# Disable default venv prompt (we'll handle it ourselves)
export VIRTUAL_ENV_DISABLE_PROMPT=1

# Colorful prompt: (venv) [user@host dir] (git-branch)$
# Colors: venv=italic pastel blue (no bg), user=bold mahogany, @=white, host=bold mahogany, dir=green, git=magenta
BOLD_MAHOGANY='\[\033[1;38;2;165;42;42m\]'
PS1="${ITALIC}${PASTEL_BLUE}\$(show_venv)${RESET}${BOLD_MAHOGANY}\u${RESET}@${BOLD_MAHOGANY}\h${RESET} ${GREEN}\W${RESET}${MAGENTA}\$(parse_git_branch)${RESET}\$ "

# Alternative simpler prompt without git:
# PS1="${CYAN}\u${RESET}@${BLUE}\h${RESET} ${GREEN}\W${RESET}\$ "
