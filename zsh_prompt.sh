# Colorful Zsh Prompt
# Add to your ~/.zshrc: source ~/Github/dotfiles/zsh_prompt.sh

# Enable prompt substitution
setopt PROMPT_SUBST

# Detect terminal type for color compatibility
if [[ "$TERM_PROGRAM" == "Apple_Terminal" ]] || [[ "$TERM" != *"256color"* && "$TERM" != "xterm-kitty" && "$TERM" != "alacritty" ]]; then
    # macOS Terminal.app - use basic 16 colors only
    VENV_COLOR='%F{cyan}'
    USER_COLOR='%B%F{red}'
    DIR_COLOR='%F{green}'
    GIT_COLOR='%F{magenta}'
    RESET='%f%b'
    ITALIC=''
    COLOR_RESET='%f%b'
else
    # Modern terminals (Alacritty, iTerm2, etc) - use RGB colors
    VENV_COLOR='%{'$'\e[3;38;2;150;180;220m''%}'  # Italic pastel blue
    USER_COLOR='%{'$'\e[1;38;2;165;42;42m''%}'    # Bold mahogany
    DIR_COLOR='%F{green}'
    GIT_COLOR='%F{magenta}'
    RESET='%f%b'
    ITALIC='%{'$'\e[3m''%}'
    COLOR_RESET='%{'$'\e[0m''%}'
fi

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

# Disable default venv prompt
export VIRTUAL_ENV_DISABLE_PROMPT=1

# Colorful prompt: (venv) user@host dir (git-branch)$
# Colors: venv=cyan/pastel blue, user=bold red/mahogany, dir=green, git=magenta
# %n = username, %m = hostname, %1~ = current directory (last component only)
PROMPT="${VENV_COLOR}\$(show_venv)${COLOR_RESET}${USER_COLOR}%n${RESET}@${USER_COLOR}%m${RESET} ${DIR_COLOR}%1~${RESET}${GIT_COLOR}\$(parse_git_branch)${RESET}\$ "

# Alternative simpler prompt without git:
# PROMPT="${CYAN}%n${RESET}@${BLUE}%m${RESET} ${GREEN}%1~${RESET}\$ "
