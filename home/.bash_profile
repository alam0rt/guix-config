# Set up the system, user profile, and related variables.
# /etc/profile will be sourced by bash automatically
# Set up the home environment profile.
if [ -f ~/.profile ]; then source ~/.profile; fi

# Honor per-interactive-shell startup file
if [ -f ~/.bashrc ]; then source ~/.bashrc; fi
PS1='\u@\h \w${GUIX_ENVIRONMENT:+ [env]}\$ '
export HISTFILE=$XDG_CACHE_HOME/.bash_history
