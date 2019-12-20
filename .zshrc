# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH
export LD_LIBRARY_PATH="/usr/local/lib"

# Path to your oh-my-zsh installation.
export ZSH=/home/scottviteri/.oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

alias agi='sudo apt install'
alias update='sudo apt update'
alias HDMI='xrandr --output HDMI-1 --auto --right-of eDP-1'
alias VGA='xrandr --output VGA-1 --auto --right-of eDP-1'
alias off='xrandr --output HDMI-1 --off && xrandr --output VGA-1 --off'
alias mute='amixer sset Master mute'
alias unmute='amixer sset Master unmute'

bindkey -v
export MIT="$HOME/MyBrain/BodySelf/FormalEducation/MIT"
export CMU="$HOME/MyBrain/BodySelf/FormalEducation/CMU"
export STANFORD="$HOME/MyBrain/BodySelf/FormalEducation/Stanford"
export CLASSES="$MIT/Year4/Spring"
export PROJ="$HOME/LocalSoftware/cvc4-lean"
export FINANCES="$HOME/MyBrain/OutsideWorld/Finances/Finances"
export DIARY="$HOME/MyBrain/BrainSelf/Diary"
export HEALTH="$HOME/MyBrain/BodySelf/Health"
export NUTRITION="$HOME/MyBrain/BodySelf/Health/Nutrition"
export SLEEP="$HOME/MyBrain/BodySelf/Health/Sleep"
export QUESTIONS="$HOME/MyBrain/BrainSelf/Questions"
export HABITS="$HOME/MyBrain/BodySelf/Habits"
export APPLICATIONS="$HOME/MyBrain/BodySelf/Career/Applications"
export EMPLOYMENT="$HOME/MyBrain/BodySelf/Career/Employment"
export TODO="$HOME/MyBrain/BrainSelf/ToDo"
export CS="$HOME/MyBrain/BrainSelf/ModelBuilding/ComputerScience"
export PROG="$CS/Programming"
export NECSI="$EMPLOYMENT/NECSI"
export PROJECTS="$HOME/MyBrain/BrainSelf/Projects"
export NIX="$HOME/LocalSoftware/ConfigAndDotFiles/NixFiles"
export COQ="$PROG/TheoremProving/Coq"
export HOTT="$PROG/TheoremProving/HoTT"
export PROOFTREES="$COQ/ManipulateProofTrees"
export WEBSITE="$STANFORD/AllYears/www"
export SMT="$COQ/SMT"

export PATH="$HOME/LocalSoftware/coq/bin:$PATH"
export PATH="$HOME/LocalSoftware/agda/.stack-work/install/x86_64-linux/lts-13.3/8.6.3/bin:$PATH"
export PATH="$HOME/LocalSoftware/exercism:$PATH"
export PATH="$HOME/LocalSoftware/lean-3.4.2-linux/bin:$PATH"
export PATH="$HOME/.mathlib/bin:$PATH"
export PATH="$HOME/.elan/bin:$PATH"
export PATH="$HOME/.cabal/bin:$PATH"

export PATH="$HOME/LocalSoftware/CVC4/build/bin:$PATH"
export PATH="$HOME/LocalSoftware/CVC4/lfsc-checker/build/src:$PATH"
export PATH="$HOME/LocalSoftware/LFSC/build/src:$PATH"
export LFSCSIGS="$HOME/LocalSoftware/smtcoq/src/lfsc/tests/signatures"
export SIGS="$HOME/LocalSoftware/CVC4/proofs/signatures"
alias lfsccc="lfscc $SIGS/sat.plf $SIGS/er.plf $SIGS/smt.plf $SIGS/th_base.plf $SIGS/lrat.plf $SIGS/drat.plf $SIGS/th_arrays.plf $SIGS/th_bv.plf $SIGS/th_bv_bitblast.plf $SIGS/th_bv_rewrites.plf $SIGS/th_real.plf $SIGS/th_int.plf $SIGS/th_lra.plf"


# opam configuration
test -r /home/scottviteri/.opam/opam-init/init.zsh && . /home/scottviteri/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

if [ -e /home/scottviteri/.nix-profile/etc/profile.d/nix.sh ]; then . /home/scottviteri/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
