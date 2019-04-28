p() {
    cd `/code/dotfiles/python/projects.py $@`
}

alias pc='p c'
alias pg='p g'
alias pl='p l'
alias pp='p p'
alias ppo='p po'
alias pr='p r'
alias pu='p u'

p2() {
    python - $@ <<EOF

import sys

print('here!', sys.argv)

EOF
}
