
# If in emacs
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
   function clear(){
        vterm_printf "51;Evterm-clear-scrollback";
        tput clear;
   }
fi

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
