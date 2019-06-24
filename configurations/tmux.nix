{ config, pkgs, ... }:

{
  programs.tmux = {
    enable = true;
    clock24 = true;
    keyMode = "vi";
    historyLimit = 10000;
    escapeTime = 50;
    extraTmuxConf = ''
        set -g mouse on
        setw -g alternate-screen on
        set -g default-terminal "screen-256color"

        # COLOUR (base16)
        # default statusbar colors
        set-option -g status-style "fg=#a09f93,bg=#393939"

        # default window title colors
        set-window-option -g window-status-style "fg=#a09f93,bg=default"

        # active window title colors
        set-window-option -g window-status-current-style "fg=#ffcc66,bg=default"

        # pane border
        set-option -g pane-border-style "fg=#393939"
        set-option -g pane-active-border-style "fg=#515151"

        # message text
        set-option -g message-style "fg=#d3d0c8,bg=#393939"

        # pane number display
        set-option -g display-panes-active-colour "#99cc99"
        set-option -g display-panes-colour "#ffcc66"

        # clock
        set-window-option -g clock-mode-colour "#99cc99"

        # copy mode highligh
        set-window-option -g mode-style "fg=#a09f93,bg=#515151"

        # bell
        set-window-option -g window-status-bell-style "fg=#393939,bg=#f2777a"
    '';
  };
}
