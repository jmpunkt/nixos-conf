{pkgs}: {
  keyboard-get = pkgs.writeShellScript "hyprland-get-keyboard" ''
    # https://github.com/hyprwm/Hyprland/discussions/2616#discussioncomment-6327590
    # get-lang
    # if we switch all of them, then getting any of them (the first) works.
    hyprctl devices -j |
      ${pkgs.jq}/bin/jq -r '.keyboards[] | .active_keymap' |
      head -n1 |
      cut -c1-2 |
      tr 'a-z' 'A-Z'
  '';
  keyboard-switch = pkgs.writeShellScript "hyprland-switch-keyboard" ''
    # https://github.com/hyprwm/Hyprland/discussions/2616#discussioncomment-6327590
    # switch-lang
    hyprctl \
      --batch "$(
        hyprctl devices -j |
          ${pkgs.jq}/bin/jq -r '.keyboards[] | .name' |
          while IFS= read -r keyboard; do
            printf '%s %s %s;' 'switchxkblayout' "''${keyboard}" 'next'
          done
      )"
  '';
}
