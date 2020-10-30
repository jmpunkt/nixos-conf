# Based on https://github.com/NixOS/nix/issues/3862#issuecomment-716088688
function __fish_current_nix_pkgs
    # Returns all paths which are loaded inside $PATH by `direnv` or `nix shell`
    set result (echo $PATH | tr '[: ]' '\n' | grep '/nix/store' | sed 's#^/nix/store/[a-z0-9]\+-##'| sed 's#-[^-]\+$##' | xargs -n2 -d'\n')
    echo $result
end
