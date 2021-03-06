function fish_prompt
    # - green lines if the last return command is OK, red otherwise
    # - your user name, in red if root or yellow otherwise
    # - your hostname, in cyan if ssh or blue otherwise
    # - the current path (with prompt_pwd)
    # - date +%X
    # - the current virtual environment, if any
    # - the current git status, if any, with __fish_git_promptv
    # - current background jobs, if any

    set -q __fish_git_prompt_showupstream
    or set -g __fish_git_prompt_showupstream auto

    function _nim_prompt_wrapper
        set retc $argv[1]
        set field_name $argv[2]
        set field_value $argv[3]

        set_color normal
        set_color $retc
        echo -n '─'
        set_color -o green
        echo -n '['
        set_color normal
        test -n $field_name
        and echo -n $field_name:
        set_color $retc
        echo -n $field_value
        set_color -o green
        echo -n ']'
end
and set retc green
or set retc red

set_color $retc
echo -n '┬─'
set_color -o green
echo -n [
if test "$USER" = root -o "$USER" = toor
    set_color -o red
else
    set_color -o yellow
end
echo -n $USER
set_color -o white
echo -n @
if [ -z "$SSH_CLIENT" ]
    set_color -o blue
else
    set_color -o cyan
end
echo -n (prompt_hostname)
set_color -o white
echo -n :(prompt_pwd)
set_color -o green
echo -n ']'

# Date
_nim_prompt_wrapper $retc '' (date +%X)

# Virtual Environment
set -q VIRTUAL_ENV
and _nim_prompt_wrapper $retc V (basename "$VIRTUAL_ENV")

# git
set prompt_git (__fish_git_prompt | string trim -c ' ()')
test -n "$prompt_git"
and _nim_prompt_wrapper $retc G $prompt_git

# nix
set prompt_pkgs (__fish_current_nix_pkgs)
if test -n "$prompt_pkgs"
    # use package number instead of names complying to max row width
    if test (string length "$prompt_pkgs") -gt 40
        set prompt_pkgs (echo $prompt_pkgs | tr -cd ' \t' | wc -c)
        _nim_prompt_wrapper $retc P "($prompt_pkgs*)"
    else
        _nim_prompt_wrapper $retc P $prompt_pkgs
    end
end

# New line
echo

# Background jobs
set_color normal
for job in (jobs)
    set_color $retc
    echo -n '│ '
    set_color brown
    echo $job
end
set_color normal
set_color $retc
echo -n '╰─>'
set_color -o red
echo -n '$ '
set_color normal
end
