# Revision

```
c723424f324f0f4ea030c1f2bca21dab4a2599ca
```

# Overlay

This overlay provides the diesel_cli binary. In order to generate the
_Cargo.nix_ and _crates-io.nix_ multiple steps are required.

# Reproduce

## Cloning the diesel repository

```bash
git clone https://github.com/diesel-rs/diesel
cd diesel
```

## Enter nix-shell with rust capabilities

```bash
nix-shell --packages pkgs.carnix pkgs.rustc pkgs.cargo
```

## Initialize the Cargo.lock file

```bash
cargo update
```

## Prepare diesel

In order to reduce the size of the generated Nix files, examples and tests
should be deleted. To delete the _examples_ folder and the _diesel_test_
folder, remove the entries for examples and diesel tests in the workspace
_Cargo.toml_.


## Generate Nix files

```bash
carnix generate-nix --standalone
```

## Fix generated files

The generated files are not usable. Some features of diesel start with an
decimal. At some point the Nix interpreter will interpret these identifiers as
floats. To fix this issue try to install the diesel_cli and fix the occurrences
of these kind of errors with the error output of the Nix interpreter.

# Modifying features

To modify features go into the file _creates-io.nix_. The simplest way is to
remove the control sequences inside a feature mapping function and replace it
with constants. For example, goto the diesel_cli definition and change the
mapping inside the function _features_.diesel_cli."1.4.0"_ from 
```nix
    diesel = fold recursiveUpdate {} [
      { "${deps.diesel_cli."1.4.0".diesel}"."mysql" =
        (f.diesel."${deps.diesel_cli."1.4.0".diesel}"."mysql" or false) ||
        (diesel_cli."1.4.0"."mysql" or false) ||
        (f."diesel_cli"."1.4.0"."mysql" or false); }
      { "${deps.diesel_cli."1.4.0".diesel}"."postgres" =
        (f.diesel."${deps.diesel_cli."1.4.0".diesel}"."postgres" or false) ||
        (diesel_cli."1.4.0"."postgres" or false) ||
        (f."diesel_cli"."1.4.0"."postgres" or false); }
      { "${deps.diesel_cli."1.4.0".diesel}"."sqlite" =
        (f.diesel."${deps.diesel_cli."1.4.0".diesel}"."sqlite" or false) ||
        (diesel_cli."1.4.0"."sqlite" or false) ||
        (f."diesel_cli"."1.4.0"."sqlite" or false); }
      { "${deps.diesel_cli."1.4.0".diesel}".default =
      (f.diesel."${deps.diesel_cli."1.4.0".diesel}".default or false); }
    ];

```
into this
```nix
    diesel = fold recursiveUpdate {} [
      { "${deps.diesel_cli."1.4.0".diesel}"."mysql" = false; }
      { "${deps.diesel_cli."1.4.0".diesel}"."postgres" =
        (f.diesel."${deps.diesel_cli."1.4.0".diesel}"."postgres" or false) ||
        (diesel_cli."1.4.0"."postgres" or false) ||
        (f."diesel_cli"."1.4.0"."postgres" or false); }
      { "${deps.diesel_cli."1.4.0".diesel}"."sqlite" =
        (f.diesel."${deps.diesel_cli."1.4.0".diesel}"."sqlite" or false) ||
        (diesel_cli."1.4.0"."sqlite" or false) ||
        (f."diesel_cli"."1.4.0"."sqlite" or false); }
      { "${deps.diesel_cli."1.4.0".diesel}".default =
      (f.diesel."${deps.diesel_cli."1.4.0".diesel}".default or false); }
    ];
```

This is not the best solution. Since MySQL cant not be added to the linkers
search path for _diesel_cli_, MySQL must be disabled for _diesel_cli_.
