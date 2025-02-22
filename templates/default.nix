rec {
  default = trivial;
  trivial = {
    path = ./trivial;
    description = "minimal example";
  };
  rust = {
    path = ./rust;
    description = "rust environment";
  };
  rust-zig = {
    path = ./rust-zig;
    description = "rust environment with zig linker";
  };
  rust-wasm = {
    path = ./rust-wasm;
    description = "rust environment for wasm";
  };
  vm = {
    path = ./vm;
    description = "VM template for quick NixOS setups.";
  };
  shell = {
    path = ./shell;
    description = "shell environemnt";
  };
}
