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
  vm = {
    path = ./vm;
    description = "VM template for quick NixOS setups.";
  };
  shell = {
    path = ./shell;
    description = "shell environemnt";
  };
}
