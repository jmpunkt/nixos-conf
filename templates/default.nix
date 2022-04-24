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
  shell = {
    path = ./shell;
    description = "shell environemnt";
  };
}
