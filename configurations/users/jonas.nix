{ config, pkgs, ... }:

{
  imports = [ ../yubico.nix ];

  users = {
    groups.jonas.gid = 1000;
    users.jonas = {
      extraGroups = ["wheel" "networkmanager" "audio" "users"];
      createHome = true;
      home = "/home/jonas";
      shell = pkgs.fish;
      uid = 1000;
      group = "jonas";
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDidocMVGF12ivEsTYTeh2vmOytHMRKYb5badVMXl4llFExsda5dmXyfUhWi2bS8nRtNua5G3lY9QrJpyUqReUm11RDeLLgWVX7x2kXkY0WryeXC1gTFuQlpcfHL2XEkGXoyXpgWr3zkrzjXDC93VgAWjGfzijp3n7XJ3dONjptXmjoWL39Cmw1ry+e+DCzeIUIcH6tJ6Xo7t7qi5Ry3GghCvJcxQZqtvJfqRnluTtzzfRaNVbhQ/soD1euo4agWaH0DaHlT1E84jUBcF7zmMKtIjdIv+bShPzo896Kz0Y+vZJpOr/qvZnHBhqDzCF6rzauh2DNt1HPw16/FRbW4vfA6JEEyySVkp6c+hc6/pPPSNY4dCxZwJefW2lZF6uawJSqmbMTPeCo5WF9jWgfgLTwBPFf5mxA2EqjG57vtr8JuY2YvsnZbqQM9+YZTSu1qZNj7kgCviK6JBG/+ajBENaqDLC77E0iGCXtz+el/Hvon+XzF23egELDXWhceAeDKD6JlIxxCoGPhSfm2u+hjq1kJiZTereGTZ4HFXOn2DxaPIRP3RreMV3ZXr5uavt7b4hTg01f+ANVoJapPLbTqukutOVkWcmk7CPsliONXSbL3RtzIMQmObqVXNygoysTIqaQvmBTtujfDXFRiEq6qFWACMxmZA45nmu0MIQslKKA0Q== openpgp:0x3E368087"
      ];
    };
  };
}

