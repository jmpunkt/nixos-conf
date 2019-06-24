self: super:

{
    let 
        default_dir = ./defaults;
    in
    self.firefox = self.pkgs.firefox.override {
        postInstall = self.pkgs.firefox.postInstall 
        ++ ''
            cp -R ${default_dir} $out/lib/browser/
        '';
    };
}
 
