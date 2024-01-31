{buildNpmPackage}:
# Single file library for converting a HTML dom into a an SVG
# image. Copy the code inside
# ${html-to-svg}/lib/node_modules/html-to-svg/out.js into the
# developer console.
buildNpmPackage {
  pname = "html-to-svg";
  version = "1.0.0";

  src = ./.;

  npmDepsHash = "sha256-ZdKm/dCFScvymu4iA5XDUCm1XmoIdXo7IxBGfXFanj8=";
}
