{
  description = "Just testing";

  inputs.nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0.1";

  outputs = inputs:

    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forEachSupportedSystem = f: inputs.nixpkgs.lib.genAttrs supportedSystems (system: f {
        pkgs = import inputs.nixpkgs {
          inherit system;
        };
      });
    in
    {
      devShells = forEachSupportedSystem ({ pkgs }: {
        default = pkgs.mkShell {
           buildInputs = [
              pkgs.sbcl

              # for koans
              pkgs.inotify-tools

              # for CLOG
              pkgs.openssl
              pkgs.sqlite
              pkgs.libffi
              pkgs.blas
              pkgs.lapack
            ];
            shellHook = ''
              export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath([pkgs.openssl])}:${pkgs.lib.makeLibraryPath([pkgs.libffi])}:${pkgs.lib.makeLibraryPath([pkgs.sqlite])}:${pkgs.lib.makeLibraryPath([pkgs.lapack])}:${pkgs.lib.makeLibraryPath([pkgs.blas])}
            '';
          # packages = with pkgs; [  ];
        };
      });
    };
}
