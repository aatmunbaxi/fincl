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
        default = pkgs.mkShell  rec {
           buildInputs = [
              pkgs.sbcl
              pkgs.openssl
              pkgs.sqlite
              # For magicl
              pkgs.libffi
              pkgs.blas
              pkgs.lapack

              pkgs.python312Packages.numpy
              pkgs.python312Packages.matplotlib
            ];
                 shellHook = ''
                        export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath buildInputs}:$LD_LIBRARY_PATH"
                        export LD_LIBRARY_PATH="${pkgs.stdenv.cc.cc.lib.outPath}/lib:$LD_LIBRARY_PATH"
                        '';
        };
      });
    };
}
