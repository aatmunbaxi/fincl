{
  description = "fincl — Common Lisp quantitative finance library";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = f:
        nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in
    {
      devShells = forAllSystems (pkgs:
        let
          inherit (pkgs) lib;

          # Everything that gets dlopen'd at runtime by CFFI: libffi for CFFI
          # itself, and cephes for Lisp-Stat's distributions system (it
          # builds its own shared object on first load, which needs a
          # compiler present). fincl uses no BLAS or LAPACK (task 22 removed
          # magicl), so there is no loader shim for them.
          runtimeLibs = [
            pkgs.libffi
            pkgs.zlib
          ];

          nativeLibPath = lib.makeLibraryPath runtimeLibs;

          # Python side of the fincl-viz developer aid. py4cl runs this
          # interpreter as a subprocess; numpy receives Lisp arrays and
          # matplotlib renders them. Not needed by fincl itself.
          pythonEnv = pkgs.python3.withPackages (ps: [
            ps.numpy
            ps.matplotlib
          ]);
        in
        {
          default = pkgs.mkShell {
            name = "fincl-dev";

            packages = with pkgs; [
              sbcl

              # Toolchain: cephes.cl compiles C sources on first load.
              gcc
              gnumake
              pkg-config

              # Plotting for fincl-viz (py4cl -> matplotlib).
              pythonEnv

              # Convenience.
              rlwrap
              git
            ] ++ runtimeLibs;

            LD_LIBRARY_PATH = nativeLibPath;
            DYLD_LIBRARY_PATH = nativeLibPath;   # ignored on Linux

            shellHook = ''
              # ASDF finds the system without a ~/quicklisp/local-projects
              # symlink, so (ql:quickload :fincl) works from a REPL started
              # anywhere in the tree. This must be $PWD and not ./. — inside a
              # flake, ./. is the read-only /nix/store copy of the source.
              export CL_SOURCE_REGISTRY="$PWD//:''${CL_SOURCE_REGISTRY:-}"

              echo "fincl dev shell — SBCL $(sbcl --version | cut -d' ' -f2)"
              echo
              echo "  sbcl                       # REPL; M-x sly-connect from Emacs"
              echo "  (ql:quickload :fincl)"
              echo "  (asdf:test-system :fincl)"
            '';
          };

          # Editor shell: the default shell plus Emacs and a Node runtime for
          # agent-shell's ACP agent. Kept separate so CI and headless REPL use
          # do not pull in Emacs.
          #
          #   nix develop .#editor
          editor = pkgs.mkShell {
            name = "fincl-editor";
            inputsFrom = [ self.devShells.${pkgs.system}.default ];
            packages = with pkgs; [
              emacs
              nodejs_22   # agent-shell launches its agent over stdio
            ];
          };

          # The default shell plus QuantLib and a C++ toolchain, for the
          # benchmarks against QuantLib (benchmarks/):
          #
          #   nix develop .#benchmarks -c sbcl --non-interactive \
          #     --eval '(ql:quickload :fincl/benchmarks)' \
          #     --eval '(fincl/benchmarks:run-benchmarks)'
          benchmarks = pkgs.mkShell {
            name = "fincl-benchmarks";
            inputsFrom = [ self.devShells.${pkgs.system}.default ];
            packages = [ pkgs.quantlib pkgs.boost pkgs.gcc ];
            LD_LIBRARY_PATH = nativeLibPath;
          };

          # QuantLib (C++; nixpkgs has no Python bindings), only for
          # regenerating test fixtures:
          #
          #   nix develop .#fixtures -c sh tests/fixtures/generate.sh
          fixtures = pkgs.mkShell {
            name = "fincl-fixtures";
            packages = [ pkgs.quantlib pkgs.boost pkgs.gcc ];
          };
        });

      formatter = forAllSystems (pkgs: pkgs.nixpkgs-fmt);
    };
}
