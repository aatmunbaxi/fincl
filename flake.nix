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

          # magicl's CFFI definitions dlopen the UNVERSIONED names
          # "libblas.so" / "liblapack.so" (and .so.3 as a fallback). nixpkgs
          # ships the versioned sonames in the default output and the
          # unversioned symlinks in "dev", so build a small compatibility
          # directory holding exactly the names magicl looks for, and put it
          # on the loader path. blas/lapack here are the nixpkgs wrappers,
          # which resolve to OpenBLAS by default.
          #
          # macOS: magicl prefers the Accelerate framework, so this directory
          # is harmless there; the dylib names differ and are found via the
          # system framework path.
          blasCompat = pkgs.runCommand "magicl-blas-compat" { } ''
            mkdir -p $out/lib
            link() {   # link <soname-base> <package-dir>
              base=$1
              dir=$2
              for ext in so so.3 dylib; do
                if [ -e "$dir/lib/$base.$ext" ]; then
                  if [ "$ext" = dylib ]; then
                    ln -sf "$dir/lib/$base.$ext" "$out/lib/$base.dylib"
                  else
                    ln -sf "$dir/lib/$base.$ext" "$out/lib/$base.so"
                  fi
                  return 0
                fi
              done
              echo "warning: $base not found under $dir/lib" >&2
            }
            link libblas ${pkgs.blas}
            link liblapack ${pkgs.lapack}
            link libopenblas ${pkgs.openblas}
          '';

          # Everything that gets dlopen'd at runtime by CFFI: BLAS/LAPACK for
          # magicl, libffi for CFFI itself, and cephes for Lisp-Stat's
          # distributions system (it builds its own shared object on first
          # load, which needs a compiler present).
          runtimeLibs = [
            blasCompat
            pkgs.blas
            pkgs.lapack
            pkgs.openblas
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

              # Toolchain: cephes.cl and magicl/ext-expokit compile C and
              # Fortran sources on first load.
              gcc
              gfortran
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
              echo "BLAS/LAPACK: ${blasCompat}/lib"
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
        });

      formatter = forAllSystems (pkgs: pkgs.nixpkgs-fmt);
    };
}
