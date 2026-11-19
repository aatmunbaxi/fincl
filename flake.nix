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

          # Python side of the fincl-viz developer aid: viz/render.py renders
          # the Plotly JSON fincl-viz writes (plotly, and kaleido for images).
          # Not needed by fincl itself.
          pythonEnv = pkgs.python3.withPackages (ps: [
            ps.plotly
            ps.kaleido
          ]);

          # Kaleido 0.2's Chromium has no WebGL of its own in nixpkgs, so 3-D
          # traces need EGL from Mesa's software renderer. viz/render.py hands
          # this to kaleido alone, leaving GL for everything else untouched.
          eglVendor = "${pkgs.mesa}/share/glvnd/egl_vendor.d/50_mesa.json";

          # One-command runners. Each must be started from the fincl
          # repository root (the directory holding fincl.asd), because the
          # shell's CL_SOURCE_REGISTRY points at $PWD.
          requireRepoRoot = name: ''
            if [ ! -f fincl.asd ]; then
              echo "${name}: run from the fincl repository root (where fincl.asd is)" >&2
              exit 1
            fi
          '';

          # Regenerates the HTML manual in docs/api/ (docgen/docs.lisp).
          #
          #   nix develop .#docs -c fincl-docs
          fincl-docs = pkgs.writeShellScriptBin "fincl-docs" ''
            set -euo pipefail
            ${requireRepoRoot "fincl-docs"}
            # docgen/fincl-docgen.asd defines fincl/docgen, which ASDF does
            # not find by a registry scan, so it is loaded by path first.
            exec sbcl --non-interactive \
              --eval '(asdf:load-asd (truename "docgen/fincl-docgen.asd"))' \
              --eval '(ql:quickload :fincl/docgen)' \
              --eval '(mgl-pax:update-asdf-system-html-docs fincl/docgen::@fincl-manual :fincl :target-dir #p"docs/api/")'
          '';

          # Runs every benchmark group against QuantLib (benchmarks/).
          #
          #   nix develop .#benchmarks -c fincl-bench
          fincl-bench = pkgs.writeShellScriptBin "fincl-bench" ''
            set -euo pipefail
            ${requireRepoRoot "fincl-bench"}
            exec sbcl --non-interactive \
              --eval '(ql:quickload :fincl/benchmarks)' \
              --eval '(fincl/benchmarks:run-benchmarks)'
          '';

          # Runs the full test suite (about 17 minutes). Exits nonzero when
          # a check fails, because asdf:test-system signals an error.
          #
          #   nix develop .#tests -c fincl-tests
          fincl-tests = pkgs.writeShellScriptBin "fincl-tests" ''
            set -euo pipefail
            ${requireRepoRoot "fincl-tests"}
            exec sbcl --non-interactive \
              --eval '(ql:quickload :fincl)' \
              --eval '(asdf:test-system :fincl)'
          '';
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

              # Plotting for fincl-viz (Plotly JSON -> viz/render.py).
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
              export FINCL_VIZ_EGL_VENDOR="${eglVendor}"

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

          # The default shell plus Clozure CL and ECL, to load fincl on an
          # implementation other than SBCL. Opt-in:
          #
          #   nix develop .#portability -c ccl
          portability = pkgs.mkShell {
            name = "fincl-portability";
            inputsFrom = [ self.devShells.${pkgs.system}.default ];
            packages = [ pkgs.ccl pkgs.ecl pkgs.abcl ];
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
            packages = [ pkgs.quantlib pkgs.boost pkgs.gcc fincl-bench ];
            LD_LIBRARY_PATH = nativeLibPath;
          };

          # Regenerates the HTML manual:
          #
          #   nix develop .#docs -c fincl-docs
          docs = pkgs.mkShell {
            name = "fincl-docs";
            inputsFrom = [ self.devShells.${pkgs.system}.default ];
            packages = [ fincl-docs ];
          };

          # Runs the full test suite:
          #
          #   nix develop .#tests -c fincl-tests
          tests = pkgs.mkShell {
            name = "fincl-tests";
            inputsFrom = [ self.devShells.${pkgs.system}.default ];
            packages = [ fincl-tests ];
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
