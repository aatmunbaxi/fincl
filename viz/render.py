#!/usr/bin/env python3
"""Render a Plotly figure saved as JSON, such as the files fincl-viz writes.

    render.py FIGURE.json                 open the figure in a browser
    render.py FIGURE.json -o OUT.png      write OUT; its extension gives the
                                          format (.html .png .svg .pdf .jpeg .webp)
"""
import argparse
import json
import os
import sys
import warnings

import plotly.graph_objects as go
import plotly.io as pio


def configure_kaleido():
    """Kaleido 0.2 starts Chromium with the GPU disabled, leaving no WebGL for
    3-D traces: give it EGL, backed by Mesa's software renderer when the dev
    shell names it in FINCL_VIZ_EGL_VENDOR."""
    import kaleido

    if not kaleido.__version__.startswith("0."):
        return
    warnings.filterwarnings("ignore", message=r"\s*Support for Kaleido versions less than 1")
    vendor = os.environ.get("FINCL_VIZ_EGL_VENDOR")
    if vendor:
        os.environ.setdefault("__EGL_VENDOR_LIBRARY_FILENAMES", vendor)
        os.environ.setdefault("LIBGL_ALWAYS_SOFTWARE", "1")
    scope = pio.kaleido.scope
    scope.chromium_args = tuple(a for a in scope.chromium_args if a != "--disable-gpu") + ("--use-gl=egl",)


def main():
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("figure", help="the figure's JSON file")
    parser.add_argument("-o", "--output", help="the file to write; none opens a browser")
    parser.add_argument("--width", type=int, help="image width in pixels (default: layout.width)")
    parser.add_argument("--height", type=int, help="image height in pixels (default: layout.height)")
    parser.add_argument("--scale", type=float, default=1, help="image pixel density multiplier")
    parser.add_argument("--cdn", action="store_true", help="HTML loads plotly.js from the CDN")
    args = parser.parse_args()

    with open(args.figure, encoding="utf-8") as f:
        spec = json.load(f)
    config = spec.pop("config", {})
    figure = go.Figure(spec)  # validates every attribute
    if args.output is None:
        figure.show(renderer="browser", config=config)
    elif args.output.lower().endswith((".html", ".htm")):
        figure.write_html(args.output, config=config,
                          include_plotlyjs="cdn" if args.cdn else True)
    else:
        configure_kaleido()
        figure.write_image(args.output, width=args.width, height=args.height, scale=args.scale)


if __name__ == "__main__":
    try:
        main()
    except Exception as e:
        hint = ("\n3-D traces need WebGL: run from the fincl dev shell, which sets FINCL_VIZ_EGL_VENDOR."
                if "error code 525" in str(e) else "")
        print(f"render.py: {e}{hint}", file=sys.stderr)
        sys.exit(1)
