"""Write src/math/sobol-directions.sexp: Joe and Kuo's new-joe-kuo-6.21201
direction numbers (QuantLib's JoeKuoD6 initializers, with its primitive
polynomials) for dimensions 2 to 1111.

  python3 -I tests/fixtures/sobol/extract.py QUANTLIB_SOURCE OUTPUT
"""
import re, sys

ql, output = sys.argv[1], sys.argv[2]
DIMENSIONS = 1111

with open(f"{ql}/ql/math/randomnumbers/primitivepolynomials.cpp") as f:
    text = f.read()
polynomials = []   # (degree, a) in QuantLib's order, from degree 1
for degree, body in re.findall(r"PrimitivePolynomialDegree(\d+)\[\]=\{(.*?)\};", text, re.S):
    for value in re.findall(r"^\s*(-?\d+),", re.sub(r"/\*.*?\*/", "", body), re.M):
        if int(value) == -1:
            break
        polynomials.append((int(degree), int(value)))

with open(f"{ql}/ql/math/randomnumbers/sobolrsg.cpp") as f:
    text = f.read()
inits = {int(k): [int(x) for x in v.split(",")]
         for k, v in re.findall(r"dim(\d+)JoeKuoD6Init\[\]\s*=\s*\{([^}]*)\}", text)}

with open(output, "w") as out:
    out.write(";;;; Sobol direction numbers: Joe and Kuo, new-joe-kuo-6.21201\n")
    out.write(";;;; (https://web.maths.unsw.edu.au/~fkuo/sobol/), dimensions 2 to 1111,\n")
    out.write(";;;; as (s a m1 ... ms): the degree s and inner coefficients a of the\n")
    out.write(";;;; primitive polynomial, then the initial direction numbers. Dimension 1\n")
    out.write(";;;; has all m = 1. Extracted from QuantLib's JoeKuoD6 tables by\n")
    out.write(";;;; tests/fixtures/sobol/extract.py.\n;;;;\n")
    out.write(";;;; Copyright (c) 2008, Frances Y. Kuo and Stephen Joe. All rights reserved.\n")
    out.write(";;;; Redistribution and use in source and binary forms, with or without\n")
    out.write(";;;; modification, are permitted under the BSD-style license at the URL above,\n")
    out.write(";;;; which requires this notice and the disclaimer there.\n(\n")
    for d in range(1, DIMENSIONS):
        degree, a = polynomials[d - 1]   # QuantLib dimension d uses the d-th polynomial
        m = inits[d][:-1]            # strip the terminating 0
        assert len(m) == degree, (d, degree, m)
        out.write(f" ({degree} {a} {' '.join(map(str, m))})\n")
    out.write(")\n")
