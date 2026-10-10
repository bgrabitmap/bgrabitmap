# Point operators regression

Checks scalar product `**` and multiplication by a scalar. Point-by-point `*`
is checked according to its actual result type: either the legacy scalar
product or FPC's component-wise product. This does not infer the operator's
meaning solely from FPC_FULLVERSION, since development branches can differ.

BGRABitmap supplies the scalar-product overloads only before FPC 3.2.3.
From 3.2.3 onwards it uses FPC's native operators, targeting the current
fixes_3_2 branch. The archived July 2021 svn/fixes_3_2 snapshot lacks both
the native `**` operator and the FPImage resolution API expected by the
library, and is not a supported 3.2.3 baseline.

This catches mismatched interface/implementation conditionals for FPC 3.2.3
(issue #338). Run with FPC 3.2.2, fixes_3_2 (3.2.3),
release_3_2_4-branch (3.2.4), and main (3.3.1).

Compile in core mode with separate unit directories for each compiler:

```
fpc -dBGRABITMAP_CORE -Fu../../../bgrabitmap -FUlib -FE. test_pointoperators.lpr
./test_pointoperators
```

Create `lib` first. Successful checks print `PASS`; failures return a nonzero
exit code. No graphical interface or Lazarus installation is needed.
