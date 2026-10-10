# Point operators regression

Checks scalar product `**` and multiplication by a scalar. Point-by-point `*`
is checked according to its actual result type: either the legacy scalar
product or FPC's component-wise product. This does not infer the operator's
meaning solely from FPC_FULLVERSION, since development branches can differ.

This catches mismatched interface/implementation conditionals for FPC 3.2.3
(issue #338). Run with FPC 3.2.2, fixes_3_2 (3.2.3), and main (3.3.1).

Compile in core mode with separate unit directories for each compiler:

```
fpc -dBGRABITMAP_CORE -Fu../../../bgrabitmap -FUlib -FE. test_pointoperators.lpr
./test_pointoperators
```

Create `lib` first. Successful checks print `PASS`; failures return a nonzero
exit code. No graphical interface or Lazarus installation is needed.
