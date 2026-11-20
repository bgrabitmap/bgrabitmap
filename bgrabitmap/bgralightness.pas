// SPDX-License-Identifier: LGPL-3.0-linking-exception

{ Lightness calculations used by 3D rendering. Kept independent of
  BGRABitmapTypes to avoid circular dependencies involving assembler routines. }
unit BGRALightness;

{$mode objfpc}{$H+}

interface

uses BGRAClasses;

{ A value of 32768 is neutral. The result may exceed 65535. }
function CombineLightness3D(lightness1,lightness2: Int32or64): Int32or64; inline;

implementation

function CombineLightness3D(lightness1,lightness2: Int32or64): Int32or64;
{$ifdef CPUI386} {$asmmode intel} assembler;
  asm
    imul edx
    shl edx, 17
    shr eax, 15
    or edx, eax
    mov result, edx
  end;
{$ELSE}
begin
  if (lightness1 < 0) xor (lightness2 < 0) then
    result := -(int64(-lightness1)*lightness2 shr 15)
  else
    result := int64(lightness1)*lightness2 shr 15;
end;
{$ENDIF}

end.
