// SPDX-License-Identifier: LGPL-3.0-linking-exception
{$IFDEF INCLUDE_INTERFACE}
{$UNDEF INCLUDE_INTERFACE}
type
  TColor = TfpgColor;

  { Raw image for fpGUI }
  TRawImage = class(TfpgImage)
    procedure BGRASetSizeAndTransparency(AWidth,AHeight: Integer; ATransparent: boolean);
    procedure Assign(AOther: TObject); virtual;
  end;
  TGUICanvas = TfpgCanvas;

const
  clNone = fpg_base.clNone;
  clBlack = fpg_base.clBlack;
  clWhite = fpg_base.clWhite;

function clRgbBtnHighlight: TColor;
function clRgbBtnShadow: TColor;
function ColorToRGB(c: TColor): TColor; inline;
function GetScreenDPIX: integer;
function GetScreenDPIY: integer;

{$ENDIF}

{$IFDEF INCLUDE_IMPLEMENTATION}
{$UNDEF INCLUDE_IMPLEMENTATION}

procedure TRawImage.BGRASetSizeAndTransparency(AWidth,AHeight: Integer; ATransparent: boolean);  
var
  tempData: pointer;
begin
  if (Width <> AWidth) or (Height <> AHeight) then
  begin
    AllocateImage(32,AWidth,AHeight);
    if ATransparent then AllocateMask;
  end else
  begin
    if ATransparent and not Masked then AllocateMask else
    if not ATransparent and Masked then
    begin
      getmem(tempData, ImageDataSize);
      if tempData <> nil then
      begin
        move(ImageData^, tempData^, ImageDataSize);
        FreeImage;
        AllocateImage(32,AWidth,AHeight);
        move(tempData^, ImageData^, ImageDataSize);
        freemem(tempData);
      end;
    end;
  end;
end;

procedure TRawImage.Assign(AOther: TObject);
var
  src: TRawImage;
begin
  if AOther is TRawImage then
  begin
    src := TRawImage(AOther);
    BGRASetSizeAndTransparency(src.Width, src.Height, src.Masked);
    move(src.ImageData^, ImageData^, ImageDataSize);
    if Masked then
      move(src.MaskData^, MaskData^, MaskDataSize);
  end else
    raise exception.Create('Unable to assign');
end;

function clRgbBtnHighlight: TColor;
begin
  result := fpgColorToRGB(fpg_base.clHilite2);
end;

function clRgbBtnShadow: TColor;
begin
  result := fpgColorToRGB(fpg_base.clShadow2);
end;

function ColorToRGB(c: TColor): TColor; inline;
begin
  result := fpgColorToRGB(c);
end;

function GetScreenDPIX: integer;
begin
  result := fpgApplication.Screen_dpi_x;
end;

function GetScreenDPIY: integer;
begin
  result := fpgApplication.Screen_dpi_y;
end;

{$ENDIF}
