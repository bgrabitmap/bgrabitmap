// SPDX-License-Identifier: LGPL-3.0-linking-exception
procedure BitmapTextOut(ABitmap: TBitmap; ACoord: TPoint; AText: string);
var sUTF16: UnicodeString;
begin 
  sUTF16 := UTF8ToUTF16(AText);
  inc(ACoord.Y, ABitmap.Canvas.Font.GlyphHeight-1);
  ABitmap.Canvas.DrawString(sUTF16, ACoord);
end;

procedure BitmapTextOutAngle(ABitmap: TBitmap; ACoord: TPoint; AText: string; AOrientation: integer);
var sUTF16: UnicodeString;
  angleRad: single;
  ofs, center: TPointF;
begin
  angleRad := AOrientation*Pi/1800;
  ofs := PointF(ACoord.X, ACoord.Y) +
         PointF(sin(angleRad), cos(angleRad))*(ABitmap.Canvas.Font.GlyphHeight-1);
  sUTF16 := UTF8ToUTF16(AText);
  center := PointF(ABitmap.Canvas.GetStringWidth(sUTF16)/2, 
                   ABitmap.Canvas.Font.Height/2);
  ABitmap.Canvas.DrawString(sUTF16, Point(round(ofs.X), round(ofs.Y)), 
                            nil, false, angleRad);
end;

procedure BitmapTextRect(ABitmap: TBitmap; ARect: TRect; ACoord: TPoint; 
  AText: string; const AStyle: TTextStyle);
var sUTF16: UnicodeString;
begin 
  sUTF16 := UTF8ToUTF16(AText);
  case AStyle.Alignment of
  taRightJustify: ACoord.X := ARect.Right - ABitmap.Canvas.GetStringWidth(sUTF16);
  taCenter: ACoord.X := ARect.Left + (ARect.Width - ABitmap.Canvas.GetStringWidth(sUTF16)) div 2;
  end;
  case AStyle.Layout of
  tlBottom: ACoord.Y := ARect.Bottom - ABitmap.Canvas.Font.Height;
  tlCenter: ACoord.Y := ARect.Top + (ARect.Height - ABitmap.Canvas.Font.Height) div 2;
  end;
  inc(ACoord.Y, ABitmap.Canvas.Font.GlyphHeight-1);
  ABitmap.Canvas.DrawString(sUTF16, ACoord);
end;

function BitmapTextExtent(ABitmap: TBitmap; AText: string): TSize;
var sUTF16: UnicodeString;
begin
  sUTF16 := UTF8ToUTF16(AText);
  result := Size(ABitmap.Canvas.GetStringWidth(sUTF16), ABitmap.Canvas.Font.Height);
end;

function BitmapTextExtentAngle(ABitmap: TBitmap; AText: string; AOrientation: integer): TSize;
begin
  result := BitmapTextExtent(ABitmap, AText);
end;

function BitmapTextFitInfo(ABitmap: TBitmap; AText: string; AMaxWidth: integer): integer;
var 
  bytePos, charLen: integer;  
  sUTF16: UnicodeString; 
begin
  result := 0;
  bytePos := 1;
  while bytePos <= length(AText) do
  begin
    charLen := UTF8CharacterLength(@AText[bytePos]);
    sUTF16 := UTF8ToUTF16(copy(AText, 1, bytePos+charLen-1));
    if ABitmap.Canvas.GetStringWidth(sUTF16) > AMaxWidth then break;
    inc(result);
    inc(bytePos,charLen);
  end;
end;

function BitmapTextFitInfoAngle(ABitmap: TBitmap; AText: string; 
  AMaxWidth: integer; AOrientation: integer): integer;
begin
  result := BitmapTextFitInfo(ABitmap, AText, AMaxWidth);
end;

procedure BitmapFillRect(ABitmap: TBitmap; ARect: TRect; AColor: TColor);
begin
  ABitmap.Canvas.FillRect(ARect, AColor);
end;
