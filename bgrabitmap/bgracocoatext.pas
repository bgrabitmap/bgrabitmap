// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ Internal single-line Cocoa text renderer. Uses the LCL-selected font, but
  shapes and rasterizes with CoreText directly, without a temporary TBitmap.
  No text or glyph cache. Other widgetsets do not depend on Apple frameworks. }
unit BGRACocoaText;
{$mode objfpc}{$H+}
{$i bgrabitmap.inc}
interface
{$IFDEF LCLCOCOA}
uses Types, BGRAClasses, BGRAGraphics, BGRABitmapTypes;
function BGRACocoaTextSupported(AFont: TFont; AOrientation: integer;
  const AText: string): boolean;
function BGRACocoaTextSize(AFont: TFont; const AText: string): TSize;
function BGRACocoaTextFitInfo(AFont: TFont; const AText: string; AMaxWidth: integer): integer;
procedure BGRACocoaTextOut(ADest: TBGRACustomBitmap; AFont: TFont;
  AQuality: TBGRAFontQuality; X,Y: single; const AText: string;
  AColor: TBGRAPixel; ATexture: IBGRAScanner; AAlign: TAlignment);
{$ENDIF}
implementation
{$IFDEF LCLCOCOA}
uses SysUtils, Math, MacOSAll, CocoaAll, CocoaGDIObjects,
  BGRAGrayscaleMask;

function BGRACocoaTextSupported(AFont: TFont; AOrientation: integer;
  const AText: string): boolean;
begin
  { Multiline layout, tabs, rotation and decorations keep their LCL path.
    Higher-level BGRA routines split ordinary multiline text themselves. }
  result := (AOrientation mod 3600 = 0) and
    (AFont.Style * [fsUnderline, fsStrikeOut] = []) and
    (AFont.Quality <> fqNonAntialiased) and
    (Pos(#10,AText) = 0) and (Pos(#13,AText) = 0) and (Pos(#9,AText) = 0);
end;

function NativeFont(AFont: TFont): CTFontRef;
begin
  { TFont owns this font. Do not change the LCL font or its shared attributes. }
  result := CTFontRef(TCocoaFont(Pointer(AFont.Handle)).Font);
end;

function MakeAttributedText(AFont: TFont; const AText: string): CFAttributedStringRef;
var s: CFStringRef; attrs: CFDictionaryRef;
  keys, values: array[0..1] of pointer;
begin
  s := CFStringCreateWithBytes(nil, PByte(PChar(AText)), Length(AText),
    kCFStringEncodingUTF8, false);
  if s = nil then raise Exception.Create('Invalid UTF-8 text');
  try
    keys[0] := kCTFontAttributeName;
    values[0] := NativeFont(AFont);
    keys[1] := kCTForegroundColorFromContextAttributeName;
    values[1] := kCFBooleanTrue;
    attrs := CFDictionaryCreate(nil, @keys[0], @values[0], 2,
      @kCFTypeDictionaryKeyCallBacks, @kCFTypeDictionaryValueCallBacks);
    if attrs = nil then raise Exception.Create('Cannot create CoreText attributes');
    try result := CFAttributedStringCreate(nil, s, attrs);
    finally CFRelease(attrs) end;
    if result = nil then raise Exception.Create('Cannot create CoreText text');
  finally CFRelease(s) end;
end;

function MakeLine(AFont: TFont; const AText: string): CTLineRef;
var s: CFAttributedStringRef;
begin
  s := MakeAttributedText(AFont, AText);
  try result := CTLineCreateWithAttributedString(s);
  finally CFRelease(s) end;
  if result = nil then raise Exception.Create('Cannot create CoreText line');
end;

function LineSize(ALine: CTLineRef): TSize;
var a,d,l: CGFloat; w: double;
begin
  w := CTLineGetTypographicBounds(ALine, @a, @d, @l);
  result := Types.Size(Round(w), Round(a+Abs(d)+l));
end;

function BGRACocoaTextSize(AFont: TFont; const AText: string): TSize;
var line: CTLineRef;
begin
  line := MakeLine(AFont, AText);
  try result := LineSize(line);
  finally CFRelease(line) end;
end;

function BGRACocoaTextFitInfo(AFont: TFont; const AText: string; AMaxWidth: integer): integer;
var text: CFAttributedStringRef; setter: CTTypesetterRef;
  s: CFStringRef; count,i: CFIndex; ch: UniChar;
  prefix: CFAttributedStringRef; line: CTLineRef; fits: boolean;
begin
  result := 0;
  if (AMaxWidth <= 0) or (AText = '') then exit;
  text := MakeAttributedText(AFont, AText);
  try
    setter := CTTypesetterCreateWithAttributedString(text);
    if setter = nil then raise Exception.Create('Cannot create CoreText typesetter');
    try count := CTTypesetterSuggestClusterBreak(setter, 0, AMaxWidth);
    finally CFRelease(setter) end;
    s := CFAttributedStringGetString(text);
    { CoreText may suggest the first cluster even when it is wider than the
      available space. Re-shaping a prefix can also change its final ligature.
      Check the actual prefix width before reporting that it fits. }
    while count > 0 do
    begin
      prefix := CFAttributedStringCreateWithSubstring(nil,text,CFRangeMake(0,count));
      try
        line := CTLineCreateWithAttributedString(prefix);
        try fits := Round(CTLineGetTypographicBounds(line,nil,nil,nil)) <= AMaxWidth;
        finally CFRelease(line) end;
      finally CFRelease(prefix) end;
      if fits then break;
      count := CFStringGetRangeOfComposedCharactersAtIndex(s,count-1).location;
    end;
    { CoreText returns UTF-16 units; the BGRA word-break code expects Unicode
      codepoints, not bytes or UTF-16 surrogate halves. }
    i := 0;
    while i < count do
    begin
      ch := CFStringGetCharacterAtIndex(s, i);
      Inc(i);
      if (ch >= $D800) and (ch <= $DBFF) and (i < count) then Inc(i);
      Inc(result);
    end;
  finally CFRelease(text) end;
end;

procedure BGRACocoaTextOut(ADest: TBGRACustomBitmap; AFont: TFont;
  AQuality: TBGRAFontQuality; X,Y: single; const AText: string;
  AColor: TBGRAPixel; ATexture: IBGRAScanner; AAlign: TAlignment);
var line: CTLineRef; sz: TSize; ink: CGRect; ascent,baseline,px,py: double;
  sx,left,top,marginX,marginY,padL,padR,padT,padB,shapeW: integer;
  temp: TBGRACustomBitmap; mask: TGrayscaleMask; ctx: CGContextRef;
  space: CGColorSpaceRef; info: CGBitmapInfo;
begin
  if (AText = '') or ((AColor.alpha = 0) and (ATexture = nil)) then exit;
  line := MakeLine(AFont, AText);
  temp := nil; mask := nil;
  try
    sz := LineSize(line);
    if (sz.cx = 0) or (sz.cy = 0) then exit;
    case AAlign of
      taCenter: X := X-sz.cx/2;
      taRightJustify: X := X-sz.cx;
    end;
    if AQuality in [fqFineClearTypeRGB,fqFineClearTypeBGR] then sx := 3 else sx := 1;
    ascent := CTFontGetAscent(NativeFont(AFont));
    marginX := sz.cy*sx; marginY := 1;
    left := Math.Floor(X)-sz.cy; top := Math.Floor(Y)-1;
    shapeW := (sz.cx+sz.cy)*sx;
    sz := Types.Size(shapeW, sz.cy+1);
    { Include overhangs, fallback accents and descenders, with a one-pixel
      rasterization guard. Padding must keep RGB/BGR triplets aligned. }
    ink := CTLineGetBoundsWithOptions(line, kCTLineBoundsUseGlyphPathBounds);
    px := (X-Math.Floor(X))*sx+marginX;
    baseline := Y-Math.Floor(Y)+marginY+ascent;
    padL := Math.Max(0,1-Math.Floor(px+ink.origin.x*sx));
    padR := Math.Max(0,Math.Ceil(px+(ink.origin.x+ink.size.width)*sx)+1-sz.cx);
    padT := Math.Max(0,1-Math.Floor(baseline-ink.origin.y-ink.size.height));
    padB := Math.Max(0,Math.Ceil(baseline-ink.origin.y)+1-sz.cy);
    padL := ((padL+sx-1) div sx)*sx;
    padR := ((padR+sx-1) div sx)*sx;
    Inc(sz.cx,padL+padR); Inc(sz.cy,padT+padB);
    Inc(marginX,padL); Inc(marginY,padT);
    Dec(left,padL div sx); Dec(top,padT);
    temp := BGRABitmapFactory.Create(sz.cx,sz.cy,BGRABlack);
    space := CGColorSpaceCreateDeviceRGB;
    if space = nil then raise Exception.Create('Cannot create CoreText color space');
    try
      if TBGRAPixel_RGBAOrder then info := kCGImageAlphaNoneSkipLast or kCGBitmapByteOrder32Big
      else info := kCGImageAlphaNoneSkipFirst or kCGBitmapByteOrder32Little;
      ctx := CGBitmapContextCreate(temp.Data,sz.cx,sz.cy,8,sz.cx*4,space,info);
      if ctx = nil then raise Exception.Create('Cannot create CoreText bitmap context');
      try
        CGContextSetTextMatrix(ctx,CGAffineTransformIdentity);
        CGContextSetShouldAntialias(ctx,1);
        CGContextSetShouldSmoothFonts(ctx,0);
        CGContextSetRGBFillColor(ctx,1,1,1,1);
        { Native optical size and native vertical antialiasing. Only fine
          RGB/BGR is oversampled, horizontally; no vertical resampling. }
        CGContextScaleCTM(ctx,sx,1);
        px := ((X-Math.Floor(X))*sx+marginX)/sx;
        py := sz.cy-(Y-Math.Floor(Y)+marginY)-ascent;
        CGContextSetTextPosition(ctx,px,py);
        CTLineDraw(line,ctx);
        CGContextFlush(ctx);
      finally CGContextRelease(ctx) end;
    finally CGColorSpaceRelease(space) end;
    mask := TGrayscaleMask.Create(temp,cGreen);
    FreeAndNil(temp);
    if sx = 3 then
      BGRAFillClearTypeGrayscaleMask(ADest,left,top,0,mask,AColor,ATexture,
        AQuality=fqFineClearTypeRGB)
    else if ATexture <> nil then
      ADest.FillMask(left,top,mask,ATexture,dmLinearBlend)
    else
      ADest.FillMask(left,top,mask,AColor,dmLinearBlend);
  finally mask.Free; temp.Free; CFRelease(line) end;
end;
{$ENDIF}
end.
