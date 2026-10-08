program TestWebPLoader;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}cthreads, cwstring,{$ENDIF}
  SysUtils, libwebp;
var
  Pixels, Decoded: array[0..15] of Byte;
  Encoded: PByte;
  EncodedSize: NativeUInt;
  x, y, i: Integer;
procedure Require(Value: Boolean; const Message: string);
begin
  if not Value then raise Exception.Create(Message);
end;
begin
  try
    if ParamCount > 0 then LibWebPFilename := ParamStr(1);
    Require(LibWebPLoad, 'Default libwebp could not be found');
    try
      Require(Assigned(WebPGetDecoderVersion) and Assigned(WebPEncodeLosslessRGBA) and
        Assigned(WebPDecodeRGBAInto) and Assigned(WebPFree), 'Required WebP symbols missing');
      Require(LibWebPLoad and (LibWebPRefCount = 2), 'Repeated load reference count failed');
      LibWebPUnload;
      Require(LibWebPLoaded and (LibWebPRefCount = 1), 'Premature unload');
      for i := 0 to 3 do
      begin
        Pixels[i * 4] := 40 + i * 30;
        Pixels[i * 4 + 1] := 20 + i * 10;
        Pixels[i * 4 + 2] := 200 - i * 20;
        Pixels[i * 4 + 3] := 255;
      end;
      Encoded := nil;
      EncodedSize := WebPEncodeLosslessRGBA(@Pixels[0], 2, 2, 8, Encoded);
      Require((EncodedSize > 0) and Assigned(Encoded), 'Lossless encoding failed');
      try
        Require(WebPGetInfo(Encoded, EncodedSize, @x, @y) <> 0, 'Encoded image info missing');
        Require((x = 2) and (y = 2), 'Dimensions changed');
        Require(WebPDecodeRGBAInto(Encoded, EncodedSize, @Decoded[0], SizeOf(Decoded), 8) <> nil, 'Decoding failed');
        for i := 0 to High(Pixels) do Require(Pixels[i] = Decoded[i], 'Lossless pixel round trip failed');
      finally WebPFree(Encoded); end;
      WriteLn('PASS: automatic library load, reference counting, WebP lossless round trip; decoder=', WebPGetDecoderVersion());
    finally LibWebPUnload; end;
    Require(not LibWebPLoaded, 'Final unload failed');
    Require(not LibWebPLoad('/nonexistent-codex-test/libwebp.so'), 'Explicit missing path unexpectedly loaded');
  except
    on E: Exception do
    begin
      WriteLn('FAIL: ', E.Message);
      Halt(1);
    end;
  end;
end.
