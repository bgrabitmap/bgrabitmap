program test_svguse;
{$mode objfpc}{$H+}
uses {$ifdef UNIX}cthreads, cwstring,{$endif}
  Classes, SysUtils, DOM, BGRABitmap, BGRABitmapTypes, BGRACanvas2D,
  BGRASVG, BGRASVGType, BGRASVGShapes, BGRATransform;
var Checks, Failures: integer;

procedure Check(OK: boolean; const Name: string);
begin
  Inc(Checks);
  if not OK then begin Inc(Failures); WriteLn('FAIL ', Name); end;
end;

function Document(const Body: string): TBGRASVG;
begin
  result := TBGRASVG.CreateFromString('<svg xmlns="http://www.w3.org/2000/svg" '+
    'xmlns:xlink="http://www.w3.org/1999/xlink" width="100" height="100">'+Body+'</svg>');
end;

procedure PixelCase(const Name, Body: string; X,Y: integer; Color: TBGRAPixel);
var svg: TBGRASVG; bmp: TBGRABitmap; ctx: TBGRACanvas2D; pass: integer;
begin
  svg := Document(Body);
  bmp := TBGRABitmap.Create(100,100,BGRAPixelTransparent);
  ctx := TBGRACanvas2D.Create(bmp);
  try
    for pass := 1 to 2 do
    begin
      bmp.Fill(BGRAPixelTransparent);
      svg.Draw(ctx,0,0);
      Check(bmp.GetPixel(X,Y)=Color, Name+' draw '+IntToStr(pass));
      Check(ctx.matrix=AffineMatrixIdentity, Name+' restores canvas transform');
    end;
  finally ctx.Free; bmp.Free; svg.Free; end;
end;

procedure TestUse;
const Shape = '<defs><rect id="r" width="20" height="20"/></defs>';
begin
  PixelCase('xlink translation',Shape+'<use xlink:href="#r" x="20" y="25" fill="red"/>',30,35,BGRA(255,0,0));
  PixelCase('href translation',Shape+'<use href="#r" x="20" y="25" fill="red"/>',30,35,BGRA(255,0,0));
  PixelCase('outside translated shape',Shape+'<use href="#r" x="20" y="25" fill="red"/>',5,5,BGRAPixelTransparent);
  PixelCase('independent instance red',Shape+'<use href="#r" x="10" fill="red"/><use href="#r" x="50" fill="blue"/>',20,10,BGRA(255,0,0));
  PixelCase('independent instance blue',Shape+'<use href="#r" x="10" fill="red"/><use href="#r" x="50" fill="blue"/>',60,10,BGRA(0,0,255));
  PixelCase('referenced style wins','<defs><rect id="r" width="20" height="20" fill="blue"/></defs><use href="#r" fill="red"/>',10,10,BGRA(0,0,255));
  PixelCase('nested use','<defs><rect id="r" width="20" height="20"/><use id="u" href="#r" x="10"/></defs><use href="#u" x="20" y="20" fill="red"/>',40,30,BGRA(255,0,0));
  PixelCase('group transform','<defs><g id="g" fill="red" transform="translate(5,6)"><rect width="20" height="20"/></g></defs><use href="#g" x="20" y="20"/>',30,32,BGRA(255,0,0));
  PixelCase('use transform',Shape+'<use href="#r" x="10" y="10" fill="red" transform="scale(2)"/>',30,30,BGRA(255,0,0));
  PixelCase('href priority','<defs><rect id="a" width="20" height="20" fill="red"/><rect id="b" width="20" height="20" fill="blue"/></defs><use href="#a" xlink:href="#b"/>',10,10,BGRA(255,0,0));
  PixelCase('empty href priority',Shape+'<use href="" xlink:href="#r" fill="red"/>',10,10,BGRAPixelTransparent);
  PixelCase('missing reference','<use href="#missing"/><rect x="70" y="70" width="20" height="20" fill="red"/>',80,80,BGRA(255,0,0));
  PixelCase('self cycle','<use id="a" href="#a"/><rect x="70" y="70" width="20" height="20" fill="red"/>',80,80,BGRA(255,0,0));
  PixelCase('indirect root cycle','<use id="a" href="#b"/><use id="b" href="#a"/><rect x="70" y="70" width="20" height="20" fill="red"/>',80,80,BGRA(255,0,0));
  PixelCase('defs cycle','<defs><use id="a" href="#b"/><use id="b" href="#a"/></defs><use href="#a"/><rect x="70" y="70" width="20" height="20" fill="red"/>',80,80,BGRA(255,0,0));
  PixelCase('ancestor cycle','<g id="g"><use href="#g"/></g><rect x="70" y="70" width="20" height="20" fill="red"/>',80,80,BGRA(255,0,0));
end;

procedure TestBounds;
var svg: TBGRASVG; bmp: TBGRABitmap; ctx: TBGRACanvas2D; b: TRectF; i: integer;
  procedure ShapeBounds(const Name, Body: string; L,T,R,Btm: single);
  var shapeSVG: TBGRASVG; bounds: TRectF;
  begin
    shapeSVG:=Document(Body);
    try
      bounds:=shapeSVG.Content.Element[0].GetBoundingBoxInUnit(ctx,cuPixel);
      Check(Abs(bounds.Left-L)<0.001,Name+' bounds left');
      Check(Abs(bounds.Top-T)<0.001,Name+' bounds top');
      Check(Abs(bounds.Right-R)<0.001,Name+' bounds right');
      Check(Abs(bounds.Bottom-Btm)<0.001,Name+' bounds bottom');
    finally shapeSVG.Free; end;
  end;
begin
  bmp:=TBGRABitmap.Create(100,100); ctx:=TBGRACanvas2D.Create(bmp);
  try
    ShapeBounds('rect','<rect x="10" y="15" width="20" height="30"/>',10,15,30,45);
    ShapeBounds('circle','<circle cx="50" cy="50" r="10"/>',40,40,60,60);
    ShapeBounds('ellipse','<ellipse cx="50" cy="50" rx="20" ry="10"/>',30,40,70,60);
    ShapeBounds('line','<line x1="80" y1="20" x2="10" y2="90"/>',10,20,80,90);
    ShapeBounds('polygon','<polygon points="10,20 40,20 30,50"/>',10,20,40,50);
    ShapeBounds('path','<path d="M10 20 L40 20 L30 50 Z"/>',10,20,40,50);
    svg:=Document('<defs><rect id="r" x="10" y="15" width="20" height="30"/></defs><use href="#r" x="20" y="25"/>');
    try
      for i:=1 to 2 do begin
        b:=svg.Content.Element[1].GetBoundingBoxInUnit(ctx,cuPixel);
        Check((b.Left=30) and (b.Top=40) and (b.Right=50) and (b.Bottom=70),'translated use bounds');
      end;
    finally svg.Free; end;
    svg:=Document('<defs><use id="a" href="#b"/><use id="b" href="#a"/></defs><use href="#a"/>');
    try
      for i:=1 to 2 do begin
        b:=svg.Content.Element[1].GetBoundingBoxInUnit(ctx,cuPixel);
        Check((b.Right<=b.Left) or (b.Bottom<=b.Top),'cyclic bounds empty and repeatable');
      end;
    finally svg.Free; end;
    svg:=Document('<use id="a" href="#a"/>');
    try
      b:=svg.Content.Element[0].GetBoundingBoxInUnit(ctx,cuPixel);
      Check((b.Right<=b.Left) or (b.Bottom<=b.Top),'self bounds empty');
    finally svg.Free; end;
  finally ctx.Free; bmp.Free; end;
end;

procedure TestGradients;
const Stops='<stop offset="0" stop-color="red"/><stop offset="1" stop-color="blue"/>';
var svg: TBGRASVG; bmp: TBGRABitmap; ctx: TBGRACanvas2D; p,q: TBGRAPixel;
begin
  bmp:=TBGRABitmap.Create(100,100,BGRAPixelTransparent); ctx:=TBGRACanvas2D.Create(bmp);
  try
    svg:=Document('<defs><linearGradient id="g">'+Stops+'</linearGradient></defs><line x1="10" y1="10" x2="90" y2="90" stroke="url(#g)" stroke-width="8"/>');
    try
      svg.Draw(ctx,0,0); p:=bmp.GetPixel(20,20); q:=bmp.GetPixel(80,80);
      Check((p.alpha=255) and (p.red>p.blue),'diagonal gradient start');
      Check((q.alpha=255) and (q.blue>q.red),'diagonal gradient end');
    finally svg.Free; end;
    bmp.Fill(BGRAPixelTransparent);
    svg:=Document('<defs><linearGradient id="g" gradientUnits="userSpaceOnUse" x1="10" y1="0" x2="90" y2="0">'+Stops+'</linearGradient></defs><line x1="10" y1="50" x2="90" y2="50" stroke="url(#g)" stroke-width="8"/>');
    try
      svg.Draw(ctx,0,0); p:=bmp.GetPixel(20,50); q:=bmp.GetPixel(80,50);
      Check((p.alpha=255) and (p.red>p.blue),'horizontal user-space gradient start');
      Check((q.alpha=255) and (q.blue>q.red),'horizontal user-space gradient end');
    finally svg.Free; end;
    bmp.Fill(BGRAPixelTransparent);
    svg:=Document('<defs><linearGradient id="g" gradientUnits="userSpaceOnUse" x1="0" y1="10" x2="0" y2="90">'+Stops+'</linearGradient></defs><line x1="50" y1="10" x2="50" y2="90" stroke="url(#g)" stroke-width="8"/>');
    try
      svg.Draw(ctx,0,0); p:=bmp.GetPixel(50,20); q:=bmp.GetPixel(50,80);
      Check((p.alpha=255) and (p.red>p.blue),'vertical user-space gradient start');
      Check((q.alpha=255) and (q.blue>q.red),'vertical user-space gradient end');
    finally svg.Free; end;
  finally ctx.Free; bmp.Free; end;
end;

procedure TestLegacyContentConstructor;
var doc:TXMLDocument; root:TDOMElement; content:TSVGContent;
begin
  doc:=TXMLDocument.Create;
  try
    root:=doc.CreateElement('g'); doc.AppendChild(root);
    content:=TSVGContent.Create(root,nil,nil);
    try Check(content.ElementCount=0,'legacy content constructor'); finally content.Free; end;
  finally doc.Free; end;
end;

begin
  TestUse;
  TestBounds;
  TestGradients;
  TestLegacyContentConstructor;
  WriteLn(Checks,' checks, ',Failures,' failures');
  Halt(Ord(Failures<>0));
end.
