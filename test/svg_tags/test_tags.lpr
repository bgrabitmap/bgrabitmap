program TestSVGTags;
{$mode objfpc}{$H+}
uses Classes, SysUtils, DOM, BGRABitmapTypes, BGRASVG, BGRASVGType,
  BGRASVGShapes;
procedure Check(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;
procedure CheckParsedTags;
const Tags: array[0..3] of String = ('polygon','polyline','rect','custom-shape');
var SVG: TBGRASVG; Shape: TSVGElement; I: Integer;
begin
  SVG:=TBGRASVG.CreateFromString('<svg xmlns="http://www.w3.org/2000/svg">'+
    '<polygon points="0,0 10,0 0,10"/><polyline points="0,0 10,0 0,10"/>'+
    '<rect width="10" height="10"/><custom-shape/></svg>');
  try
    Check(SVG.Content.ElementCount=4,'parsed element count');
    for I:=0 to High(Tags) do
    begin
      Shape:=SVG.Content.Element[I];
      Check(Shape.ActualDOMTag=Tags[I],'actual parsed tag '+Tags[I]);
      Check(Shape.ActualDOMTag=String(Shape.DOMElement.TagName),'DOM node agrees');
    end;
    Check(SVG.Content.Element[0] is TSVGPolypoints,'polygon class');
    Check(SVG.Content.Element[1] is TSVGPolypoints,'polyline class');
    Check(TSVGPolypoints(SVG.Content.Element[0]).closed,'polygon is closed');
    Check(not TSVGPolypoints(SVG.Content.Element[1]).closed,'polyline is open');
    Check(TSVGRectangle.GetDOMTag='rect','class factory tag stays available');
  finally SVG.Free; end;
end;
procedure CheckConstructors;
var Doc: TXMLDocument; Shape: TSVGPolypoints; Closed: Boolean;
begin
  Doc:=TXMLDocument.Create;
  try
    for Closed:=False to True do
    begin
      Shape:=TSVGPolypoints.Create(Doc,nil,Closed,nil);
      try
        Check(Shape.closed=Closed,'constructor closure');
        if Closed then Check(Shape.ActualDOMTag='polygon','created polygon tag')
        else Check(Shape.ActualDOMTag='polyline','created polyline tag');
        Shape.points:='0,0 10,0 0,10';
        Check(Length(Shape.pointsF)=3,'created shape retains points');
      finally Shape.Free; end;
    end;
  finally Doc.Free; end;
end;
procedure CheckAppendAndRoundTrip;
var SVG, Reloaded: TBGRASVG; Stream: TMemoryStream; Shape: TSVGPolypoints;
  I, PolygonCount: Integer;
begin
  SVG:=TBGRASVG.Create;
  Stream:=TMemoryStream.Create;
  try
    Shape:=SVG.Content.AppendPolygon([Single(0),0,10,0,0,10]);
    Check(Shape.ActualDOMTag='polygon','float-coordinate AppendPolygon');
    Shape:=SVG.Content.AppendPolygon([PointF(0,0),PointF(10,0),PointF(0,10)]);
    Check(Shape.ActualDOMTag='polygon','point-coordinate AppendPolygon');
    SVG.SaveToStream(Stream);
    Stream.Position:=0;
    Reloaded:=TBGRASVG.Create(Stream);
    try
      PolygonCount:=0;
      // XML serialization introduces whitespace text nodes.
      for I:=0 to Reloaded.Content.ElementCount-1 do
        if Reloaded.Content.IsSVGElement[I] then
        begin
          Check(Reloaded.Content.Element[I].ActualDOMTag='polygon','round-trip tag');
          Check(Length(TSVGPolypoints(Reloaded.Content.Element[I]).pointsF)=3,'round-trip points');
          Inc(PolygonCount);
        end;
      Check(PolygonCount=2,'round-trip polygon count');
    finally Reloaded.Free; end;
  finally Stream.Free; SVG.Free; end;
end;
begin
  CheckParsedTags;
  CheckConstructors;
  CheckAppendAndRoundTrip;
  WriteLn('SVG tags: all checks passed');
end.