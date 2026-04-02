// SPDX-License-Identifier: LGPL-3.0-linking-exception
{*******************************************************************************

 (c) 2025 - Massimo Magnano

********************************************************************************

 PDF Document that allows you to specify ColorSpace and BitPerComponent of an image
}

unit BGRAPDF;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, fpPDF;

type
  TPDFColorSpace = (
    csDeviceCMYK, //Device-dependent names
    csDeviceGray,
    csDeviceN,
    csDeviceRGB,
    csCalGray,     //Device-independent names
    csCalRGB,
    csLab,
    csICCBased,
    csIndexed,     //Special names
    csPattern,
    csSeparation);

  { TBGRAPDFImageItem }

  TBGRAPDFImageItem = class(TPDFImageItem)
  private
    FBitsPerComponent: Integer;
    FColorSpace: TPDFColorSpace;
  public
    constructor Create(ACollection: TCollection); override;

    property ColorSpace: TPDFColorSpace read FColorSpace write FColorSpace;
    property BitsPerComponent: Integer read FBitsPerComponent write FBitsPerComponent;
  end;

  { TBGRAPDFDocument }

  TBGRAPDFDocument = class(TPDFDocument)
  protected
    function CreatePDFImages: TPDFImages; override;
    procedure CreateImageEntry(ImgWidth, ImgHeight, NumImg: integer;
                               out ImageDict: TPDFDictionary); override;

  end;

const
  PDFColorSpace : array[TPDFColorSpace] of String = (
      'DeviceCMYK', //Device-dependent names
      'DeviceGray',
      'DeviceN',
      'DeviceRGB',
      'CalGray',     //Device-independent names
      'CalRGB',
      'Lab',
      'ICCBased',
      'Indexed',     //Special names
      'Pattern',
      'Separation');


implementation

type
    //MaxM: Only to have access to protected methods,
    //      What's the point of making Add methods protected remains a mystery?
    TBGRAPDFDictionary = class(TPDFDictionary)
    end;

{ TBGRAPDFImageItem }

constructor TBGRAPDFImageItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);

  FColorSpace:= csDeviceRGB;
  FBitsPerComponent:= 8;
end;

{ TBGRAPDFDocument }

function TBGRAPDFDocument.CreatePDFImages: TPDFImages;
begin
  Result:=TPDFImages.Create(Self, TBGRAPDFImageItem);
end;

procedure TBGRAPDFDocument.CreateImageEntry(ImgWidth, ImgHeight, NumImg: integer;
                                            out ImageDict: TPDFDictionary);
var
  N: TPDFName;
  ADict: TPDFDictionary;
  i: integer;
  lXRef: integer;
  curImg: TBGRAPDFImageItem;

begin
  //MaxM: There is no way to Replace ColorSpace Dictionary Item, the Name property is readonly for no reason
  //      So we copy the entire code from fpc source

  curImg:= TBGRAPDFImageItem(Images[NumImg]);

  lXRef := GlobalXRefCount; // reference to be used later

  ImageDict:=CreateGlobalXRef.Dict;
  TBGRAPDFDictionary(ImageDict).AddName('Type','XObject');
  TBGRAPDFDictionary(ImageDict).AddName('Subtype','Image');
  TBGRAPDFDictionary(ImageDict).AddInteger('Width',ImgWidth);
  TBGRAPDFDictionary(ImageDict).AddInteger('Height',ImgHeight);

  if (curImg <> nil)
  then begin
         TBGRAPDFDictionary(ImageDict).AddName('ColorSpace', PDFColorSpace[curImg.FColorSpace]);
         TBGRAPDFDictionary(ImageDict).AddInteger('BitsPerComponent', curImg.FBitsPerComponent);
       end
  else begin
         TBGRAPDFDictionary(ImageDict).AddName('ColorSpace','DeviceRGB');
         TBGRAPDFDictionary(ImageDict).AddInteger('BitsPerComponent',8);
       end;

  N:=CreateName('I'+IntToStr(NumImg)); // Needed later
  TBGRAPDFDictionary(ImageDict).AddElement('Name',N);

  // now find where we must add the image xref - we are looking for "Resources"
  for i := 1 to GlobalXRefCount-1 do
  begin
    ADict:=GlobalXRefs[i].Dict;
    if ADict.ElementCount > 0 then
    begin
      if (ADict.Values[0] is TPDFName) and ((ADict.Values[0] as TPDFName).Name='Page') then
      begin
        ADict:=ADict.ValueByName('Resources') as TPDFDictionary;
        ADict:=TPDFDictionary(ADict.FindValue('XObject'));
        if Assigned(ADict) then
        begin
          TBGRAPDFDictionary(ADict).AddReference(N.Name, lXRef);
        end;
      end;
    end;
  end;
end;


end.

