// SPDX-License-Identifier: LGPL-3.0-linking-exception

{ Proposes a class to unzip files from stream to stream }
unit UnzipperExt;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, zipper;

type
  {* Unzipping class with custom input stream that can extract a specific file }
  TUnzipperStreamUtf8 = class(TUnZipper)
    private
      FCustomOutputStream: TStream;
      FCustomInputStream: TStream;
      { Sets the input stream }
      procedure SetInputStream(AValue: TStream);
    protected
      { Callback method to open the ZIP input }
      Procedure CustomOpenInput(Sender: TObject; var AStream: TStream);
      { Callback method to close the ZIP input }
      procedure CustomCloseInput(Sender: TObject; var AStream: TStream);
      { Callback method to create the output for the content of the extracted item.
        This function actually just supplies the chosen output stream. }
      procedure CustomCreateOutput(Sender : TObject; var AStream : TStream; {%H-}AItem : TFullZipFileEntry);
      { Callback method to close the output for the content of the extracted item }
      procedure CustomCloseOutput(Sender : TObject; var AStream : TStream; {%H-}AItem : TFullZipFileEntry);
    public
      {** Creates the unzipper class. You need to set the _InputStream_  ou _Filename_ property afer that. }
      constructor Create;
      {** Unzips the specified item with the given filename into the given stream }
      function UnzipFileToStream(AFilename: string; AStream: TStream; ACaseSensitive: boolean= true): boolean;
      {** Unzips the specified item with the given filename and return it as a string }
      function UnzipFileToString(AFilename:string): string;
      {** Input stream to use when unzipping, otherwise will use the inherited _Filename_ property }
      property InputStream: TStream read FCustomInputStream write SetInputStream;
  end;

implementation

uses BGRAUTF8;

{ TUnzipperStreamUtf8 }

procedure TUnzipperStreamUtf8.SetInputStream(AValue: TStream);
begin
  if FCustomInputStream=AValue then Exit;
  FCustomInputStream:=AValue;
end;

procedure TUnzipperStreamUtf8.CustomOpenInput(Sender: TObject; var AStream: TStream);
begin
  if Assigned(FCustomInputStream) then
    AStream := FCustomInputStream
  else
    AStream := TFileStreamUTF8.Create(FileName, fmOpenRead or fmShareDenyWrite);
end;

procedure TUnzipperStreamUtf8.CustomCloseInput(Sender: TObject; var AStream: TStream);
begin
  if AStream = FCustomInputStream then
    AStream := nil
  else
    FreeAndNil(AStream);
end;

procedure TUnzipperStreamUtf8.CustomCreateOutput(Sender: TObject;
  var AStream: TStream; AItem: TFullZipFileEntry);
begin
  AStream := FCustomOutputStream;
end;

procedure TUnzipperStreamUtf8.CustomCloseOutput(Sender: TObject;
  var AStream: TStream; AItem: TFullZipFileEntry);
begin
  AStream := nil;
end;

function TUnzipperStreamUtf8.UnzipFileToStream(AFilename: string; AStream: TStream;
  ACaseSensitive: boolean): boolean;
var
  i: integer;
  entryName: string;
begin
  OpenInput;
  AFilename := StringReplace(AFilename,'/','\',[rfReplaceAll]);
  Try
    ReadZipDirectory;
    for i := 0 to Entries.count-1 do
    begin
      entryName := Entries.FullEntries[i].ArchiveFileName;
      entryName:= StringReplace(entryName,'/','\',[rfReplaceAll]);
      if (entryName = AFilename) or
        (not ACaseSensitive and (CompareText(entryName,AFilename)=0)) then
      begin
        OnCreateStream := @CustomCreateOutput;
        OnDoneStream := @CustomCloseOutput;
        FCustomOutputStream := AStream;
        UnZipOneFile(Entries.FullEntries[i]);
        OnCreateStream := nil;
        OnDoneStream := nil;
        FCustomOutputStream := nil;
        result := true;
        exit;
      end;
    end;
  Finally
    CloseInput;
  end;
  result := false;
end;

function TUnzipperStreamUtf8.UnzipFileToString(AFilename: string): string;
var mem: TMemoryStream;
begin
  mem := TMemoryStream.Create;
  try
    UnzipFileToStream(AFilename,mem);
    setlength(result,mem.Size);
    mem.Position:= 0;
    mem.Read(result[1], length(result));
  finally
    mem.Free;
  end;
end;

constructor TUnzipperStreamUtf8.Create;
begin
  inherited Create;
  OnOpenInputStream := @CustomOpenInput;
  OnCloseInputStream:= @CustomCloseInput;
end;


end.

