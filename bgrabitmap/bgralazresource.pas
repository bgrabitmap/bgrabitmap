unit BGRALazResource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRAMultiFileType;

type
  { TLazResourceEntry }

  TLazResourceEntry = class(TMultiFileEntry)
  private
    procedure Serialize(ADestination: TStream);
  protected
    FName: utf8string;
    FValueType: utf8string;
    FContent: TStream;
    function GetName: utf8string; override;
    procedure SetName(AValue: utf8string); override;
    function GetExtension: utf8string; override;
    function GetFileSize: int64; override;
  public
    constructor Create(AContainer: TMultiFileContainer; AName: utf8string; AValueType: utf8string; AContent: TStream);
    destructor Destroy; override;
    function CopyTo(ADestination: TStream): int64; override;
  end;

  { TFormDataEntry }

  TFormDataEntry = class(TLazResourceEntry)
  protected
    FTextContent: TStream;
    procedure RequireTextContent;
    function GetExtension: utf8string; override;
    function GetFileSize: int64; override;
  public
    constructor Create(AContainer: TMultiFileContainer; AName: utf8string; ABinaryContent: TStream);
    destructor Destroy; override;
    function CopyTo(ADestination: TStream): int64; override;
  end;

  { TLazResourceContainer }

  TLazResourceContainer = class(TMultiFileContainer)
  protected
    function CreateEntry(AName: utf8string; AExtension: utf8string; AContent: TStream): TMultiFileEntry; override;
  public
    procedure LoadFromStream(AStream: TStream); override;
    procedure SaveToStream(ADestination: TStream); override;
  end;

implementation

uses LResources, BGRAUTF8;

{ TFormDataEntry }

procedure TFormDataEntry.RequireTextContent;
begin
  if FTextContent = nil then
  begin
    FTextContent:= TMemoryStream.Create;
    FContent.Position:= 0;
    LRSObjectBinaryToText(FContent, FTextContent);
  end;
end;

function TFormDataEntry.GetExtension: utf8string;
begin
  Result:= 'lfm';
end;

function TFormDataEntry.GetFileSize: int64;
begin
  RequireTextContent;
  Result:= FTextContent.Size;
end;

constructor TFormDataEntry.Create(AContainer: TMultiFileContainer;
  AName: utf8string; ABinaryContent: TStream);
begin
  inherited Create(AContainer,AName,'FORMDATA',ABinaryContent);
end;

destructor TFormDataEntry.Destroy;
begin
  FreeAndNil(FTextContent);
  inherited Destroy;
end;

function TFormDataEntry.CopyTo(ADestination: TStream): int64;
begin
  RequireTextContent;
  if FTextContent.Size = 0 then
    result := 0
  else
  begin
    FTextContent.Position:= 0;
    result := ADestination.CopyFrom(FTextContent,FTextContent.Size);
  end;
end;

{ TLazResourceEntry }

procedure TLazResourceEntry.Serialize(ADestination: TStream);
begin
  FContent.Position := 0;
  BinaryToLazarusResourceCode(FContent, ADestination, Name, FValueType);
end;

function TLazResourceEntry.GetName: utf8string;
begin
  Result:= FName;
end;

procedure TLazResourceEntry.SetName(AValue: utf8string);
begin
  if AValue = FName then exit;
  if Container.IndexOf(AVAlue, Extension) <> -1 then
    raise Exception.Create('Name is already used for this extension');
  FName := AValue;
end;

function TLazResourceEntry.GetExtension: utf8string;
begin
  Result:= FValueType;
end;

function TLazResourceEntry.GetFileSize: int64;
begin
  Result:= FContent.Size;
end;

destructor TLazResourceEntry.Destroy;
begin
  FreeAndNil(FContent);
  inherited Destroy;
end;

constructor TLazResourceEntry.Create(AContainer: TMultiFileContainer; AName: utf8string; AValueType: utf8string;
  AContent: TStream);
begin
  inherited Create(AContainer);
  FName := AName;
  FValueType := UTF8UpperCase(AValueType);
  FContent := AContent;
end;

function TLazResourceEntry.CopyTo(ADestination: TStream): int64;
begin
  if FContent.Size = 0 then
    result := 0
  else
  begin
    FContent.Position:= 0;
    result := ADestination.CopyFrom(FContent, FContent.Size);
  end;
end;

{ TLazResourceContainer }

procedure TLazResourceContainer.LoadFromStream(AStream: TStream);
const
  entryStart = 'LazarusResources.Add(';
  entryEnd = ');';
  whiteSpace = [' ',#9,#10,#13,#26];
var
  fileContent: String;
  filePos : integer;

  procedure SkipWhitespace;
  begin
    while (filePos <= length(fileContent)) and (fileContent[filePos] in whiteSpace) do inc(filePos);
  end;

  procedure SkipComma;
  begin
    SkipWhitespace;
    if (filePos <= length(fileContent)) and (fileContent[filePos] = ',') then
      inc(filePos)
    else
      raise Exception.Create('Comma expected');
  end;

  function ParseString(ignoreCommas: boolean): TStream;
  var
    expectPlus: boolean;

    procedure AppendChar(c: char);
    begin
      result.WriteByte(ord(c));
    end;

    function ParseNumber: integer;
    var numberStart, errPos: integer;
      s: String;
    begin
      numberStart:= filePos;
      if (filePos <= length(fileContent)) and (fileContent[filePos] = '$') then
      begin
        inc(filePos);
        while (filePos <= length(fileContent)) and (fileContent[filePos] in['0'..'9','a'..'f','A'..'F']) do inc(filePos);
      end else
      begin
        while (filePos <= length(fileContent)) and (fileContent[filePos] in['0'..'9']) do inc(filePos);
      end;
      s := copy(fileContent,numberStart,filePos-numberStart);
      val(s, result, errPos);
      if errPos <> 0 then
        raise exception.Create('Invalid number "' + s + '"');
    end;

    function ParseStringPart: boolean;
    var charCode: integer;
    begin
      SkipWhitespace;
      if filePos <= length(fileContent) then
      begin
        if expectPlus then
          if fileContent[filePos] <> '+' then
          begin
            result := false;
            expectPlus := false;
            exit;
          end else
          inc(filePos);

        case fileContent[filePos] of
        '+': raise exception.Create('Unexpected "+"');
        '''': begin
            inc(filePos);
            while (filePos <= length(fileContent)) do
            begin
              if fileContent[filePos] = '''' then
              begin
                inc(filePos);
                if (filePos <= length(fileContent)) and (fileContent[filePos] = '''') then
                begin
                  AppendChar('''');
                  inc(filePos);
                end
                else break;
              end else
              if fileContent[filePos] in[#10,#13] then
                raise Exception.Create('Unexpected end of line')
              else
              begin
                AppendChar(fileContent[filePos]);
                inc(filePos);
              end;
            end;
            if (filePos <= length(fileContent)) and (fileContent[filePos] = '#') then
              expectPlus := false
            else
              expectPlus := true;
            result := true;
          end;
        '#': begin
            inc(filePos);
            charCode := ParseNumber;
            if (charCode < 0) or (charCode > 255) then
              raise exception.Create('Character code out of bounds');
            AppendChar(chr(charCode));
            if (filePos <= length(fileContent)) and (fileContent[filePos] in['#','''']) then
              expectPlus := false
            else
              expectPlus := true;
            result := true;
          end;
         else
         begin
           result := false;
           expectPlus := false;
         end;
         end;
      end
       else
       begin
         result := false;
         expectPlus := false;
       end;
    end;

  begin
    result := TMemoryStream.Create;
    expectPlus := false;
    if not ParseStringPart then raise exception.Create('Expecting string');
    repeat
      if ignoreCommas then
      begin
        SkipWhitespace;
        if (filePos <= length(fileContent)) and (fileContent[filePos] = ',') then
        begin
          inc(filePos);
          expectPlus := false;
        end;
      end;
    until not ParseStringPart;
  end;

  procedure ReadContent;
  var
    bytesRead: integer;
  begin
    setlength(fileContent,AStream.Size-AStream.Position);
    bytesRead := AStream.Read(fileContent[1],length(fileContent));
    setlength(fileContent, bytesRead);
    filePos := 1;
  end;

  function StreamToUTF8String(AStream: TStream): utf8String;
  begin
    setlength(result, AStream.Size);
    AStream.Position := 0;
    AStream.Read(result[1], length(result));
    AStream.Free;
  end;

var
  entryName: utf8string;
  entryType: utf8string;
  entryContent: TStream;
  inArray: boolean;

begin
  Clear;
  ReadContent;
  while filePos <= length(fileContent) do
  begin
    if (upcase(fileContent[filePos]) = upcase(entryStart[1])) and
      (CompareText(copy(fileContent,filePos,length(entryStart)),entryStart)=0) then
    begin
      inc(filePos, length(entryStart));
      entryName := StreamToUTF8String(ParseString(false));
      SkipComma;
      entryType := StreamToUTF8String(ParseString(false));
      SkipComma;

      SkipWhitespace;
      if (filePos <= length(fileContent)) and (fileContent[filePos] = '[') then
      begin
        inArray := true;
        inc(filePos);
      end else
        inArray := false;
      entryContent := ParseString(inArray);
      SkipWhitespace;
      if inArray then
      begin
        if (filePos <= length(fileContent)) and (fileContent[filePos] = ']') then
          inc(filePos)
        else
          raise exception.Create('Expecting "]"');
      end;

      if entryType = 'FORMDATA' then
        AddEntry(TFormDataEntry.Create(self,entryName,entryContent))
      else
        AddEntry(TLazResourceEntry.Create(self,entryName,entryType,entryContent));

      if (filePos+length(entryEnd)-1 <= length(fileContent)) and (CompareText(copy(fileContent,filePos,length(entryEnd)),entryEnd)=0) then
        inc(filePos,length(entryEnd))
      else
        raise exception.Create('Expecting "'+entryEnd+'"');
    end else
    if fileContent[filePos] in whiteSpace then
      inc(filePos)
    else
      raise exception.Create('Unexpected character "'+fileContent[filePos]+'"');
  end;
end;

function TLazResourceContainer.CreateEntry(AName: utf8string; AExtension: utf8string;
  AContent: TStream): TMultiFileEntry;
var
  binContent: TMemoryStream;
begin
  if UTF8CompareText(AExtension,'lfm')=0 then
  begin
    binContent := TMemoryStream.Create;
    try
      AContent.Position:= 0;
      LRSObjectTextToBinary(AContent, binContent);
      result := TFormDataEntry.Create(self,AName,binContent);
    except
      on ex:Exception do
      begin
        binContent.Free;
        result := nil;
      end;
    end;
    AContent.Free;
  end
  else
    result := TLazResourceEntry.Create(self,AName,UTF8UpperCase(AExtension),AContent);
end;

procedure TLazResourceContainer.SaveToStream(ADestination: TStream);
var
  i: Integer;
begin
  for i := 0 to Count-1 do
    TLazResourceEntry(Entry[i]).Serialize(ADestination);
end;

end.

