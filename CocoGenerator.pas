{ Copyright (C) 2009, Serge Voloshenyuk
  
  This file is Free Software and part of DCocoR
  
  It is licensed under the following three licenses as alternatives:
    1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
    2. GNU General Public License (GPL) V2 or any newer version
    3. Apache License, V2.0 or any newer version
  
  You may not use this file except in compliance with at least one of
  the above three licenses.
  
  See LICENSE.txt at the top of this package for the
  complete terms and further detail along with the license texts for
  the licenses in COPYING-LIB.txt, COPYING.txt and LICENSE-2.0.txt respectively.
} 

unit CocoGenerator;

interface
uses Classes, CocoAncestor;

type
  TCocoGenerator = class(TCocoRGrammar)
  private
    fSrcName: String;
    fOutputs: TStringList;
    procedure setOutputStream(const aName: String; const Value: TStream);
  public
    Output: Text;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetSource(const Src: String); override;
    procedure SetSourceFileName(const Filename: String); override;
    procedure SetOutput(const aName: String);
    procedure SetOutputFilename(const aName: String);
    procedure ClearOutputs;

    property SrcName: String read fSrcName;
    property Outputs[const aName: String]: TStream write setOutputStream;
  end;

implementation
uses SysUtils, StrUtils,Windows,StreamText;

{ TCocoGenerator }

procedure TCocoGenerator.ClearOutputs;
var I: Integer;
  tmp: TStream;
begin
  DetachStreamFromTextFile(Output,tmp);
  for I := 0 to fOutputs.Count - 1 do
     TStream(fOutputs.Objects[I]).Free;
  fOutputs.Clear;
  NULLText(Output);
end;

constructor TCocoGenerator.Create(AOwner: TComponent);
begin
  inherited;
  fOutputs := TStringList.Create;
  fOutputs.CaseSensitive := False;
  NULLText(Output);
end;

destructor TCocoGenerator.Destroy;
begin
  ClearOutputs;
  fOutputs.Free;
  inherited;
end;

procedure TCocoGenerator.SetOutput(const aName: String);
var I: Integer; strm,tmp: TStream;
  path: String;
begin
  if not DetachStreamFromTextFile(Output,tmp) then
    Flush(Output);
  I := fOutputs.IndexOf(aName);
  if I>=0 then strm := TStream(fOutputs.Objects[I])
  else begin
    I := IndexText(aName,['STDOUT','STDERR']);
    if I>=0 then
    begin
      if IsConsole then
      begin
        AssignFile(Output,'');
        Rewrite(Output);
        case I of
          0: Flush(System.Output);
          1: begin Flush(System.ErrOutput); TTextRec(Output).Handle := GetStdHandle(STD_ERROR_HANDLE); end;
        end;
        Exit;
      end;
      strm := TStringStream.Create('');
    end else begin  
      I := LastDelimiter(':.\', SrcName);
      if (I > 0) and (SrcName[I] = '.') then
        path := Copy(SrcName, 1, I - 1)
      else path := SrcName;
      strm := ResorceSystem.ResourceForWriting(Concat(path,'.',LowerCase(aName)));
    end;
    fOutputs.AddObject(aName,strm);
  end;
  AttachStreamToTextFile(Output,strm,fmOutput);
end;

procedure TCocoGenerator.SetOutputFilename(const aName: String);
var fn: String;
  strm,tmp: TStream;
  I: Integer;
begin
  if not DetachStreamFromTextFile(Output,tmp) then
    Flush(Output);
  if (Length(aName)>2)and ((aName[1]='''')or(aName[1]='"')) then
    fn := DequotedStr(aName)
  else fn := aName;
  I := fOutputs.IndexOf(fn);
  if I<0 then
  begin
    strm := ResorceSystem.ResourceForWriting(ResorceSystem.AbsoluteURL(SrcName,fn));
    fOutputs.AddObject(fn,strm);
  end else strm := TStream(fOutputs.Objects[I]);
  AttachStreamToTextFile(Output,strm,fmOutput);
end;

procedure TCocoGenerator.setOutputStream(const aName: String; const Value: TStream);
var I: Integer;
begin
  I := fOutputs.IndexOf(aName);
  if I>=0 then
  begin
    fOutputs.Objects[I].Free;
    fOutputs.Objects[I] := Value;
  end else fOutputs.AddObject(aName,Value);
end;

procedure TCocoGenerator.SetSource(const Src: String);
begin
  inherited;
  fSrcName := '';
end;

procedure TCocoGenerator.SetSourceFileName(const Filename: String);
begin
  inherited;
  fSrcName := Filename;
end;

end.
