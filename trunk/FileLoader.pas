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

unit FileLoader;

interface
uses Classes, SysUtils,CocoAncestor;

type
  TFileSystem = class(TResorceSystem)
  protected
    function FileStreamName(const URL, mime: String): String;
  public
    function AbsoluteURL(const BaseURL, RelativeURL: String): String; override;
    function ResourceExists(const URL, mime: String): Boolean; override;
    function ResourceAge(const URL: string; out ResDateTime: TDateTime; const mime: String): Boolean; override;
    function ResourceForReading(const URL, mime: String): TStream; override;
    function GetResString(const URL, mime: String): String; override;
    function ResourceForWriting(const URL, mime: String): TStream; override;
  end;

implementation

{ TFileSystem }

function TFileSystem.FileStreamName(const URL, mime: String): String;
begin
  if mime<>'' then Result := Concat(URL,':',mime)
  else Result := URL;
end;

function TFileSystem.AbsoluteURL(const BaseURL, RelativeURL: String): String;
begin
  Result := ExpandFileName(ExtractFilePath(BaseURL)+RelativeURL);
end;

function TFileSystem.GetResString(const URL, mime: String): String;
begin
  Result := LoadFileToString(FileStreamName(URL,mime));
end;

function TFileSystem.ResourceAge(const URL: string;
  out ResDateTime: TDateTime; const mime: String): Boolean;
begin
  Result := FileAge(FileStreamName(URL,mime),ResDateTime);
end;

function TFileSystem.ResourceExists(const URL, mime: String): Boolean;
begin
  Result := FileExists(FileStreamName(URL,mime));
end;

function TFileSystem.ResourceForReading(const URL, mime: String): TStream;
var fn: String;
begin
  fn := FileStreamName(URL,mime);
  if FileExists(fn) then
    Result := TFileStream.Create(fn,fmOpenRead)
  else Result := nil;  
end;

function TFileSystem.ResourceForWriting(const URL, mime: String): TStream;
begin
  Result := TFileStream.Create(FileStreamName(URL,mime),fmCreate);
end;

initialization
  CocoAncestor.ResorceSystem := TFileSystem.Create;
end.
