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
program CocoRComp;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  CocoAncestor in 'CocoAncestor.pas',
  CRT in 'CRT.pas',
  CRTypes in 'CRTypes.pas',
  CRA in 'CRA.pas',
  Coco in 'Coco.PAS',
  ParserGen in 'ParserGen.pas',
  PascalParserGen in 'PascalParserGen.pas',
  FileLoader in 'FileLoader.pas',
  CocoCompiler in 'CocoCompiler.pas',
  CharSets in 'CharSets.pas',
  StreamText in 'StreamText.pas',
  CocoGenerator in 'CocoGenerator.pas',
  Windows,
  CocoSets in 'CocoSets.pas';

//This is never used
procedure CreateOutput(const BaseName,aName: String; var strm: TStream);
var I: Integer; path: String;
begin
  if aName='STDOUT' then
     strm := THandleStream.Create(GetStdHandle(STD_OUTPUT_HANDLE))
  else if aName='STDERR' then
     strm := THandleStream.Create(GetStdHandle(STD_ERROR_HANDLE))
  else begin
    I := LastDelimiter(':.\', BaseName);
    if (I > 0) and (BaseName[I] = '.') then
      path := Copy(BaseName, 1, I - 1)
    else path := BaseName;
    strm := ResorceSystem.ResourceForWriting(
      Concat(path,'.',LowerCase(aName)));
  end;
end;

var comp: TCocoCompiler;
    I: Integer;
begin
 try
   if (ParamCount=0)or not ResorceSystem.ResourceExists(ParamStr(1)) then
   begin
      WriteLn(Format('Usage: %s <grammar>.atg',[ChangeFileExt(ExtractFileName(ParamStr(0)),'')]));
      Exit;
   end;
   ParserGen.ExePath := ExtractFilePath(ParamStr(0));
//   ParserGen.OnCreateOut := CreateOutput;
   comp := TCocoCompiler.Create(nil);
   try
     if not comp.Compile(ParamStr(1)) then
     begin
       if not comp.Successful then
       begin
         WriteLn('!!!Error on frame parsing: results are abrupt!!!');
         if comp.MetaErrors<>nil then
         for I := 0 to comp.MetaErrors.Count - 1 do
           WriteLn(TCocoRError(comp.MetaErrors[I]).FullMessage(comp.FrameName));
       end;
     end;
   finally
     comp.Free;
   end;
 except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
 end;
end.
