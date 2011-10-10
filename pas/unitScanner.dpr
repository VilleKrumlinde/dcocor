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
program unitScanner;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  DelphiUnit in 'DelphiUnit.pas',
  CocoAncestor in '..\CocoAncestor.pas',
  FileLoader in '..\FileLoader.pas',
  DelphiCond in 'DelphiCond.pas';

var comp: TDelphiUnit;
  meth: TMethod;
  errProc: TErrorEvent absolute meth;
  F: TSearchRec;
  R: Integer;
  dir,path,filename: String;

procedure ErrorHandler(this: TDelphiUnit; Sender: TObject; ErrorType,ErrorCode, line,col: Integer;
    const Msg, data: string);
begin
  WriteLn(Format('Error in "%s"(%d,%d): %s',[filename,Line,Col,Msg,data]));
end;

begin
 try
   if (ParamCount=0) then
   begin
      WriteLn(Format('Usage: %s <dir or file>',[ChangeFileExt(ExtractFileName(ParamStr(0)),'')]));
      Exit;
   end;
   comp := TDelphiUnit.Create(nil);
   try
     meth.Data := comp;
     meth.Code := @ErrorHandler;
     comp.OnError := errProc;
     dir := ParamStr(1);
     if FileExists(dir) then
     begin
       filename := dir;
       comp.SetSourceFileName(dir);
       comp.Execute;
     end else begin
       path := dir+'*.pas';
       R := FindFirst(path, 0, F);
       while R = 0 do
       begin
         try
           filename := dir+F.Name;
           comp.SetSourceFileName(filename);
           comp.Execute;
//           WriteLn(Format('"%s" done',[filename]));
         except
          on E:Exception do
            Writeln(Format('%s in "%s": %s',[E.Classname,F.Name,E.Message]));
         end;
          R := FindNext(F);
       end;
       FindClose(F);
     end;
   finally
     comp.Free;
   end;
 except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
 end;
end.
