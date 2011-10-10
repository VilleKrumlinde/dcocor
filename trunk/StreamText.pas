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

unit StreamText;

interface
uses Classes, SysUtils;

type TStreamTextMode = (stmIn,stmOut,stmInOut);

procedure AttachStreamToTextFile(var t: Text; stream: TStream; aMode: Integer=fmClosed);
function DetachStreamFromTextFile(var t: Text; var strm: TStream): Boolean;
function GetStreamUnderTextFile(var t: Text; var strm: TStream): Boolean;
function SwapStreamUnderTextFile(var t: Text; var strm: TStream): Boolean;

procedure NULLText(var t: Text);

implementation

function FileNOPProc(var t): Integer;
begin
  Result := 0;
end;

function NULLOut(var t: TTextRec): Integer;
begin
  t.BufPos := 0;
  Result := 0;
end;

procedure NULLText(var t: Text);
begin
  with TTextRec(t) do
  begin
    Mode := fmOutput;
    BufSize := SizeOf(Buffer);
    BufPtr := @Buffer;
    Name[0] := #0;
    OpenFunc :=  @FileNOPProc;
    CloseFunc := @FileNOPProc;
    InOutFunc := @NULLOut;
    FlushFunc := @NULLOut;
  end;
end;

function StreamTextIn(var t: TTextRec): Integer; forward;
function StreamTextOut(var t: TTextRec): Integer; forward;

function StreamTextFlush(var t: TTextRec): Integer;
begin
  Result := StreamTextOut(t);
end;

function StreamTextInit(var t: TTextRec): Integer;
begin
  Result := 0;
  with t do
  begin
    BufPos := 0;
    BufEnd := 0;
    Handle := PInteger(@t.UserData)^;
    case Mode of
      fmInput:
        begin
          t.InOutFunc := @StreamTextIn;
          t.FlushFunc := @FileNOPProc;
        end;
      fmOutput:
        begin
          t.InOutFunc := @StreamTextOut;
          t.FlushFunc := @StreamTextFlush;
        end;
      fmInOut:
        begin
          t.InOutFunc := @StreamTextOut;
          t.FlushFunc := @StreamTextFlush;
        end;
      else Exit;
    end;
  end;
end;

function StreamTextOpen(var t: TTextRec): Integer;
var strm: TStream;
  tx: Text absolute t;
begin
  Result := StreamTextInit(t);
  strm := TStream(t.Handle);
  try
    if (t.Mode=fmInput)or(t.Mode=fmOutput) then
        strm.Seek(0,soBeginning);
  except
    AssignFile(tx,'');
  end;
end;

function StreamTextClose(var t: TTextRec): Integer;
var strm: TStream;
  tx: Text absolute t;
begin
  if t.Mode <> fmClosed then
  begin
    strm := TStream(PPointer(@t.UserData)^);
    strm.Free;
  end;
  AssignFile(tx,'');
  Result := 0;
end;

function StreamTextIn(var t: TTextRec): Integer;
begin
  Result := 0;
  t.BufPos := 0;
  t.BufEnd := TStream(t.Handle).Read(t.BufPtr^,t.BufSize);
end;

function StreamTextOut(var t: TTextRec): Integer;
var tmp: Cardinal;
begin
  Result := 0;
  if t.BufPos = 0 then Exit;
  tmp := TStream(t.Handle).Write(t.BufPtr^,t.BufPos);
  if tmp<>t.BufPos then
     Move((t.BufPtr+tmp)^,t.BufPtr^,t.BufPos-tmp);
  Dec(t.BufPos,tmp);
end;

procedure AttachStreamToTextFile(var t: Text; stream: TStream; aMode: Integer);
var tr: TTextRec absolute t;
begin
  with tr do
  begin
    Mode := aMode;
    BufSize := SizeOf(Buffer);
    BufPtr := @Buffer;
    Name[0] := #0;
    PPointer(@UserData)^ := stream;
    OpenFunc := @StreamTextOpen;
    CloseFunc := @StreamTextClose;
    if aMode=fmClosed then
    begin
      InOutFunc := nil;
      FlushFunc := @FileNOPProc;
    end else StreamTextOpen(tr);
  end;
end;

function DetachStreamFromTextFile(var t: Text; var strm: TStream): Boolean;
var tr: TTextRec absolute t;
begin
  Result := tr.OpenFunc=@StreamTextOpen;
  if Result then
  begin
    strm := TStream(PPointer(@tr.UserData)^);
    PPointer(@tr.UserData)^ := nil;
    CloseFile(t);
  end;
end;

function GetStreamUnderTextFile(var t: Text; var strm: TStream): Boolean;
var tr: TTextRec absolute t;
begin
  Result := tr.OpenFunc=@StreamTextOpen;
  if Result then
  begin
    Flush(t);
    strm := TStream(PPointer(@tr.UserData)^);
  end;
end;

function SwapStreamUnderTextFile(var t: Text; var strm: TStream): Boolean;
var tr: TTextRec absolute t;
  tmp: TStream;
begin
  Result := tr.OpenFunc=@StreamTextOpen;
  if Result then
  begin
    Flush(t);
    if tr.Mode<>fmClosed then
      tr.Handle := Integer(strm);
    tmp := TStream(PPointer(@tr.UserData)^);
    PPointer(@tr.UserData)^ := strm;
    strm := tmp;
  end;
end;

end.
