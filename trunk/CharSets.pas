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

unit CharSets;

interface
uses Classes;

type
  PRange = ^TRange;
  TRange = packed record
   case Boolean of
   False: (ptr: Pointer);
   True:  (from_,to_: Word);
  end;

  TCharSet = class
  protected
    fList: TList;
    function GetBit(Index: Integer): Boolean;
    procedure UniteRangePtr(aPtr: Pointer);
    procedure SubtractRangePtr(aPtr: Pointer);
    procedure Exchange(other: TCharSet);
  public
    class function Intersection(s1,s2: TCharSet): TCharSet;
    class function Intersection1(s1,s2: TCharSet): TCharSet;

    constructor Create;
    destructor Destroy; override;
    function Clone: TCharSet;
    function Count: Integer;
    procedure Clear;
    procedure Fill;

    procedure AddChar(Index: Integer);
    procedure AddRange(start,stop: Integer);
    procedure Unite(s: TCharSet);
    procedure Unite1(s: TCharSet);
    procedure Subtract(s: TCharSet);
    procedure Subtract1(s: TCharSet);
    procedure Intersect(s: TCharSet);

    function IsEmpty: Boolean;
    function Equals(s: TCharSet): Boolean;
    function Includes(s: TCharSet): Boolean;
    function Intersects(s: TCharSet): Boolean;
    function IsOne(var UniqIndex: Integer): Boolean;
    function ToString: String;

    property Chars[Index: Integer]: Boolean read GetBit; default;
  end;

function CharToStr(ch: Integer): String;

implementation
uses SysUtils;

function CharToStr(ch: Integer): String;
begin
  if (ch<32)or(ch>=127) then
    Result := Format('#%d',[ch])
  else if ch=Ord('''') then
     Result := ''''''''''
  else Result := Format('''%s''',[Char(ch)]);
end;

function RangePtr(_from,_to: Integer): Pointer;
var r: TRange;
begin
  r.from_ := _from; r.to_ := _to;
  Result := r.ptr;
end;

function SingleRangePtr(Index: Integer): Pointer;
var r: TRange;
begin
  r.from_ := Index; r.to_ := Index;
  Result := r.ptr;
end;

{ TCharSet }

constructor TCharSet.Create;
begin
  fList := TList.Create;
end;

destructor TCharSet.Destroy;
begin
  fList.Free;
  inherited;
end;

function TCharSet.Clone: TCharSet;
begin
  Result := TCharSet.Create;
  with Result do
    fList.Assign(Self.fList,laCopy);
end;

procedure TCharSet.AddRange(start, stop: Integer);
var r: TRange;
begin
  r.from_ := start; r.to_ := stop;
  UniteRangePtr(r.ptr);
end;

procedure TCharSet.Clear;
begin
  fList.Clear;
end;

function TCharSet.Count: Integer;
var I: Integer;
    r: TRange;
begin
  Result := 0;
  for I := 0 to fList.Count - 1 do
  with r do
  begin
    ptr := fList[I];
    Inc(Result,to_-from_+1);
  end;
end;

function TCharSet.Equals(s: TCharSet): Boolean;
var I: Integer;
begin
  Result := fList.Count=s.fList.Count;
  if Result then
  for I := 0 to fList.Count - 1 do
  if fList[I]<>s.fList[I] then begin Result := False; Exit; end;
end;

procedure TCharSet.Fill;
begin
  Clear;
  fList.Add(RangePtr(0,Ord(High(WideChar))));
end;

function TCharSet.GetBit(Index: Integer): Boolean;
var I: Integer;
    r: TRange;
begin
  Result := False;
  for I := 0 to fList.Count - 1 do
  with r do
  begin
    ptr := fList[I];
    if Index<from_ then Exit
    else if Index <=to_ then begin Result := True; Exit; end;
  end;
end;

procedure TCharSet.UniteRangePtr(aPtr: Pointer);
var ar,r: TRange;
    I: Integer;
begin
  ar.ptr := aPtr;
  if ar.to_<ar.from_ then Exit;
  I := 0;
  while I<fList.Count do
  with r do
  begin
    ptr := fList[I];
    if from_-1>ar.to_ then
       Break;
    if to_+1>=ar.from_ then
    begin
      if from_<ar.from_ then
        ar.from_ := from_;
      if to_>ar.to_ then
        ar.to_ := to_;
      fList.Delete(I);
    end else Inc(I);
  end;
  fList.Insert(I,ar.ptr);
end;

procedure TCharSet.SubtractRangePtr(aPtr: Pointer);
var ar,r: TRange;
    I,M: Integer;
begin
  ar.ptr := aPtr;
  I := 0;
  while I<fList.Count do
  with r do
  begin
    ptr := fList[I];
    if from_>ar.to_ then Exit;

    if to_>=ar.from_ then
    begin
      if from_>=ar.from_ then
         from_ := ar.to_+1;
      if to_<=ar.to_ then
         to_ := ar.from_-1;
      if from_>to_ then
        fList.Delete(I)
      else if from_>ar.to_ then
      begin
        fList[I] := ptr; Exit;
      end;
      if to_<ar.from_ then
      begin
        fList[I] := ptr; Inc(I);
      end else begin
        M := to_;
        to_ := ar.from_-1;
        fList.Insert(I,ptr);
        from_ := ar.to_+1;
        to_ := M;
        fLIst[I+1] := ptr;
        Exit;
      end;
    end else Inc(I);
  end;
end;

function TCharSet.ToString: String;
var I: Integer;
  r: TRange; delim: String;
begin
  Result := '';
  for I := 0 to fList.Count - 1 do
  with r do
  begin
    ptr := fList[I];
    if from_=to_ then
      Result := Format('%s%s#%d',[Result,delim,from_])
    else Result := Format('%s%s%d-%d',[Result,delim,from_,to_]);
    delim := ',';
  end;
end;

procedure TCharSet.AddChar(Index: Integer);
var I: Integer;
    r,rn: TRange;
begin
  for I := 0 to fList.Count - 1 do
  with r do
  begin
    ptr := fList[I];
    if Index<from_-1 then
    begin
      fList.Insert(I,SingleRangePtr(Index));
      Exit;
    end else if Index<=to_+1 then
    begin
      if Index=from_-1 then
        Dec(from_)
      else if Index=to_+1 then Inc(to_);
      if (I<fList.Count-1) then
      begin
        rn.ptr := fList[I+1];
        if to_=rn.from_-1 then
        begin
          to_ := rn.to_;
          fList.Delete(I+1);
        end;
      end;
      fList[I] := ptr;
      Exit;
    end;
  end;
  fList.Add(SingleRangePtr(Index));
end;

procedure TCharSet.Unite1(s: TCharSet);
var I,J: Integer;
    r: TRange;
begin
//  WriteLn(Format('Unite "%s" and "%s"',[ToString,s.ToString]));
  for I := 0 to s.fList.Count - 1 do
  with r do
  begin
    ptr := s.fList[I];
    for J := from_ to to_ do
      AddChar(J);
  end;
//  WriteLn(Format(' ->"%s"',[ToString]));
end;

procedure TCharSet.Unite(s: TCharSet);
var I: Integer;
begin
//  WriteLn(Format('Unite "%s" and "%s"',[ToString,s.ToString]));
  for I := 0 to s.fList.Count - 1 do
    UniteRangePtr(s.fList[I]);
//  WriteLn(Format(' ->"%s"',[ToString]));
end;

class function TCharSet.Intersection1(s1,s2: TCharSet): TCharSet;
var I,J: Integer;
    r: TRange;
begin
//  WriteLn(Format('Intersection "%s" and "%s"',[s1.ToString,s2.ToString]));
  Result := TCharSet.Create;
  try
    for I := 0 to s1.fList.Count - 1 do
    with r do
    begin
      ptr := s1.fList[I];
      for J := from_ to to_ do
        if s2[J] then Result.AddChar(J);
    end;
  except
    Result.Free;
  end;
//  WriteLn(Format(' ->"%s"',[Result.ToString]));
end;

class function TCharSet.Intersection(s1, s2: TCharSet): TCharSet;
var I,J: Integer;
    ar,r: TRange;
begin
//  WriteLn(Format('Intersection1 "%s" and "%s"',[s1.ToString,s2.ToString]));
  Result := TCharSet.Create;
  try
    for I := 0 to s2.fList.Count - 1 do
    begin
      ar.ptr := s2.fList[I];
      for J := 0 to s1.fList.Count - 1 do
      with r do
      begin
        ptr := s1.fList[J];
        if (ar.to_<from_)or(ar.from_>to_) then
           Continue;
        if from_<ar.from_ then
           from_ := ar.from_;
        if to_>ar.to_ then
           to_ := ar.to_;
        Result.UniteRangePtr(ptr);
      end;
    end;
  except
    Result.Free;
  end;
//  WriteLn(Format(' ->"%s"',[Result.ToString]));
end;

procedure TCharSet.Exchange(other: TCharSet);
var l: TList;
begin
  l := other.fList;
  other.fList := fList;
  fList := l;
end;

procedure TCharSet.Intersect(s: TCharSet);
var tmp: TCharSet;
begin
  tmp := Intersection(Self, s);
  Exchange(tmp);
  tmp.Free;
end;

procedure TCharSet.Subtract1(s: TCharSet);
var I,J: Integer;
    r: TRange;
    tmp: TCharSet;
begin
//  WriteLn(Format('Subtract "%s" and "%s"',[ToString,s.ToString]));
  tmp := TCharSet.Create;
  try
    for I := 0 to fList.Count - 1 do
    with r do
    begin
      ptr := fList[I];
      for J := from_ to to_ do
        if not s[J] then tmp.AddChar(J);
    end;
    r.ptr := tmp.fList;
    tmp.fList := Self.fList;
    fList := TList(r.ptr);
  finally
    tmp.Free;
  end;
//  WriteLn(Format(' ->"%s"',[ToString]));
end;

procedure TCharSet.Subtract(s: TCharSet);
var I: Integer;
begin
//  WriteLn(Format('Subtract "%s" and "%s"',[ToString,s.ToString]));
  for I := 0 to s.fList.Count - 1 do
    SubtractRangePtr(s.fList[I]);
//  WriteLn(Format(' ->"%s"',[ToString]));
end;

function TCharSet.Includes(s: TCharSet): Boolean;
var I,J: Integer;
    r: TRange;
begin
  Result := False;
  for I := 0 to s.fList.Count - 1 do
  with r do
  begin
    ptr := s.fList[I];
    for J := from_ to to_ do
      if not Chars[J] then Exit;
  end;
  Result := True;
end;

function TCharSet.Intersects(s: TCharSet): Boolean;
var I,J: Integer;
    r: TRange;
begin
  Result := True;
  for I := 0 to s.fList.Count - 1 do
  with r do
  begin
    ptr := s.fList[I];
    for J := from_ to to_ do
      if Chars[J] then Exit;
  end;
  Result := False;
end;

function TCharSet.IsEmpty: Boolean;
begin
  Result := fList.Count=0;
end;

function TCharSet.IsOne(var UniqIndex: Integer): Boolean;
begin
  Result := (fList.Count=1) and
  (TRange(fList[0]).from_=TRange(fList[0]).to_);
  if Result then
    UniqIndex := TRange(fList[0]).from_;
end;

end.
