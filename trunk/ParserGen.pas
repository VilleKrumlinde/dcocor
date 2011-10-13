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

unit ParserGen;

interface
uses SysUtils, Classes, Contnrs, CocoSets, CharSets, CocoAncestor, CRTypes,CRA,CRT, Coco;

const
  maxInlineSet = 3;
  MetaKey: String = '<|';

type
  TCocoRError = class
  public
    ErrorType,ErrorCode, line,col: Integer;
    Msg, Data: string;
    function FullMessage(const SourceName: String): String;
  end;

  TCreateOutProc = procedure(const BaseName,aName: String; var strm: TStream);

  TParserGenerator = class
  private
    fCoco: TCoco;
  protected
    fSets: TObjectList;
    fSynErrors: TStringList;
    fSymbolNamesHadGenerated: Boolean;
    function getHasSymSets: Boolean;
    function GenParams(pos: PSymbol): String; overload;
    function GenParams(sym:TSymbol): String; overload;
    function NewCondSet(aset: TCocoSet): Integer;
    function NewSynError(const Msg: String): Integer;
  public
    destructor Destroy; override;
    procedure Clear;
    procedure WriteCoco(const S : string); overload;
    procedure WriteCoco(I,Width : integer); overload;
    procedure WriteLnCoco(const S : string);

    function GenTokenName(n: Integer): String;

    procedure Print(const str: String);
    procedure PrintLn(const str: String); overload;
    procedure PrintLn; overload;
    procedure PrintListEnd(count: Integer);
    procedure PrintFragment(pos: PSymbol; eol: Boolean=False);

    procedure PrintState(state: TState); virtual;
    procedure PrintParserDeclarations; virtual;
    procedure PrintTokens;             virtual;
    procedure PrintTokenStrings;       virtual;
    procedure PrintAnyAndSyncSets;     virtual;
    procedure PrintInitStartStates;    virtual;
    procedure PrintComments;       virtual;
    procedure PrintInitLiterals;       virtual;
    procedure PrintInitSymSets;        virtual;
    procedure PrintErrors;             virtual;
    procedure PrintScanSym;            virtual;
    procedure PrintIgnoreSet;          virtual;
    procedure PrintParserImplementation; virtual;
    procedure PrintPragmas;            virtual;
    procedure PrintSemanticCode(Text: PChar; line,col,len: Integer); virtual;

    function  GenTerminals(aset: TCocoSet): String;
    procedure PrintTerminals(aset: TCocoSet);
    procedure PrintListOfDeletableSymbols;
    procedure PrintStartFollowerSets;
    procedure PrintXRef;
    procedure PrintListing(Errors: TList);
    procedure PrintSymbolTable;
    procedure PrintNodes;
    procedure PrintStates;

    property Coco: TCoco read fCoco write fCoco;
    property hasSymSets: Boolean read getHasSymSets;
  end;

  TParserGeneratorClass = class of TParserGenerator;

  procedure RegisterParserGenerator(const Name: String; aClass: TParserGeneratorClass);
  function FindParserGeneratorClass(const Name: String; var aClass: TParserGeneratorClass): Boolean;

var ExePath: String;
    OnCreateOut: TCreateOutProc;
implementation
uses StrUtils, StreamText;

const
  sTyp: array[TSymbolType] of String = ('    ', 't    ', 'nt  ', 'pr  ');
  nTyp: array[TNodeType] of String = ('','clas', 'chr ','t   ', 'wt  ','nt  ',
     'alt ', 'iter', 'opt ', 'eps ','any ', 'sync', 'if  ', 'sem ');

type TCharSetAccess = class(TCharSet);

var _GenClasses: TStringList;

procedure RegisterParserGenerator(const Name: String; aClass: TParserGeneratorClass);
begin
  if _GenClasses=nil then
  begin
    _GenClasses := TStringList.Create;
    _GenClasses.CaseSensitive := False;
    _GenClasses.Sorted := True;
    _GenClasses.Duplicates := dupError;
  end;
  _GenClasses.AddObject(Name,TObject(aClass));
end;

function FindParserGeneratorClass(const Name: String; var aClass: TParserGeneratorClass): Boolean;
var I: Integer;
begin
  Result := (_GenClasses<>nil) and _GenClasses.Find(Name,I);
  if Result then aClass := TParserGeneratorClass(_GenClasses.Objects[I]);
end;


{ TParserGenerator }

destructor TParserGenerator.Destroy;
begin
  fSets.Free;
  fSynErrors.Free;
  inherited;
end;

procedure TParserGenerator.Clear;
begin
  if fSets<>nil then
    fSets.Clear;
  if fSynErrors<>nil then
    fSynErrors.Clear;
end;

procedure TParserGenerator.PrintAnyAndSyncSets;
var I: Integer;
begin
  for I := 0 to Coco.tab.NodesCount - 1 do
  with Coco.tab.Nodes[I] do
  if typ in [ntAny, ntSync]  then
  begin
    WriteLn(Coco.Output,index:4,nTyp[typ]:5,' = ',GenTerminals(aset));
  end;
end;

procedure TParserGenerator.PrintLn;
begin
  WriteLn(Coco.Output,'');
end;

procedure TParserGenerator.PrintLn(const str: String);
begin
  WriteLn(Coco.Output,str);
end;

procedure TParserGenerator.Print(const str: String);
begin
  WriteCoco(str);
end;

procedure TParserGenerator.PrintListEnd(count: Integer);
begin
  if count=0 then
    WriteCoco(#9'-- none --')
  else PrintLn;
end;

procedure TParserGenerator.PrintListOfDeletableSymbols;
var I,C: Integer;
begin
  C := 0;
  with Coco.tab do
  for I := 0 to NonTerminalCount - 1 do
  if NonTerminals[I].deletable then
  begin
    Inc(C);
    WriteCoco(#13#10#9 + NonTerminals[I].Name);
  end;
  PrintListEnd(C);
end;

procedure TParserGenerator.PrintStartFollowerSets;
var I: Integer;
begin
  with Coco.tab do
  for I := 0 to NonTerminalCount - 1 do
  begin
    WriteCoco(#9 + NonTerminals[I].Name + #13#10#9#9'first :');
    PrintTerminals(NonTerminals[I].first);
    WriteCoco(#9#9'follow:');
    PrintTerminals(NonTerminals[I].follow);
  end;
end;


procedure TParserGenerator.PrintStates;

  procedure PrintActions(a: TAction);
  var I: Integer; str: String;
  begin
    str := ' ';
    if a=nil then PrintLn;
    while a<>nil do
    begin
      if a.typ=ntCharClass then
        WriteCoco(str+Coco.tab.CharClassNames[a.sym])
      else WriteCoco(str+Char(a.sym));
      str := '                     ';
      for I := 0 to a.TargetCount - 1 do
        WriteCoco(a.Targets[I].index,4);
      WriteLnCoco(IfThen(a.tc=contextTrans,' context',''));
      a := a.next;
    end;
  end;

  function GenCharClassString(s: TCharSet): String;
    function ToChar(I: Integer): String;
    begin
      if (I>32)and(I<126) then
        Result := Char(I)
      else Result := Format('{#%d}',[I]);
    end;
  var I,J: Integer;
    r: TRange;
  begin
    Result := '';
    with TCharSetAccess(s) do
    for I := 0 to fList.Count - 1 do
    with r do
    begin
      ptr := fList[I];
      if from_=to_ then
        Result := Result + ToChar(from_)
      else for J := from_ to to_ do
        Result := Result + ToChar(J);
    end;
  end;

var I: Integer; str: String;
begin
  for I := 0 to Coco.DFA.StatesCount - 1 do
  with Coco.DFA.States[I] do
  begin
    if endOf=nil then str := ''
    else str := Format('E(%12s)',[endOf.Name]);
    //WriteCoco(str:15,index:4,':');
    WriteCoco(str);
    WriteCoco(index,4);
    WriteCoco(':');
    PrintActions(firstAction);
  end;
  PrintLn;
  WriteLnCoco('---------- character classes ----------');
  with Coco.tab do
  for I := 0 to CharClassCount - 1 do
    WriteLnCoco(CharClassNames[I] + ' = ' + CharClassSet[I].ToString);
end;

type
PIterRec = ^TIterRec;
TIterRec = record
  empty: Boolean;
  gen: TParserGenerator;
end;

procedure _PrintTerminalSym(data: Pointer; Index: Integer);
begin
  with PIterRec(data)^ do
  begin
    empty := False;
    with gen do
      WriteCoco(' ' + Coco.tab.Terminals[Index].Name);
  end;
end;

procedure TParserGenerator.PrintTerminals(aset: TCocoSet);
var rec: TIterRec;
begin
  with rec do
  begin
    empty := True;
    gen := Self;
  end;
  if aset<>nil then
    aset.IterateTrue(_PrintTerminalSym,@rec);
  if rec.empty then
    WriteLnCoco('  -- empty set--')
  else PrintLn;
end;

function TParserGenerator.GenTokenName(n: Integer): String;
begin
  if fSymbolNamesHadGenerated and (n>=0) then
  with Coco.tab do
  begin
    if n<TerminalCount then
       Result := Terminals[n].symID
    else Result := Pragmas[n-TerminalCount].symID;
  end else Result := IntToStr(n);
end;

type
PgIterRec = ^TgIterRec;
TgIterRec = record
  r: String;
  lc: Integer;
  gen: TParserGenerator;
end;

procedure _GenTerminalSym(data: Pointer; Index: Integer);
var s: String;
begin
  with PgIterRec(data)^ do
  begin
    if r<>'' then r := r+', ';
    s := gen.GenTokenName(Index);
    Inc(lc,Length(s));
    if lc>500 then
    begin
      lc := 0;
      r := r+#10#13;
    end;
    r := r + s;
  end;
end;

function TParserGenerator.GenParams(sym: TSymbol): String;
begin
  if sym.hasAttrPos then
    Result := GenParams(sym.attrPos)
  else Result := '';
end;

function TParserGenerator.GenTerminals(aset: TCocoSet): String;
var rec: TgIterRec;
begin
  if aset=nil then Result := '<?>'
  else begin
    with rec do
    begin
      r := ''; lc := 0;
      gen := Self;
    end;
    aset.IterateTrue(_GenTerminalSym,@rec);
    Result := rec.r;
  end;
end;

procedure TParserGenerator.PrintSymbolTable;

  procedure PrintSym(sym: TSymbol);
  var str: String;
  begin
    if sym.graph<>nil then str := IntToStr(sym.graph.index)
    else str := '';
    WriteLn(Coco.Output,sym.index:3,' ',sym.Name:14,' ',sTyp[sym.typ],' ',str);
  end;
var I: Integer;
begin
  WriteLn(Coco.Output,'idx name          typ node');
  WriteLn(Coco.Output,'--------------------------');
  with Coco.tab do
  begin
    for I := 0 to TerminalCount - 1 do
      PrintSym(Terminals[I]);
    PrintLn;
    for I := 0 to PragmaCount - 1 do
      PrintSym(Pragmas[I]);
    PrintLn;
    for I := 0 to NonTerminalCount - 1 do
      PrintSym(NonTerminals[I]);
    WriteLn(Coco.Output,'++++++++++++++++++++++');
  end;
end;

procedure TParserGenerator.PrintNodes;
  procedure PrintNode(gn: TNode);

    function Ptr(p: TNode; up: Boolean): Integer;
    begin
      if p=nil then Result := 0
      else if up then Result := -p.index
      else Result := p.index;
    end;
    function Pos(pos: PSymbol): String;
    begin
      if pos<>nil then Result := Format('%5d',[pos^.Beg])
      else Result := '     ';
    end;

  var str: String;
  begin
    if gn.sym<>nil then Str := gn.sym.Name
    else if gn.typ=ntCharClass then str := Coco.tab.CharClassNames[gn.val];
    Write(Coco.Output,gn.index:4,nTyp[gn.typ]:5, str:15,Ptr(gn.next,gn.up):6);
    case gn.typ of
      ntTerminal, ntWeakTerminal, ntNonTerminal:
        Write(Coco.Output,Pos(@gn.pos):18);
      ntChr:
        Write(Coco.Output,gn.val:5, gn.code:6);
      ntCharClass:
        Write(Coco.Output,'      ',gn.code:5,'       ');
      ntAlt, ntIter, ntOpt:
        Write(Coco.Output,Ptr(gn.down, false):5, Ptr(gn.sub, false):6,'       ');
      ntSem:
        Write(Coco.Output,Pos(@gn.pos):18);
      ntEps, ntAny, ntSync:
        Write(Coco.Output,'                  ');
    end;
    WriteLn(Coco.Output,gn.line:5);
  end;
var I: Integer;
begin
	WriteLn(Coco.Output,'   n type name            next  down   sub   pos  line');
	WriteLn(Coco.Output,'                                 val  code');
	WriteLn(Coco.Output,'----------------------------------------------------');
  with Coco.tab do
  begin
    for I := 0 to NodesCount - 1 do
      PrintNode(Nodes[I]);
    PrintLn;
  end;
end;

function TParserGenerator.GenParams(pos: PSymbol): String;
begin
  if (pos<>nil)and(pos^.Beg>=0) then
    Result := Format('(%s)',[Coco.Scanner.getSymbolText(pos)])
  else Result := '';
end;

function TParserGenerator.NewCondSet(aset: TCocoSet): Integer;
var I: Integer;
begin
  if fSets=nil then
    fSets := TObjectList.Create
  else
  for I := 0 to fSets.Count - 1 do
  if TCocoSet(fSets[I]).Equals(aset) then
  begin
    Result := I; Exit;
  end;
  Result := fSets.Add(aset.Clone);
end;

function TParserGenerator.NewSynError(const Msg: String): Integer;
begin
  if fSynErrors=nil then
    fSynErrors := TStringList.Create
  else begin
    Result := fSynErrors.IndexOf(Msg)+1;
    if Result>0 then Exit;
  end;
  Result := fSynErrors.Add(Msg)+1;
end;

procedure _PrintRefListAndFree(AInfo, AItem, AData: Pointer; out AContinue: Boolean);
var this: TParserGenerator absolute AInfo;
    sym: TSymbol absolute AItem;
    list: TList absolute AData;
    I: Integer;
begin
  try
    Write(this.Coco.Output,'  ',sym.Name,#9);
    for I := 0 to list.Count - 1 do
      Write(this.Coco.Output,Integer(list[I]):5);
    this.PrintLn;
  finally
    list.Free;
  end;
end;

procedure TParserGenerator.PrintXRef;
var map: TBucketList;
  I: Integer;
  data: Pointer;
  list: TList absolute data;
  s: TSymbol;
begin
  map := TBucketList.Create;
  try
    for I := 0 to Coco.tab.NonTerminalCount - 1 do
    begin
      s := Coco.tab.NonTerminals[I];
      if not map.Find(s,data) then
      begin
        list := TList.Create;
        map.Add(s,data);
      end;
      list.Add(Pointer(-s.line));
    end;
    for I := 0 to Coco.tab.NodesCount - 1 do
    with Coco.tab.Nodes[I] do
    if typ in [ntTerminal, ntWeakTerminal, ntNonTerminal] then
    begin
      if not map.Find(sym,data) then
      begin
        list := TList.Create;
        map.Add(sym,data);
      end;
      list.Add(Pointer(line));
    end;
    map.ForEach(_PrintRefListAndFree,Self);
    PrintLn;

  finally
    map.Free;
  end;
end;

procedure TParserGenerator.WriteCoco(const S: string);
begin
  Write(Coco.Output,AnsiString(S))
end;

procedure TParserGenerator.WriteCoco(I,Width : integer);
begin
  Write(Coco.Output, Format('%d:' + IntToStr(Width) ,[I]) );
end;

procedure TParserGenerator.WriteLnCoco(const S: string);
begin
  WriteLn(Coco.Output,AnsiString(S));
end;

procedure TParserGenerator.PrintListing(Errors: TList);
var I,eI,L: Integer;
    str,strShift: String;
begin
  if Errors=nil then Exit;
  eI := 0;
  I := 0;
  L := TCocoRScanner(Coco.Scanner).StartLine;
  while (eI<Errors.Count)and(TCocoRError(Errors[eI]).line<L) do
  begin
    WriteLn(Coco.Output,'*****  ',TCocoRError(Errors[eI]).Msg);
    Inc(eI);
  end;
  if eI>0 then
     WriteLn(Coco.Output,'**************************');
  while TCocoRScanner(Coco.tab.Parser.Scanner).GetLine(I,str) do
  begin
    WriteLn(Coco.Output,L:5,'  ',str);
    while eI<Errors.Count do
    with TCocoRError(Errors[eI]) do
    begin
      if line<=L then
      begin
        if line=L then
        begin
          if col>0 then
            strShift := StringOfChar(' ',col)
          else strShift := '';
          WriteLn(Coco.Output,'***** ',strShift,'^ ',Msg);
        end;
        Inc(eI);
      end else Break;
    end;
    Inc(L);
  end;
  PrintLn;
  if eI<Errors.Count then
  begin
    WriteLn(Coco.Output,'**************************');
    while eI<Errors.Count do
    begin
      WriteLn(Coco.Output,'*****  ',TCocoRError(Errors[eI]).Msg);
      Inc(eI);
    end;
  end;
end;

procedure TParserGenerator.PrintSemanticCode(Text: PChar; line, col, len: Integer);
var strm: TStream;
    str: String;
    S : ansistring;
begin
  if col>1 then
      WriteCoco(StringOfChar(' ',col+1));
  if GetStreamUnderTextFile(Coco.Output,strm) then
  begin
    S := AnsiString(WideCharToString(PWideChar(Text)));
    strm.Write( PAnsiChar(S)^,len)
  end
  else
  begin
    SetString(str, Text,len);
    WriteCoco(str);
  end;
end;

procedure TParserGenerator.PrintFragment(pos: PSymbol; eol: Boolean);
var ptr: PChar;
begin
  ptr := Coco.Scanner.GetSymbolPtr(pos);
  if (ptr<>nil)and(pos^.Len>0) then
  begin
    PrintSemanticCode(ptr,pos^.Line,pos^.Col,pos^.Len);
    if eol then PrintLn;
  end;
end;

procedure TParserGenerator.PrintErrors;
begin
end;

procedure TParserGenerator.PrintIgnoreSet;
begin
end;

procedure TParserGenerator.PrintComments;
begin
end;

procedure TParserGenerator.PrintInitLiterals;
begin
end;

procedure TParserGenerator.PrintInitStartStates;
begin
end;

function TParserGenerator.getHasSymSets: Boolean;
begin
  Result := (fSets<>nil)and(fSets.Count>0);
end;

procedure TParserGenerator.PrintInitSymSets;
var I,L: Integer;
begin
  if fSets<>nil then
  begin
    L := fSets.Count-1;
    for I := 0 to L-1 do
      WriteLn(Coco.Output,#9'{',I:2,'} ',GenTerminals(TCocoSet(fSets[I])),', -1,');
    Write(Coco.Output,#9'{',L:2,'} ',GenTerminals(TCocoSet(fSets[L])));
  end;
end;

procedure TParserGenerator.PrintParserDeclarations;
begin
end;

procedure TParserGenerator.PrintParserImplementation;
begin
end;

procedure TParserGenerator.PrintPragmas;
begin
end;

procedure TParserGenerator.PrintState(state: TState);
begin
end;

procedure TParserGenerator.PrintScanSym;
var I: Integer;
begin
  with Coco.DFA do
  for I := 1 to StatesCount-1 do //first's omited
    PrintState(States[I]);
end;

procedure TParserGenerator.PrintTokens;
begin
end;

procedure TParserGenerator.PrintTokenStrings;
begin
end;

{ TCocoRError }

function TCocoRError.FullMessage(const SourceName: String): String;
begin
  Result := Format('Error in "%s"(%d,%d): %s'#13#10,[SourceName,Line,Col,Msg]);
end;


end.
