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

unit CRT;

interface
uses Classes, Contnrs, SysUtils, Sets, CharSets, CRTypes, CocoAncestor;

type

  TSymbolTable = class
  private
    fParser: TCocoRGrammar;
    fSymbolNames, fCharClassNames, fLiterals,
    fTemplateVars: TStringList;
    fSymbols, fNodes: TObjectList;
    fTerminals, fNonterminals, fPagmas: TList;
    fGramSy: TNtSymbol;
    fEofSy: TtSymbol;
    fDummyNode: TNode;

    fDummyName: Integer;
    fAllSyncSets: TSet;
    fNoSym: TtSymbol;

    fDeletableCount,
    fNondefinedTerminals, fUnreachableNonterminals,
    fCircularProductions,fNonderivedNonterminals: Integer;
    fReachables: TSet;

    fFrameName: String;
    fIgnoreCase: Boolean;

    function getSymbolByName(const Name: String): TSymbol;
    function getCharClassByName(const name: String): Integer;
    function getCharClassCount: Integer;
    function GetCharClassName(Idx: Integer): String;
    function getCharClassSet(Idx: Integer): TCharSet;
    function getTemplateVar(const name: String): PSymbol;
    procedure setTemplateVar(const name: String; const Value: PSymbol);
    function getLiteral(const Name: String): TSymbol;
    procedure setLiteral(const aName: String; const Value: TSymbol);
    function getNodesCount: Integer;
    procedure setFrameName(const Value: String);
    function getNonTerminal(I: Integer): TNtSymbol;
    function getNonTerminalCount: Integer;
    function getSymbol(I: Integer): TSymbol;
    function getTerminal(I: Integer): TtSymbol;
    function getTerminalCount: Integer;
    function getNode(I: Integer): TNode;
    procedure setIgnoreCase(const Value: Boolean);
    function getSymbolCount: Integer;
    function getPragma(I: Integer): TtSymbol;
    function getPragmaCount: Integer;
  protected
    procedure Clear;
    procedure Init;
  public
    IgnoredChars: TCharSet;
    WarnSuppression: Boolean;
    constructor Create(aParser: TCocoRGrammar);
    destructor Destroy; override;

    procedure Reinit;
    procedure Finalize;
    procedure DeleteNodes;

    function NewSym(typ: TSymbolType; name: String; line: Integer=0): TSymbol;

    function  NewNode(typ: TNodeType; sym: TSymbol; line: Integer): TNode; overload;
    function  NewNode(typ: TNodeType; sub: TNode): TNode; overload;
    function  NewNode(typ: TNodeType; val, line: Integer): TNode; overload;
    procedure MakeFirstAlt(var nL, nR: TNode);
    procedure MakeAlt(nL1,nR1,nL2,nR2: TNode);
    procedure MakeSeq(nL1: TNode; var nR1: TNode; nL2,nR2: TNode);
    procedure MakeIter(var nL, nR: TNode);
    procedure MakeOpt(var nL, nR: TNode);

    procedure StrToGraph(const str: String; var nL,nR: TNode);
    procedure SetContextTrans(p: TNode);

    function NewCharClass(name: String; cset: TCharSet=nil): Integer;
    function FindCharClass(cset: TCharSet): Integer;
    function FixStringForToken(const name: string): String;
    function CountHomographs: Integer;

    procedure CompExpected(gn: TNode; curNT: TNtSymbol; s: TSet);
    procedure CompFirstSet(gn: TNode; rset: TSet);
    procedure CompFirstSets;
    procedure CompFollowSets;
    procedure CompAnySets;
    procedure CompSyncSets;
    procedure SetupAnys;
    procedure CompDeletableSymbols;
    procedure RenumberPragmas;

    function CountNondefinedTerminals: Integer;
    function CountUnreachableNt: Integer;
    function CountCircularProductions: Integer;
    function CountNonderivedNonterminals: Integer;
    procedure CheckResolvers;
    procedure CheckLL1;

    property Parser: TCocoRGrammar read fParser;

    property IgnoreCase: Boolean read fIgnoreCase write setIgnoreCase;
    property Symbols[const Name: String]: TSymbol read getSymbolByName;
    property SymbolCount: Integer read getSymbolCount;
    property Symbol[I: Integer]: TSymbol read getSymbol; default;
    property eofSy: TtSymbol read fEofSy;
    property gramSy: TNtSymbol read fGramSy write fGramSy;
    property noSym: TtSymbol read fNoSym;
    property AllSyncSets: TSet read fAllSyncSets;
    property Literals[const Name: String]: TSymbol read getLiteral write setLiteral;

    property Nodes[I: Integer]: TNode read getNode;
    property NodesCount: Integer read getNodesCount;

    property CharClassByName[const name: String]: Integer read getCharClassByName;
    property CharClassNames[Idx: Integer]: String read GetCharClassName;
    property CharClassSet[Idx: Integer]: TCharSet read getCharClassSet;
    property CharClassCount: Integer read getCharClassCount;
    property FrameName: String read fFrameName write setFrameName;
    property TemplateVars[const name: String]: PSymbol read getTemplateVar write setTemplateVar;

    property NonTerminals[I: Integer]: TNtSymbol read getNonTerminal;
    property NonTerminalCount: Integer read getNonTerminalCount;
    property Terminals[I: Integer]: TtSymbol read getTerminal;
    property TerminalCount: Integer read getTerminalCount;
    property Pragmas[I: Integer]: TtSymbol read getPragma;
    property PragmaCount: Integer read getPragmaCount;

    property DeletableCount: Integer read fDeletableCount;
  end;

procedure FillSetByStr(cset: TCharSet; const str: String; IgnoreCase: Boolean=False);

implementation

procedure FillSetByStr(cset: TCharSet; const str: String; IgnoreCase: Boolean);
var I: Integer; ch: Char;
begin
  for I := 1 to Length(str) do
  begin
    if IgnoreCase then
      ch := UpCase(str[I])
    else ch := str[I];
    cset.AddChar(Ord(ch));
  end;
end;

{ TSymbolTable }

constructor TSymbolTable.Create(aParser: TCocoRGrammar);
begin
  fParser := aParser;
  fSymbolNames := TStringList.Create;
  with fSymbolNames do
  begin
    CaseSensitive := True;
    Sorted := True;
    Duplicates := dupIgnore;
  end;
  fCharClassNames := TStringList.Create;
  with fCharClassNames do
  begin
    CaseSensitive := True;
    Duplicates := dupIgnore;
  end;

  fSymbols := TObjectList.Create;
  fNodes   := TObjectList.Create;
  fTerminals := TList.Create;
  fNonterminals := TList.Create;
  fPagmas := TList.Create;
  fAllSyncSets := TSet.Create;
  fReachables := TSet.Create;
  Init;
end;

destructor TSymbolTable.Destroy;
begin
  Clear;
  fSymbols.Free;
  fNodes.Free;
  fSymbolNames.Free;

  fCharClassNames.Free;
  fTemplateVars.Free;
  fLiterals.Free;

  fTerminals.Free;
  fNonterminals.Free;
  fPagmas.Free;
  fAllSyncSets.Free;
  fReachables.Free;
  IgnoredChars.Free;
  inherited;
end;

procedure TSymbolTable.Clear;
var I: Integer;
  P: PSymbol;
begin
  for I := 0 to fCharClassNames.Count - 1 do
    fCharClassNames.Objects[I].Free;
  fCharClassNames.Clear;

  if fTemplateVars<>nil then
  begin
    for I := 0 to fTemplateVars.Count - 1 do
    begin
      P := PSymbol(Pointer(fTemplateVars.Objects[I]));
      Dispose(P);
    end;
    fTemplateVars.Clear;
  end;
  if fLiterals<>nil then
    fLiterals.Clear;

  fSymbolNames.Clear;
  fSymbols.Clear;
  fDummyNode := nil;
  fNodes.Clear;
  fTerminals.Clear;
  fNonterminals.Clear;
  fPagmas.Clear;
  fEofSy := nil;
  fNoSym := nil;
  fDummyName := 0;
  fAllSyncSets.Size := 0;

  fDeletableCount := 0;
  fNondefinedTerminals := 0;
  fUnreachableNonterminals := 0;
  fCircularProductions := 0;
  fNonderivedNonterminals := 0;
  fReachables.Clear;
  if IgnoredChars<>nil then
    IgnoredChars.Clear;
end;

procedure TSymbolTable.DeleteNodes;
begin
  fNodes.Clear;
	fDummyNode := NewNode(ntEps,nil, 0);
end;

procedure TSymbolTable.Init;
begin
	fEofSy := TtSymbol(NewSym(stTerminal, 'EOF', 0));
  fEofSy.symID := '_EOFSYMB';
	fDummyNode := NewNode(ntEps,nil, 0);
  FrameName := '';
  WarnSuppression := False;
end;

procedure TSymbolTable.Reinit;
begin
  Clear;
  Init;
end;

procedure TSymbolTable.Finalize;
begin
  fNoSym := TtSymbol(NewSym(stTerminal,'???',0));
  fNoSym.symID := '_NOSYMB';
  SetupAnys;
  RenumberPragmas;
  if Parser.Successful then
  begin
    CompDeletableSymbols;
    CompFirstSets;
    CompFollowSets;
    CompAnySets;
    CompSyncSets;
  end;
  if Parser.Successful then
  begin
    CountNondefinedTerminals;
    if gramSy<>nil then
      CountUnreachableNt;
    CountCircularProductions;
    CountNonderivedNonterminals;
    CheckResolvers;
    CheckLL1;
  end;
end;

function TSymbolTable.getSymbol(I: Integer): TSymbol;
begin
  Result := TSymbol(fSymbols[I]);
end;

function TSymbolTable.getSymbolCount: Integer;
begin
  Result := fSymbols.Count;
end;

function TSymbolTable.getSymbolByName(const Name: String): TSymbol;
var I: Integer;
begin
  if fSymbolNames.Find(Name,I) then
    Result :=  TSymbol(fSymbolNames.Objects[I])
  else Result := nil;
end;

function TSymbolTable.NewSym(typ: TSymbolType; name: String;
  line: Integer): TSymbol;
begin
  if (name='""')or(name='''''') then
  begin
    Parser.SemError(200);
    name := '';
  end;
  if typ=stNonterminal then
    Result := TNtSymbol.Create(typ,name,line)
  else Result := TtSymbol.Create(typ,name,line);

  fSymbols.Add(Result);
  if (name<>'')and(typ in [stTerminal,stNonterminal]) then
     fSymbolNames.AddObject(name,Result);
  case typ of
    stTerminal:    Result.index := fTerminals.Add(Result);
    stNonterminal: Result.index := fNonterminals.Add(Result);
    stPragma:      fPagmas.Add(Result);
  end;
end;

function TSymbolTable.getNode(I: Integer): TNode;
begin
  Result := TNode(fNodes[I]);
end;

function TSymbolTable.getNodesCount: Integer;
begin
  Result := fNodes.Count;
end;

function TSymbolTable.getNonTerminal(I: Integer): TNtSymbol;
begin
  Result := TNtSymbol(fNonterminals[I]);
end;

function TSymbolTable.getNonTerminalCount: Integer;
begin
  Result := fNonterminals.Count;
end;

function TSymbolTable.getPragma(I: Integer): TtSymbol;
begin
  Result := TtSymbol(fPagmas[I]);
end;

function TSymbolTable.getPragmaCount: Integer;
begin
  Result := fPagmas.Count;
end;

function TSymbolTable.getTerminal(I: Integer): TtSymbol;
begin
  Result := TtSymbol(fTerminals[I]);
end;

function TSymbolTable.getTerminalCount: Integer;
begin
  Result := fTerminals.Count;
end;

function TSymbolTable.NewNode(typ: TNodeType; sym: TSymbol;
  line: Integer): TNode;
begin
  Result := TNode.Create(typ,sym,line);
  Result.index := fNodes.Add(Result);
  Result.NoWarning := WarnSuppression;
end;

function TSymbolTable.NewNode(typ: TNodeType; val, line: Integer): TNode;
begin
  Result := NewNode(typ,nil,line);
  Result.val := val;
end;

function TSymbolTable.NewNode(typ: TNodeType; sub: TNode): TNode;
begin
  Result := NewNode(typ,nil,0);
  Result.sub := sub;
end;

procedure TSymbolTable.MakeFirstAlt(var nL, nR: TNode);
begin
  nL := NewNode(ntAlt, nL);
  if nL.sub<>nil then
    nL.line := nL.sub.line;
  nL.next := nR;
  nR := nL;
end;

procedure TSymbolTable.MakeAlt(nL1,nR1,nL2,nR2: TNode);
var p: TNode;
begin
  nL2 := NewNode(ntAlt, nL2);
  if nL2.sub<>nil then
    nL2.line := nL2.sub.line;
  p := nL1;
  while p.down<>nil do p := p.down;
  p.down := nL2;
  p := nR1;
  while p.next<>nil do p := p.next;
  p.next := nL2;
  nL2.next := nR2;
end;

procedure TSymbolTable.MakeIter(var nL, nR: TNode);
var p, q: TNode;
begin
  nL := NewNode(ntIter, nL);
  nL.line := nL.sub.line;
  p := nR;
  nR := nL;
  while p<>nil do
  begin
    q := p.next;
    p.next := nL;
    p.up := True;
    p := q;
  end;
end;

procedure TSymbolTable.MakeOpt(var nL, nR: TNode);
begin
  nL := NewNode(ntOpt, nL);
  nL.line := nL.sub.line;
  nL.next := nR;
  nR := nL;
end;

procedure TSymbolTable.MakeSeq(nL1: TNode; var nR1: TNode; nL2,nR2: TNode);
var p, q: TNode;
begin
  p := nR1.next;
  nR1.next := nL2;
  while p<>nil do
  begin
    q := p.next;
    p.next := nL2;
    p.up := True;
    p := q;
  end;
  nR1 := nR2;
end;

function TSymbolTable.GetCharClassName(Idx: Integer): String;
begin
  Result := fCharClassNames[Idx];
end;

function TSymbolTable.getCharClassByName(const name: String): Integer;
begin
  Result := fCharClassNames.IndexOf(name);
end;

function TSymbolTable.getCharClassCount: Integer;
begin
  Result := fCharClassNames.Count;
end;

function TSymbolTable.getCharClassSet(Idx: Integer): TCharSet;
begin
  Result := TCharSet(fCharClassNames.Objects[Idx]);
end;

function TSymbolTable.NewCharClass(name: String; cset: TCharSet): Integer;
begin
  if name[1] = '#' then
  begin
    name[2] := Chr(Ord('A') + fDummyName);
    Inc(fDummyName);
  end;
  if cset=nil then cset := TCharSet.Create;
  Result := fCharClassNames.AddObject(name,cset);
end;

function TSymbolTable.FindCharClass(cset: TCharSet): Integer;
var I : integer;
begin
  for I := 0 to fCharClassNames.Count - 1 do
  if TCharSet(fCharClassNames.Objects[I]).Equals(cset) then
  begin
    Result := I;
    Exit;
  end;
  Result := -1;
end;

function TSymbolTable.getLiteral(const Name: String): TSymbol;
var I: Integer;
begin
  if (fLiterals<>nil)and fLiterals.Find(Name,I) then
    Result := TtSymbol(fLiterals.Objects[I])
  else Result := nil;
end;

procedure TSymbolTable.setLiteral(const aName: String; const Value: TSymbol);
begin
  if fLiterals=nil then
  begin
    fLiterals := TStringList.Create;
    with fLiterals do
    begin
      CaseSensitive := not fIgnoreCase;
      Sorted := True;
      Duplicates := dupIgnore;
    end;
  end;
  fLiterals.AddObject(aName,Value);
  if Value is TtSymbol then
  with TtSymbol(Value) do
    if fIgnoreCase then
      literal := UpperCase(aName)
    else literal := aName;
end;

procedure TSymbolTable.setFrameName(const Value: String);
begin
  if (Value<>'')and((Value[1]='"')or(Value[1]='''')) then
    fFrameName := AnsiDequotedStr(Value,Value[1])
  else fFrameName := '';
end;

procedure TSymbolTable.setIgnoreCase(const Value: Boolean);
begin
  fIgnoreCase := Value;
  if fLiterals<>nil then
    fLiterals.CaseSensitive := not Value;
end;

function TSymbolTable.getTemplateVar(const name: String): PSymbol;
var I: Integer;
begin
  if (fTemplateVars=nil)or not fTemplateVars.Find(Name,I) then
     Result := nil
  else Result := PSymbol(fTemplateVars.Objects[I]);
end;

procedure TSymbolTable.setTemplateVar(const name: String;
  const Value: PSymbol);
var  P: PSymbol;
     I: Integer;
begin
  if fTemplateVars=nil then
  begin
    fTemplateVars := TStringList.Create;
    fTemplateVars.Sorted := True;
  end else if fTemplateVars.Find(name,I) then
  begin
    Parser.SemError(201,name);
    Exit;
  end;
  New(P);
  P^ := Value^;
  fTemplateVars.AddObject(name,TObject(P));
end;

procedure TSymbolTable.StrToGraph(const str: String; var nL, nR: TNode);
var I: integer; p: TNode;
begin
  nR := fDummyNode;
  for I := 2 to Length(str) - 1 do
  begin
    p := NewNode(ntChr, Ord(str[i]), 0);
    nR.next := p; nR := p;
  end;
  nL := fDummyNode.next;
  fDummyNode.next := nil;
end;

procedure TSymbolTable.SetContextTrans(p: TNode);
begin
  while p<>nil do
  begin
    case p.typ of
      ntChr, ntCharClass:
        p.code := contextTrans;
      ntOpt,ntIter:
        SetContextTrans(p.sub);
      ntAlt:
        begin
          SetContextTrans(p.sub);
          SetContextTrans(p.down);
        end;
    end;
    if p.up then Break;
    p := p.next;
  end;
end;

procedure TSymbolTable.CompFirstSet(gn: TNode; rset: TSet);
var visited: TSet;

  procedure CompFirst(gn: TNode; rset: TSet);
  begin
    while (gn <> nil) and not visited[gn.index] do
    begin
      visited[gn.index] := True;
      case gn.typ of
        ntNonTerminal:
            with TNtSymbol(gn.sym) do
            if first<>nil then
              rset.Unite(first)
            else CompFirst(graph,rset);
        ntTerminal, ntWeakTerminal:
          rset[gn.sym.index] := True;
        ntAny:
          rset.Unite(gn.aset);
        ntAlt, ntIter, ntOpt:
          begin
            CompFirst(gn.sub,rset);
            if gn.typ=ntAlt then
              CompFirst(gn.down,rset);
          end;
      end;
      if not gn.IsDeletableNode then
        Exit;
      gn := gn.next;
    end;
  end;

begin
  visited := TSet.Create(fNodes.Count);
  try
    rset.Clear;
    CompFirst(gn,rset);
  finally
    visited.Free;
  end;
end;

procedure TSymbolTable.CompFirstSets;
var I: Integer;
    fs: TSet;
begin
   for I := 0 to fNonterminals.Count - 1 do
   with TNtSymbol(fNonterminals[I]) do
     FreeAndNil(first);

   for I := 0 to fNonterminals.Count - 1 do
   begin
     fs := TSet.Create(fTerminals.Count);
     with TNtSymbol(fNonterminals[I]) do
     begin
       CompFirstSet(graph,fs);
       first := fs;
     end;
   end;
end;

procedure TSymbolTable.CompFollowSets;
var I: Integer;
  visited: TSet;
  curSy: TNtSymbol;

  procedure CompFollow(gn: TNode);
  var s : TSet;
  begin
   s := TSet.Create;
   try
    while (gn<>nil) and not visited[gn.index] do
    begin
      visited[gn.index] := True;
      if gn.typ = ntNonTerminal then
      begin
        CompFirstSet(gn.next,s);
        with gn.sym as TNtSymbol do
        begin
          follow.Unite(s);
          if gn.next.IsDeletableGraph then
            nts[curSy.index] := True;
        end;
      end
      else if (gn.typ = ntOpt) or (gn.typ = ntIter) then
        CompFollow(gn.sub)
      else if gn.typ = ntAlt then
      begin
        CompFollow(gn.sub);
        CompFollow(gn.down);
      end;
      gn := gn.next;
    end;
   finally
     s.Free;
   end;
  end;

  procedure Complete(sym: TNtSymbol);
  var I: integer;
  begin
    if visited[sym.index] then Exit;
    visited[sym.index] := True;

    for I := 0 to fNonterminals.Count-1 do
    with TNtSymbol(fNonterminals[I]) do
    if sym.nts[index] then
    begin
      Complete(TNtSymbol(fNonterminals[I]));
      sym.follow.Unite(follow);
      if sym=curSy then
        sym.nts[I] := False;
    end;
  end;

begin
  visited := TSet.Create(fNodes.Count);
  try
    for I := 0 to fNonterminals.Count - 1 do
    with TNtSymbol(fNonterminals[I]) do
    begin
      follow.Free;
      follow := TSet.Create(fTerminals.Count);
      nts.Free;
      nts := TSet.Create(fNonterminals.Count);
    end;
    gramSy.follow[eofSy.index] := True;

    for I := 0 to fNonterminals.Count - 1 do
    begin
      curSy := TNtSymbol(fNonterminals[I]);
      CompFollow(curSy.graph);
    end;

    visited.Size := fNonterminals.Count;
    for I := 0 to fNonterminals.Count - 1 do
    begin
       visited.Clear;
       curSy := TNtSymbol(fNonterminals[I]);
       Complete(curSy);
    end;
  finally
    visited.Free;
  end;
end;

procedure TSymbolTable.CompExpected(gn: TNode; curNT: TNtSymbol; s: TSet);
begin
  s.Clear;
  CompFirstSet(gn,s);
  if gn.IsDeletableGraph then
    s.Unite(curNT.follow);
end;

procedure TSymbolTable.CompAnySets;
var curSy: TNtSymbol;

  function LeadingAny(gn: TNode; var a: TNode): Boolean;
  begin
    Result := False;
    if gn=nil then Exit;

    if gn.typ = ntAny then
    begin
      a := gn;
      Result := True;
    end
    else if gn.typ = ntAlt then
      Result := LeadingAny(gn.sub, a) or LeadingAny(gn.down, a)
    else if (gn.typ = ntOpt)or(gn.typ = ntIter) then
      Result := LeadingAny(gn.sub, a)
    else if gn.IsDeletableNode and not gn.up then
      Result := LeadingAny(gn.next,a);
  end;

  procedure FindAS(gn: TNode);
  var a,q: TNode;
    s1,s2: TSet;
  begin
   s1 := TSet.Create(fTerminals.Count);
   s2 := TSet.Create(fTerminals.Count);
   try
    while gn<>nil do
    begin
      if (gn.typ = ntOpt)or(gn.typ = ntIter) then
      begin
        FindAS(gn.sub);
        if LeadingAny(gn.sub, a) then
        begin
          CompExpected(gn.next, curSy,s1);
          a.aset.Subtract(s1);
        end;
      end
      else if gn.typ = ntAlt then
      begin
        q := gn;
        s1.Clear;
        while q<>nil do
        begin
          FindAS(q.sub);
          if LeadingAny(q.sub, a) then
          begin
            CompExpected(q.down, curSy, s2);
            s2.Unite(s1);
            a.aset.Subtract(s2);
          end
          else begin
            CompFirstSet(q.sub, s2);
            s1.Unite(s2);
          end;
          q := q.down;
        end;
      end;
 		  if gn.up then  Break;
   	  gn := gn.next;
    end;
   finally
     s1.Free; s2.Free;
   end;
  end;

var I: Integer;
begin
  for I := 0 to fNonterminals.Count - 1 do
  begin
    curSy := TNtSymbol(fNonterminals[I]);
    FindAS(curSy.graph);
  end;
end;

procedure TSymbolTable.CompSyncSets;
var visited: TSet;
  curSy: TNtSymbol;

 procedure CompSync(gn: TNode);
 var s: TSet;
 begin
   s := TSet.Create(fTerminals.Count);
   try
    while (gn<>nil) and not visited[gn.index] do
    begin
      visited[gn.index] := True;
      if gn.typ = ntSync then
      begin
        CompExpected(gn.next, curSy, s);
        s[eofSy.index] := True;
        AllSyncSets.Unite(s);
        gn.aset := s;
      end
      else if gn.typ = ntAlt then
      begin
        CompSync(gn.sub);
        CompSync(gn.down);
      end
      else if (gn.typ = ntIter) or (gn.typ = ntOpt) then
        CompSync(gn.sub);
      gn := gn.next;
    end;
   finally
     s.Free;
   end;
 end;

var I: Integer;
begin
  fAllSyncSets.Size := fTerminals.Count;
  fAllSyncSets.Clear;
  fAllSyncSets[eofSy.index] := True;
  visited := TSet.Create(fNodes.Count);
  try
    for I := 0 to fNonterminals.Count - 1 do
    begin
      curSy := TNtSymbol(fNonterminals[I]);
      CompSync(curSy.graph);
    end;
  finally
    visited.Free;
  end;
end;

procedure TSymbolTable.SetupAnys;
var I: Integer;
  s: TSet;
begin
  s := TSet.Create(fTerminals.Count);
  s.Fill;
  s[Self.eofSy.index] := False;
  try
    for I := 0 to fNodes.Count - 1 do
    with TNode(fNodes[I]) do
    if typ=ntAny then
       aset := s;
  finally
    s.Free;
  end;
end;

procedure TSymbolTable.CompDeletableSymbols;
var changed: Boolean;
    I: integer;
begin
  fDeletableCount := 0;
  repeat
    changed := false;
    for I := 0 to fNonterminals.Count - 1 do
    with TNtSymbol(fNonterminals[I]) do
    begin
      if not deletable and (graph<>nil) and graph.IsDeletableGraph then
      begin
        deletable := true;
        Inc(fDeletableCount);
        changed := true;
      end;
    end;
  until not changed;
end;

procedure TSymbolTable.RenumberPragmas;
var I,n: Integer;
begin
  n := fTerminals.Count;
  for I := 0 to fPagmas.Count - 1 do
  begin
    TSymbol(fPagmas[I]).index := n;
    Inc(n);
  end;
end;

function TSymbolTable.CountNondefinedTerminals: Integer;
var I: Integer;
begin
  fNondefinedTerminals := 0;
  for I := 0 to NonTerminalCount - 1 do
  with NonTerminals[I] do
  if graph=nil then
  begin
    Inc(fNondefinedTerminals);
    Parser.SemErrorInLine(-1,line,Format('Terminal "%s" is not defined',[Name]));
  end;
  Result := fNondefinedTerminals;
end;

function TSymbolTable.CountUnreachableNt: Integer;

  procedure MarkReachedNts(p: TNode);
  begin
    while p<>nil do
    begin
      if (p.typ=ntNonTerminal) and not fReachables[p.sym.index] then
      begin
        fReachables[p.sym.index] := True;
        MarkReachedNts(TNtSymbol(p.sym).graph);
      end else if p.typ in [ntAlt, ntIter, ntOpt] then
      begin
        MarkReachedNts(p.sub);
        if p.typ=ntAlt then
          MarkReachedNts(p.down);
      end;
      if p.up then Break;
      p := p.next;
    end;
  end;

var I: Integer;
begin
  fUnreachableNonterminals := 0;
  fReachables.Clear;
  fReachables.Size := NonTerminalCount;
  fReachables[gramSy.index] := True;
  MarkReachedNts(gramSy.graph);

  for I := 0 to NonTerminalCount - 1 do
  with NonTerminals[I] do
  if not fReachables[index] then
  begin
    Inc(fUnreachableNonterminals);
    Parser.SemErrorInLine(-1,line,Format('Nonterminal "%s" is unreachable',[Name]));
  end;
  Result := fUnreachableNonterminals;
end;

function TSymbolTable.CountCircularProductions: Integer;
var I,J,C: Integer;
    singles, pairs: TList;
    changed,onLeftSide, onRightSide: Boolean;

  procedure GetSingles(p: TNode);
  begin
    if p=nil then Exit;
    if p.typ=ntNonTerminal then
    begin
      if p.up or p.next.IsDeletableGraph then
        singles.Add(p.sym);
    end else if p.typ in [ntAlt, ntIter, ntOpt] then
    begin
      if p.up or p.next.IsDeletableGraph then
      begin
        GetSingles(p.sub);
        if p.typ=ntAlt then GetSingles(p.down);
      end;
    end;
    if (not p.up) and p.IsDeletableNode then
      GetSingles(p.next);
  end;

begin
 singles := TList.Create;
 pairs := TList.Create;
 try
  for I := 0 to NonTerminalCount - 1 do
  with NonTerminals[I] do
  begin
    singles.Clear;
    GetSingles(graph);
    for J := 0 to singles.Count - 1 do
    begin
      pairs.Add(NonTerminals[I]); pairs.Add(singles[J]);
    end;
  end;
  C := pairs.Count div 2;
  repeat
    changed := False;
    for I := 0 to C-1 do
     if pairs[2*I]<>nil then
     begin
       onLeftSide := False; onRightSide := False;
       for J := 0 to C-1 do
       if pairs[2*J]<>nil then
       begin
				 if pairs[2*I]=pairs[2*J+1] then onRightSide := True;
				 if pairs[2*I+1]=pairs[2*J] then onLeftSide := True;
       end;
			 if (not onLeftSide) or (not onRightSide) then
       begin
         pairs[2*I] := nil; changed := True;
       end;
     end;
  until not changed;
  fCircularProductions := 0;
  for I := 0 to C-1 do
  if pairs[2*I]<>nil then
  begin
    Inc(fCircularProductions);
    Parser.SemErrorInLine(-1,TSymbol(pairs[2*I+1]).line,Format('%s --> %s is a circular derivation',
      [TSymbol(pairs[2*I]).Name,TSymbol(pairs[2*I+1]).Name]));
  end;
  Result := fCircularProductions;
 finally
   pairs.Free;
   singles.Free;
 end;
end;

function TSymbolTable.CountHomographs: Integer;
var I: Integer;
begin
  Result := 0;
  for I := 0 to TerminalCount - 1 do
    if Terminals[I].homograph then Inc(Result);
end;

function TSymbolTable.CountNonderivedNonterminals: Integer;
var I: Integer; changed: Boolean;  mark: TSet;

  function IsTerm(p: TNode): Boolean;
  begin
    Result := False;
    while p<>nil do
    begin
      if (p.typ=ntNonTerminal) and not mark[p.sym.index] then Exit;
      if (p.typ=ntAlt) and (not IsTerm(p.sub)) and
         ((p.down=nil)or not IsTerm(p.down)) then Exit;

      if p.up then Break;
      p := p.next;
    end;
    Result := True;
  end;
begin
  mark := TSet.Create(NonTerminalCount);
  try
    repeat
      changed := False;
      for I := 0 to NonTerminalCount - 1 do
      with NonTerminals[I] do
      if (not mark[index]) and IsTerm(graph) then
      begin
        mark[index] := True; changed := True;
      end;
    until not changed;

    fNonderivedNonterminals := 0;
    for I := 0 to NonTerminalCount - 1 do
    with NonTerminals[I] do
    if not mark[index] then
    begin
      Inc(fNonderivedNonterminals);
      Parser.SemErrorInLine(-1,line,Format('%s is an underivable nonterminal',[Name]));
    end;
    Result := fNonderivedNonterminals;
  finally
    mark.Free;
  end;
end;

procedure TSymbolTable.CheckResolvers;
var curSy: TNtSymbol;

  procedure ResErr(p: TNode; const msg: String);
  begin
    if not p.NoWarning then
      Parser.Warning(p.pos.Line,msg);
  end;

  procedure CheckRes(p: TNode; rslvAllowed: Boolean);
  var fs1,fs2, fs3: TSet; q: TNode;
  begin
   fs1 := TSet.Create(fTerminals.Count);
   fs2 := TSet.Create(fTerminals.Count);
   fs3 := TSet.Create(fTerminals.Count);
   try
    while p<>nil do
    begin
      if p.typ=ntAlt then
      begin
        fs1.Clear;
        q := p;
        while q<>nil do
        begin
          if q.sub.typ<>ntIf then
          begin
            CompExpected(q.sub, curSy, fs2);
            fs1.Unite(fs2);
          end;
          q := q.down;
        end;
        fs2.Clear;
        q := p;
        while q<>nil do
        begin
          if q.sub.typ=ntIf then
          begin
            CompExpected(q.sub.next, curSy, fs3);
            if  fs3.Intersects(fs2) then
              ResErr(q.sub,'Resolver will never be evaluated. Place it at previous conflicting alternative');
            if not fs3.Intersects(fs1) then
              ResErr(q.sub,'Misplaced resolver: no LL(1) conflict');
          end else begin
            CompExpected(q.sub, curSy, fs3);
            fs2.Unite(fs3);
          end;
          CheckRes(q.sub, True);
          q := q.down;
        end;
      end else if p.typ in [ntIter, ntOpt] then
      begin
        if p.sub.typ=ntIf then
        begin
          fs1.Clear; fs2.Clear;
          CompFirstSet(p.sub.next,fs1);
          CompExpected(p.next, curSy, fs2);
          if not fs1.Intersects(fs2) then
            ResErr(p.sub,'Misplaced resolver: no LL(1) conflict');
        end;
			  CheckRes(p.sub, True);
      end else if (p.typ=ntIf) and not rslvAllowed then
         ResErr(p,'Misplaced resolver: no alternative');

      if p.up then Break;
      p := p.next;
      rslvAllowed := False;
    end;
   finally
     fs1.Free; fs2.Free; fs3.Free;
   end;
  end;

var I: Integer;
begin
  for I := 0 to NonTerminalCount - 1 do
  begin
    curSy := NonTerminals[I];
    CheckRes(curSy.graph,False);
  end;
end;

procedure TSymbolTable.CheckLL1;
var curSy: TNtSymbol; I: Integer;

  procedure CheckAlts(p: TNode);
  var s1,s2: TSet;
    q: TNode;

    procedure LL1Error(cond: Integer; sym: TSymbol);
    const msgs: array[1..4] of string = (
      'start of several alternatives',
      'start & successor of deletable structure',
      'an ANY node that matches no symbol',
      'contents of [...] or {...} must not be deletable'
    );
    var str: String;
    begin
      if sym<>nil then str := Format('%s is',[sym.Name]);
      Parser.Warning(0,Format('LL1 warning in %s:%s %s',[curSy.Name,str,msgs[cond]]));
    end;

    procedure CheckOverlap(cond: Integer);
    var I: Integer;
    begin
      for I := 0 to TerminalCount - 1 do
      if s1[I] and s2[I] then
         LL1Error(cond, Terminals[I]);
    end;

  begin
   s1 := TSet.Create(TerminalCount);
   s2 := TSet.Create(TerminalCount);
   try
    while p<>nil do
    begin
      if not p.NoWarning then
      begin
        if p.typ=ntAlt then
        begin
          q := p;
          s1.Clear;
          while q<>nil do
          begin
            if q.sub.typ<>ntIf then
            begin
              CompExpected(q.sub, curSy, s2);
              CheckOverlap(1);
              s1.Unite(s2);
            end;
            CheckAlts(q.sub);
            q := q.down;
          end;
        end else if p.typ in [ntIter, ntOpt] then
        begin
          if p.sub.IsDeletableSubGraph then
             LL1Error(4, nil)
          else begin
            if p.sub.typ<>ntIf then
            begin
              CompExpected(p.sub, curSy, s1);
              CompExpected(p.next, curSy, s2);
              CheckOverlap(2);
            end;
          end;
          CheckAlts(p.sub);
        end else if (p.typ=ntAny)and p.aset.Empty then
           LL1Error(3, nil);
      end;
      if p.up then Break;
      p := p.next;
    end;
   finally
     s1.Free; s2.Free;
   end;
  end;

begin
  for I := 0 to NonTerminalCount - 1 do
  begin
    curSy := NonTerminals[I];
    CheckAlts(curSy.graph);
  end;
end;


function TSymbolTable.FixStringForToken(const name: string): String;
begin
  Result := AnsiDequotedStr(name,name[1]);
  if name='' then
    Parser.SemError(202)
  else begin
    if IgnoreCase then
      Result := UpperCase(Result);
    if Pos(' ',Result)<>0 then
      Parser.SemError(203);
  end;
  if Pos('"',Result)=0 then
     Result := Format('"%s"',[Result])
  else Result := QuotedStr(Result);   
end;

end.
