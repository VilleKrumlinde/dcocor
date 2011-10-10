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

unit CRA;

interface
uses SysUtils, Classes, Contnrs, Sets, CharSets, CRTypes, CocoAncestor, CRT;

type
  TState = class;

  TComment = class
  public
    start: TSymbol;
    stop: String;
    nested: Boolean;
    constructor Create(start_: TSymbol; const stop_: String; nested_: Boolean);
  end;

  TAction = class
  private
    fTargets: TList;
    function getTarget(I: Integer): TState;
    function getTargetCount: Integer;
  public
    typ:TNodeType;   // type of action symbol: char, class
    sym,             // action symbol
    tc: Integer;     // transition code: normTrans, contextTrans
    next: TAction;

	  constructor Create(atyp: TNodeType; asym,atc: Integer);
    destructor Destroy; override;

   	procedure AddTarget(state: TState);  // add s to the action.targets
	  procedure AddTargets(a: TAction); // add copy of a.targets to targets
    procedure SetTarget(state: TState);
	  function  Symbols(tab: TSymbolTable): TCharSet; //construct set
	  procedure ShiftWith(s: TCharSet; tab: TSymbolTable);

    property Targets[I: Integer]: TState read getTarget; // states after transition with input symbol
    property TargetCount: Integer read getTargetCount;
  end;

  TState = class
  private
    fMeltedSet: TSet;
    procedure FreeActionList(a: TAction);
  public
    index: Integer;
    firstAction: TAction;
    endOf: TSymbol; // recognized token if state is final
    ctx: Boolean;   // true if state is reached via contextTrans
    destructor Destroy; override;

    procedure AddAction(act: TAction);
    procedure FreeAction(act: TAction);
    procedure MeltWith(s: TState);
    function FindAction(ch: Char; Tab: TSymbolTable): TAction;
  end;

  TAutomaton = class
  private
    fParser: TCocoRGrammar;
    fTab: TSymbolTable;
    fStates, fComments: TObjectList;

    FHasCtxMoves: Boolean;
    fDirtyDFA: Boolean;
    function getState(Index: Integer): TState;
    function getStatesCount: Integer;
    function getComment(I: Integer): TComment;
    function getCommentCount: Integer;
  protected
    procedure Clear;
    procedure Init;
  public
    constructor Create(aParser: TCocoRGrammar; aTab: TSymbolTable);
    destructor Destroy; override;

    procedure Reinit;

    procedure MakeDeterministic;
    procedure MatchLiteral(const s: String; sym: TSymbol);
    procedure ConvertToStates(aNode: TNode; aSym: TSymbol);
    procedure NewComment(fromT, toT: TNode; nested: Boolean);

    function NewState: TState;
    procedure NewTransition(from, to_: TState; typ: TNodeType; sym, tc: Integer);

//    function GenStartTable: TStartTable;

    property Parser: TCocoRGrammar read fParser;
    property SymbolTable: TSymbolTable read fTab;

    property HasCtxMoves: Boolean read FHasCtxMoves write FHasCtxMoves;
    property States[Index: Integer]: TState read getState;
    property StatesCount: Integer read getStatesCount;
    property DirtyDFA: Boolean read fDirtyDFA;
    property Comments[I: Integer]: TComment read getComment;
    property CommentCount: Integer read getCommentCount;
  end;

implementation

{ TComment }

constructor TComment.Create(start_: TSymbol; const stop_: String; nested_: Boolean);
begin
  start := start_;
  stop  := stop_;
  nested := nested_;
end;

{ TState }

destructor TState.Destroy;
begin
  FreeActionList(firstAction);
  fMeltedSet.Free;
  inherited;
end;

procedure TState.FreeActionList(a: TAction);
var tmp: TAction;
begin
  while a <> nil do
  begin
    tmp := a.next;
    a.Free;
    a := tmp;
  end
end;

procedure TState.AddAction(act: TAction);
var lasta,a: TAction;
begin
  lasta := nil;
  a := firstAction;
  while (a<>nil)and(act.typ>=a.typ) do
  begin lasta := a; a := a.next; end;
  act.next := a;
	if a=firstAction then
    firstAction := act
  else lasta.next := act;
end;

procedure TState.MeltWith(s: TState);
var a,act: TAction;
begin
  act := s.firstAction;
  while act<>nil do
  begin
		a := TAction.Create(act.typ, act.sym, act.tc);
		a.AddTargets(act);
		AddAction(a);
    act := act.next;
  end;
end;

procedure TState.FreeAction(act: TAction);
var lasta,a: TAction;
begin
  lasta := nil;
  a := firstAction;
  while (a<>nil)and(a<>act) do
  begin lasta := a; a := a.next; end;

  if a<>nil then
    if a=firstAction then
     firstAction := a.next
    else lasta.next := a.next;
  act.Free;
end;

function TState.FindAction(ch: Char; Tab: TSymbolTable): TAction;
begin
  Result := firstAction;
  while Result<>nil do
  begin
    if ((Result.typ=ntChr)and(Ord(ch)=Result.sym)) or
       ((Result.typ=ntCharClass) and Tab.CharClassSet[Result.sym][Ord(ch)])
    then  Exit;
    Result := Result.next;
  end;
end;

{ TAction }

constructor TAction.Create(atyp: TNodeType; asym, atc: Integer);
begin
  typ := atyp;
  sym := asym;
  tc  := atc;
  fTargets := TList.Create;
end;

destructor TAction.Destroy;
begin
  fTargets.Free;
  inherited;
end;

function TAction.getTarget(I: Integer): TState;
begin
  Result := TState(fTargets[I]);
end;

function TAction.getTargetCount: Integer;
begin
  Result := fTargets.Count;
end;

procedure TAction.AddTarget(state: TState);
var I: Integer;
begin
  if fTargets.Count<>0 then
    for I := 0 to fTargets.Count - 1 do
    if state=TState(fTargets[I]) then Exit
    else if TState(fTargets[I]).index>state.index then
    begin
      fTargets.Insert(I,state);
      Exit;
    end;
  fTargets.Add(state);
end;

procedure TAction.AddTargets(a: TAction);
var I: Integer;
begin
  for I := 0 to a.fTargets.Count - 1 do
    AddTarget(TState(a.fTargets[I]));
	if a.tc=contextTrans then tc := contextTrans;
end;

procedure TAction.SetTarget(state: TState);
begin
  fTargets.Clear;
  fTargets.Add(state);
end;

procedure TAction.ShiftWith(s: TCharSet; tab: TSymbolTable);
var first,c: Integer;
begin
	if s.IsOne(first) then begin
		typ := ntChr; sym := first;
	end else begin
    c := tab.FindCharClass(s);
		if c<0 then c := tab.NewCharClass('##',s.Clone);
		typ := ntCharClass; sym := c;
	end;
end;

function TAction.Symbols(tab: TSymbolTable): TCharSet;
begin
	if typ=ntCharClass then
		Result := tab.CharClassSet[sym].Clone
	else begin
		Result := TCharSet.Create; Result.AddChar(sym);
	end;
end;

{ TAutomaton }

constructor TAutomaton.Create(aParser: TCocoRGrammar; aTab: TSymbolTable);
begin
  fParser := aParser;
  fTab := aTab;
  fStates := TObjectList.Create;
  Init;
end;

destructor TAutomaton.Destroy;
begin
  fStates.Free;
  fComments.Free;
  inherited;
end;

procedure TAutomaton.Clear;
begin
  FHasCtxMoves := False;
  fStates.Clear;
  if fComments<>nil then
    fComments.Clear;
end;

function TAutomaton.getState(Index: Integer): TState;
begin
  if Index<fStates.Count then
    Result := TState(fStates[Index])
  else Result := nil;  
end;

function TAutomaton.getStatesCount: Integer;
begin
  Result := fStates.Count;
end;

procedure TAutomaton.Init;
begin
  NewState;
  fDirtyDFA := False;
end;

function TAutomaton.NewState: TState;
begin
  Result := TState.Create;
  Result.index := fStates.Add(Result);
end;

procedure TAutomaton.NewTransition(from, to_: TState; typ: TNodeType; sym,
  tc: Integer);
var a: TAction;
begin
  a := TAction.Create(typ,sym,tc);
  a.AddTarget(to_);
  from.AddAction(a);
end;

procedure TAutomaton.Reinit;
begin
  Clear;
  Init;
end;

procedure TAutomaton.ConvertToStates(aNode: TNode; aSym: TSymbol);
var curGraph: TNode; curSym: TSymbol;
  visited,_stepped: TSet;

  procedure NumberNodes(gn: TNode; state: TState; renumIter: Boolean);
  begin
    if gn=nil then Exit;
    if gn.state<>nil then Exit;
	  if (state=nil)or((gn.typ=ntIter)and renumIter) then
      state := NewState;
   	gn.state := state;
  	if gn.IsDeletableGraph then state.endOf := curSym;

    case gn.typ of
      ntChr, ntCharClass:
        NumberNodes(gn.next,nil,false);
      ntOpt:
        begin
          NumberNodes(gn.next,nil,False);
          NumberNodes(gn.sub, state,True);
        end;
      ntIter:
        begin
          NumberNodes(gn.next, state,True);
          NumberNodes(gn.sub, state,True);
        end;
      ntAlt:
        begin
          NumberNodes(gn.next, nil,False);
          NumberNodes(gn.sub, state,True);
          NumberNodes(gn.down, state,renumIter);
        end;
    end;
  end;

  function TheState(gn: TNode): TState;
  begin
    if gn=nil then
    begin
      Result := NewState;
      Result.endOf := curSym;
    end else Result := TState(gn.state);
  end;

  procedure Step(from: TState; gn: TNode; stepped: TSet);
  var newStepped: TSet;
  begin
    if gn=nil then Exit;
    stepped[gn.index] := True;

    case gn.typ of
      ntChr, ntCharClass:
        begin
          NewTransition(from, TheState(gn.next), gn.typ, gn.val,gn.code);
          if gn.typ=ntCharClass then
             curSym.kind := tkClass;
        end;
      ntAlt:
        begin
          Step(from, gn.sub, stepped);
          Step(from, gn.down, stepped);
        end;
      ntIter, ntOpt:
        begin
          if (gn.next<>nil) and not stepped[gn.next.index] then
            Step(from, gn.next, stepped);
          Step(from, gn.sub, stepped);
          if (gn.typ=ntIter)and(TState(gn.state)<>from) then
          begin
            newStepped := TSet.Create(SymbolTable.NodesCount);
            try
              Step(TState(gn.state),gn,newStepped);
            finally
              newStepped.Free;
            end;
          end;
        end;
    end;
  end;

  procedure FindTrans(gn: TNode; start: Boolean);
  begin
    if (gn=nil) or visited[gn.index] then Exit;
    visited[gn.index] := True;
    if start then
      begin
        _stepped.Clear;
        Step(TState(gn.state), gn, _stepped);
      end;
    case gn.typ of
      ntChr, ntCharClass:
        FindTrans(gn.next, True);
      ntOpt:
        begin
          FindTrans(gn.next, True);
          FindTrans(gn.sub, False);
        end;
      ntIter:
        begin
          FindTrans(gn.next, False);
          FindTrans(gn.sub, False);
        end;
      ntAlt:
        begin
          FindTrans(gn.sub, False);
          FindTrans(gn.down, False);
        end;
    end;
  end;

begin
  visited := TSet.Create(SymbolTable.NodesCount);
  _stepped := TSet.Create(SymbolTable.NodesCount);
  try
    curGraph := aNode; curSym := aSym;
    if aNode.IsDeletableGraph then
      Parser.SemError(204);
    NumberNodes(curGraph, States[0], true);
    FindTrans(curGraph, True);
    if aNode.typ=ntIter then
    begin
      _stepped.Clear;
      Step(States[0],aNode,_stepped);
    end;
  finally
    visited.Free;
    _stepped.Free;
  end;
end;

procedure TAutomaton.MatchLiteral(const s: String; sym: TSymbol);
var state,sto: TState;
    a: TAction;
    I,Len: Integer;
    str: String;
    matchedSym: TSymbol;
begin
  str := AnsiDequotedStr(s,s[1]);
  state := States[0];
  a := nil;
  len := Length(str);
  I := 1;
  while I<=Len do
  begin
    a := state.FindAction(str[I],SymbolTable);
    if a = nil then Break;
    state := a.Targets[0];
    Inc(I);
  end;
	if (I<=Len)or(state.endOf=nil) then
  begin
		state := States[0]; I := 1; a := nil;
		fDirtyDFA := True;
	end;

  while I<=Len do
  begin
    sto := NewState;
    NewTransition(state,sto, ntChr, Ord(str[I]),normalTrans);
    state := sto;
    Inc(I);
  end;

  matchedSym := state.endOf;
  if state.endOf=nil then
    state.endOf := sym
  else if (matchedSym.kind=tkFixed)or((a<>nil)and(a.tc=contextTrans)) then
    Parser.SemError(205, Format('%s and %s',[sym.Name,state.endOf.Name]))
  else  begin
    matchedSym.kind := tkClassLit;
    sym.kind := tkLit;
  end;
end;

procedure TAutomaton.MakeDeterministic;
var FirstMeltedState: Integer;

  function MakeUnique(state: TState): Boolean;

    function Overlap(a,b: TAction): Boolean;
    var seta: TCharSet;
    begin
      if a.typ = ntChr then
      begin
        if b.typ = ntChr then
          Result := a.sym = b.sym
        else
          Result := SymbolTable.CharClassSet[b.sym][a.sym];
      end
      else begin
        seta := SymbolTable.CharClassSet[a.sym];
        if b.typ = ntChr then
          Result := seta[b.sym]
        else
          Result := seta.Intersects(SymbolTable.CharClassSet[b.sym]);
      end;
    end;

    procedure SplitActions(a,b: TAction);
    var c: TAction;
        seta, setb, setc: TCharSet;
    begin
     seta := a.Symbols(SymbolTable);
     setb := b.Symbols(SymbolTable);
     setc := nil;
     try
      if seta.Equals(setb)  then
      begin
        a.AddTargets(b);
        state.FreeAction(b);
      end else if seta.Includes(setb) then
      begin
        setc := seta.Clone; setc.Subtract(setb);
        b.AddTargets(a);
        a.ShiftWith(setc,SymbolTable);
      end else if setb.Includes(seta) then
      begin
        setc := setb.Clone; setc.Subtract(seta);
        a.AddTargets(b);
        b.ShiftWith(setc,SymbolTable);
      end else begin
        setc := TCharSet.Intersection(seta,setb);
        seta.Subtract(setc);
        setb.Subtract(setc);
        a.ShiftWith(seta,SymbolTable);
        b.ShiftWith(setb,SymbolTable);
        c := TAction.Create(ntUnknown,0,normalTrans);
        c.AddTargets(a);
        c.AddTargets(b);
        c.ShiftWith(setc,SymbolTable);
        state.AddAction(c);
      end;
     finally
       seta.Free; setb.Free; setc.Free;
     end;
    end;

  var a,b: TAction;
  begin
    Result := False;
    a := state.firstAction;
    while a<>nil do
    begin
      b := a.next;
      while b <> nil do
      begin
        if Overlap(a,b) then
        begin
          SplitActions(a,b);
          Result := True;
        end;
        b := b.next;
      end;
      a := a.next;
    end;
  end;

  procedure FindCtxStates;
  var I: Integer; a: TAction;
  begin
    for I := 0 to fStates.Count - 1 do
    begin
      a := TState(fStates[I]).firstAction;
      while a<>nil do
      begin
        if a.tc=contextTrans then
          a.Targets[0].ctx := True;
        a := a.next;
      end;
    end;
  end;

  function GetTargetStates(a: TAction;
     var endOf: TSymbol; var _Ctx: Boolean): TSet;
  var I: Integer;
      s: TState;
  begin
    Result := TSet.Create(FirstMeltedState*2);
    endOf := nil;
    _Ctx := false;
    for I := 0 to a.TargetCount - 1 do
    begin
      s := a.Targets[I];
      if s.fMeltedSet<>nil then
        Result.Unite(s.fMeltedSet)
      else Result[s.index] := True;

      if s.endOf<>nil then
      begin
        if (endOf=nil)or(endOf=s.endOf) then
          endOf := s.endOf
        else
          Parser.SemError(205, Format('%s and %s',[endOf.Name, s.endOf.Name]));
      end;
      if s.ctx then
        _Ctx := True;
    end;
  end;

  procedure MeltStates(state: TState);

  function StateWithSet(targets: TSet): TState;
  var I: Integer;
  begin
    for I := FirstMeltedState to StatesCount - 1 do
    with States[I] do
    if (fMeltedSet<>nil) and fMeltedSet.Equals(targets) then
    begin
      Result := States[I]; Exit;
    end;
    Result := nil;
  end;

  var a: TAction;
      targets: TSet;
      endOf: TSymbol;
      ctx: Boolean;
      s: TState;
      I: Integer;
  begin
    a := state.firstAction;
    while a<>nil do
    begin
      if a.TargetCount>1 then
      begin
        targets := GetTargetStates(a,endOf, ctx);
        s := StateWithSet(targets);
        if s=nil then
        begin
          s := NewState; s.endOf := endOf; s.ctx := ctx;
          for I := 0 to a.TargetCount - 1 do
            s.MeltWith(a.Targets[I]);
          while MakeUnique(s) do ;
          s.fMeltedSet := targets;
        end
        else targets.Free;
        a.SetTarget(s);
      end;
      a := a.next;
    end;
  end;

  procedure DeleteRedundantStates;
  var used: TSet;
      newStates: array of TState;
      I,J: Integer;

    procedure FindUsedStates(state: TState);
    var a: TAction;
    begin
      if used[state.index] then Exit;
      used[state.index] := True;
      a := state.firstAction;
      while a<>nil do
      begin
        Assert(a.TargetCount=1);
        FindUsedStates(a.Targets[0]);
        a := a.next;
      end;
    end;

  var a: TAction;
  begin
   used := TSet.Create(StatesCount);
   try
    SetLength(newStates,StatesCount);

    FindUsedStates(States[0]);
    for I := 1 to StatesCount - 1 do
     if used[I] and (States[I].endOf<>nil) and (States[I].firstAction=nil) and not States[I].ctx then
     for J := I+1 to StatesCount - 1 do
       if used[J] and (States[I].endOf=States[J].endOf) and (States[J].firstAction=nil) and not States[J].ctx then
       begin
         used[J] := False; newStates[J] := States[I];
       end;

    for I := 0 to StatesCount - 1 do
     if used[I] then
     begin
       a := States[I].firstAction;
       while a<>nil do
       begin
         if not used[a.Targets[0].index]  then
            a.SetTarget(newStates[a.Targets[0].index])
         else a.SetTarget(a.Targets[0]);
         a := a.next;
       end;
     end;
    for I := 1 to StatesCount - 1 do
      if not used[I]  then fStates[I] := nil;
   finally
     used.Free;
     fStates.Pack;
     for I := 0 to StatesCount - 1 do
       States[I].index := I;
   end;
  end;

  procedure CombineShifts;
  var I: Integer;
    a,b,c: TAction;
    seta,setb: TCharSet;
  begin
    for I := 0 to StatesCount - 1 do
    begin
      a := States[I].firstAction;
      while a<>nil do
      begin
        b := a.next;
        while b<>nil do
        if (a.Targets[0]=b.Targets[0])and(a.tc=b.tc) then
        begin
          seta := a.Symbols(SymbolTable); setb := b.Symbols(SymbolTable);
          try
          seta.Unite(setb);
          a.ShiftWith(seta,SymbolTable);
          c := b; b := b.next; States[I].FreeAction(c);
          finally
            seta.Free; setb.Free;
          end;
        end  else b := b.next;
        a := a.next;
      end;
    end;
  end;

var I: Integer;
begin
  FirstMeltedState := StatesCount;
  FindCtxStates;
  for I := 0 to StatesCount - 1 do
    while MakeUnique(States[I]) do ;

  I := 0;
  while I<StatesCount do
  begin
    MeltStates(States[I]);
    Inc(I);
  end;

	DeleteRedundantStates;
	CombineShifts;
end;

function TAutomaton.getComment(I: Integer): TComment;
begin
  Result := TComment(fComments[I]);
end;

function TAutomaton.getCommentCount: Integer;
begin
  if fComments<>nil then Result := fComments.Count
  else Result := 0;
end;

procedure TAutomaton.NewComment(fromT, toT: TNode; nested: Boolean);

  function CommentStr(p: TNode): String;
  var s: TCharSet; I: Integer;
  begin
    Result := '';
    while p<>nil do
    begin
      case p.typ of
        ntChr: Result := Result+Char(p.val);
        ntCharClass:
          begin
            s := SymbolTable.CharClassSet[p.val];
            if s.IsOne(I) then
              Result := Result+Char(I)
            else  Parser.SemError(206);
          end;
        else Parser.SemError(207);
      end;
      p := p.next;
    end;
    I := Length(Result);
    if I=0 then
    begin
       Parser.SemError(208);
       Result := '?';
    end;
  end;
var sym: TSymbol; n: String;
begin
  if fComments=nil then
    fComments := TObjectList.Create;
  n := QuotedStr(CommentStr(fromT));
  sym := SymbolTable.NewSym(stPragma,n,0);
  fComments.Add(TComment.Create(sym,CommentStr(toT),nested));
  MatchLiteral(n, sym);
end;

end.
