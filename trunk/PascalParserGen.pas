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

unit PascalParserGen;

interface
uses SysUtils, Classes, CRTypes, ParserGen, CharSets, Sets, CRT, CRA;

type
  TPacalParserGenerator = class(TParserGenerator)
  protected
    procedure PrintProductionCode(const lineprefix: String; curSy: TNtSymbol; gn: TNode; checked: TSet);
    function GenCond(s: Tset; p: TNode): String;
    function GenCharSelector(s: TCharSet): String;
  public
    procedure PrintState(state: TState); override;
    procedure PrintParserDeclarations; override;
    procedure PrintTokens;             override;
    procedure PrintTokenStrings;       override;
    procedure PrintInitStartStates;    override;
    procedure PrintComments;       override;
    procedure PrintInitLiterals;       override;
    procedure PrintErrors;             override;
    procedure PrintIgnoreSet;          override;
    procedure PrintParserImplementation; override;
    procedure PrintPragmas;            override;
  end;


implementation
uses StrUtils, CocoAncestor;

type TCharSetAccess = class(TCharSet);

function GenStr(W: String): String;

  procedure WriteStr(const str: String);
  begin
    Result := Result+str;
  end;
  
var I,J,L: Integer;
begin
  L := Length(W);
  Result := '';
  if L = 0 then WriteStr('''''') else
  begin
    I := 1;
    repeat
      if (W[I] >= ' ') and (W[I] <> '''') and (Ord(W[i]) <= 127) then
      begin
        J := I;
        repeat
          Inc(I)
        until (I > L) or (W[I] < ' ') or (W[I] = '''') or
          (Ord(W[i]) > 127);
        WriteStr('''');
        while J < I do
        begin
          WriteStr(Char(W[J]));
          Inc(J);
        end;
        WriteStr('''');
      end else
      begin
        WriteStr('#');
        WriteStr(IntToStr(Ord(W[I])));
        Inc(I);
      end;
    until I > L;
  end;
end;

{ TPacalParserGenerator }

function TPacalParserGenerator.GenCond(s: Tset; p: TNode): String;
var I: Integer; str: String;
begin
  if s.IsOne(I) then
    Result := Format('(CurrentInputSymbol=%s)',[GenTokenName(I)])
  else begin
    I := s.Count;
    if I=0 then
      Result := 'False'
    else if I<=maxInlineSet then
      Result := Format('(CurrentInputSymbol in [%s])',[GenTerminals(s)])
    else Result := Format('InSet(CurrentInputSymbol,%d)',[NewCondSet(s)]);
  end;
  if p.typ=ntIf then
  begin
    str := GenParams(@p.pos);
    if str<>'' then Result := Format('%s and %s',[Result,str]);
  end;
end;

procedure _AddChar(data: Pointer; Index: Integer);
begin
  PString(data)^ := PString(data)^+Char(Index);
end;

procedure TPacalParserGenerator.PrintErrors;
var I: Integer;
begin
  if fSynErrors<>nil then
  for I := 0 to fSynErrors.Count - 1 do
  WriteLn(Coco.Output,#9,I+1,' : Result := ',QuotedStr(fSynErrors[I]),';');
end;

procedure TPacalParserGenerator.PrintInitLiterals;
var I,C1,C2: Integer; str,tok,sign,delim,l1,l2: String;
begin
  C1 := 0; C2 := 0;
  for I := 0 to Coco.tab.TerminalCount - 1 do
  with Coco.tab.Terminals[I] do
  if kind=tkLit then
  begin
     str := TokenString;
     str := GenStr(AnsiDequotedStr(str,str[1]));
     Inc(C1,Length(str));
     tok := GenTokenName(index);
     Inc(C2,Length(tok));
     if homograph then
       sign := '-'
     else sign := '';
     l1 := Concat(l1,delim,str);
     if C1>100 then
     begin
       C1 := 0; l1 := l1 +#13#10#9#9;
     end;
     l2 := Concat(l2,delim,sign,tok);
     if C2>100 then
     begin
       C2 := 0; l2 := l2 +#13#10#9#9;
     end;
     delim := ','
  end;
  Write(Coco.Output,#9'[',l1,'],'#13#10#9'[',l2,']');
end;

procedure TPacalParserGenerator.PrintInitStartStates;
var I: Integer;
    strLine: String;
    a: TAction;
    r: TRange;
  procedure _Write(const str: String);
  begin
    strLine := strLine + str;
    if Length(strLine)<80 then Exit;
    WriteLn(Coco.Output,#9+strLine); strLine := '';
  end;
begin
  a := Coco.DFA.States[0].firstAction;
  while a<>nil do
  begin
    if a.typ=ntChr then
       _Write(Format( '  States[%d] := %d;',[a.sym,a.Targets[0].index]))
    else
    with TCharSetAccess(Coco.tab.CharClassSet[a.sym]) do
    begin
      for I := 0 to fList.Count - 1 do
      with r do
      begin
        ptr := fList[I];
        if from_=to_ then
          _Write(Format( '  States[%d] := %d;',[from_,a.Targets[0].index]))
        else
          _Write(Format( '  FillRange(%d, %d, %d);',[from_,to_,a.Targets[0].index]));
      end;
    end;
    a := a.next;
  end;
  if strLine<>'' then Write(Coco.Output,#9+strLine);
end;

procedure TPacalParserGenerator.PrintParserDeclarations;
var I: Integer;
begin
  with Coco.tab do
  for I := 0 to NonTerminalCount - 1 do
    WriteLn(Coco.Output,'    procedure _',NonTerminals[I].Name,GenParams(NonTerminals[I]),';');
end;

procedure TPacalParserGenerator.PrintParserImplementation;
var I: Integer;
  checked: TSet;
begin
  for I := 0 to Coco.tab.NonTerminalCount - 1 do
  with Coco.tab,NonTerminals[I] do
  begin
    WriteLn(Coco.Output,'procedure T',gramSy.Name,'._',Name,GenParams(NonTerminals[I]),';');
    if hasSemPos then
      PrintFragment(semPos,True);
    WriteLn(Coco.Output,'begin');
    checked := TSet.Create(TerminalCount);
    try
      PrintProductionCode('  ',NonTerminals[I],graph,checked);
    finally
      checked.Free;
    end;
    WriteLn(Coco.Output,'end;');
    PrintLn;
  end;
end;

procedure TPacalParserGenerator.PrintComments;
var I: Integer;
begin
  for I := 0 to Coco.DFA.CommentCount - 1 do
  with Coco.DFA.Comments[I] do
  begin
    Write(Coco.Output,'    ',GenTokenName(start.index),': ');
    if nested then
      WriteLn(Coco.Output,'SkipNestedComment(',GenStr(start.Name),',',GenStr(stop),');')
    else WriteLn(Coco.Output,'SkipCommentTo(',GenStr(stop),');');
  end;
end;

procedure TPacalParserGenerator.PrintPragmas;
var sym: TtSymbol;
  procedure PrintPragma(const prefix: String);
  begin
    with sym do
    begin
      WriteLn(Coco.Output,prefix+'begin');
         PrintFragment(semPos,True);
      WriteLn(Coco.Output,prefix+'end;');
    end;
  end;
var I,C: Integer;
begin
 with Coco.tab do
 begin
  C := PragmaCount;
  if C=1 then
  begin
    sym := Pragmas[0];
    if sym.hasSemPos then
    begin
      WriteLn(Coco.Output,'  if CurrentInputSymbol=',GenTokenName(sym.index),' then');
      PrintPragma('  ');
    end;
  end else if C>1 then
  begin
    WriteLn(Coco.Output,'  case CurrentInputSymbol of');
    for I := 0 to PragmaCount - 1 do
    begin
      sym := Pragmas[I];
      if sym.hasSemPos then
      begin
        WriteLn(Coco.Output,'    ',GenTokenName(sym.index),':');
        PrintPragma(#9);
      end;
    end;
    WriteLn(Coco.Output,'  end;');
  end;
 end;
end;

procedure TPacalParserGenerator.PrintProductionCode(const lineprefix: String;
   curSy: TNtSymbol; gn: TNode; checked: TSet);
var s1,s2: TSet; p: TNode; str: String; eq: Boolean;
begin
 s1 := TSet.Create(Coco.tab.TerminalCount);
 s2 := TSet.Create(Coco.tab.TerminalCount);
 try
  while gn<>nil do
  begin
    case gn.typ of
      ntNonTerminal:
          WriteLn(Coco.Output,lineprefix,'_',gn.sym.Name,GenParams(@gn.pos),';');
      ntTerminal:
          if checked[gn.sym.index] then
            WriteLn(Coco.Output,lineprefix+'Get;')
          else WriteLn(Coco.Output,lineprefix,'Expect(',GenTokenName(gn.sym.index),');');
      ntWeakTerminal:
          begin
            Coco.tab.CompExpected(gn.next,curSy,s1);
            s1.Unite(Coco.tab.AllSyncSets);
            WriteLn(Coco.Output,lineprefix,'ExpectWeak(',GenTokenName(gn.sym.index),',',NewCondSet(s1),');');
          end;
      ntSync:
          begin
            WriteLn(Coco.Output,lineprefix,'while not ',GenCond(gn.aset,gn),' do');
            WriteLn(Coco.Output,lineprefix,'begin SynError(',
                NewSynError(Format('this symbol not expected in %s',[curSy.Name])),
                '); Get; end;');
          end;
      ntAlt:
          begin
            Coco.tab.CompFirstSet(gn,s1);
            eq := s1.Equals(checked);
            p := gn;
            str := '';
            while p<>nil do
            begin
              Coco.tab.CompExpected(p.sub,curSy,s1);
              WriteLn(Coco.Output,lineprefix,str,'if ',GenCond(s1,p.sub),' then');
              str := 'else ';
              WriteLn(Coco.Output,lineprefix+'begin');
              s1.Unite(checked);
              PrintProductionCode(lineprefix+'     ',curSy,p.sub,s1);
              WriteLn(Coco.Output,lineprefix+'end');
              p := p.down;
            end;
            if not eq then
               WriteLn(Coco.Output,lineprefix,'else SynError(',
                  NewSynError(Format('invalid %s',[curSy.Name])),
                  ');')
            else WriteLn(Coco.Output,lineprefix+';');
          end;
      ntIter:
          begin
            p := gn.sub;
            if p.typ=ntWeakTerminal then
            begin
              Coco.tab.CompExpected(p.next,curSy,s1);
              Coco.tab.CompExpected(gn.next,curSy,s2);
              str := Format('WeakSeparator(%d,%d,%d)',[p.sym.index,NewCondSet(s1),NewCondSet(s2)]);
              s1.Clear;
              if p.up then p :=nil else p := p.next;
            end  else begin
              Coco.tab.CompFirstSet(p,s1);
              str := GenCond(s1,p);
            end;
            WriteLn(Coco.Output,lineprefix,'while ',str,' do');
            WriteLn(Coco.Output,lineprefix+'begin');
            PrintProductionCode(lineprefix+'  ',curSy,p,s1);
            WriteLn(Coco.Output,lineprefix+'end;');
          end;
      ntOpt:
          begin
            Coco.tab.CompFirstSet(gn.sub,s1);
            WriteLn(Coco.Output,lineprefix,'if ',GenCond(s1,gn.sub),' then');
            WriteLn(Coco.Output,lineprefix+'begin');
            PrintProductionCode(lineprefix+'  ',curSy,gn.sub,s1);
            WriteLn(Coco.Output,lineprefix+'end;');
          end;
      ntAny:
          WriteLn(Coco.Output,lineprefix+'Get;');
      ntSem:
        PrintFragment(@gn.pos,True);
    end;
    if not (gn.typ in [ntEps,ntSem,ntSync]) then
      checked.Clear;
    if gn.up then Break;
    gn := gn.next;
  end;
 finally
   s1.Free; s2.Free;
 end;
end;

function TPacalParserGenerator.GenCharSelector(s: TCharSet): String;
  function IsLikeANY: Boolean;
  var f,l: Integer;
    r: TRange;
  begin
    with TCharSetAccess(s) do
    begin
      r.ptr := fList[0]; f := r.from_;
      r.ptr := fList.Last; l := r.to_;
    end;
    Result := (f=0)and(l=Ord(High(WideChar)));
  end;
  function _GenCharSelector(s: TCharSet): String;
  var I: Integer;
    r: TRange;
    delim: String;
  begin
    Result := '';
    with TCharSetAccess(s) do
    for I := 0 to fList.Count - 1 do
    with r do
    begin
      ptr := fList[I];
      if from_=to_ then
        Result := Format('%s%s(CurrInputCh=%s)',[Result,delim,CharToStr(from_)])
      else
        if from_=0 then
          Result := Format('%s%s(CurrInputCh<=%s)',[Result,delim,CharToStr(to_)])
        else if to_=Ord(High(WideChar)) then
          Result := Format('%s%s(CurrInputCh>=%s)',[Result,delim,CharToStr(from_)])
        else if to_=from_+1 then
          Result := Format('%s%s(CurrInputCh=%s)or(CurrInputCh=%s)',
            [Result,delim,CharToStr(from_),CharToStr(to_)])
        else
          Result := Format('%s%s((CurrInputCh>=%s)and(CurrInputCh<=%s))',
            [Result,delim,CharToStr(from_),CharToStr(to_)]);
      delim := 'or';
    end;
  end;

var contrary: TCharSet;
begin
  if IsLikeANY then
  begin
    contrary := TCharSet.Create;
    try
      contrary.Fill;
      contrary.Subtract(s);
      Result := Format('not(%s)',[_GenCharSelector(contrary)]);
    finally
      contrary.Free;
    end;
  end else
  Result := _GenCharSelector(s);
end;

procedure TPacalParserGenerator.PrintIgnoreSet;
begin
  with Coco.tab do
  if IgnoredChars<>nil then
    Write(Coco.Output,GenCharSelector(IgnoredChars));
end;

procedure TPacalParserGenerator.PrintState(state: TState);
var endOf: TSymbol;
    ctxEnd: Boolean;
    a: TAction;
    first,str,delim: String;
    c: Integer;
begin
  endOf := state.endOf;
  ctxEnd := state.ctx;
  WriteLn(Coco.Output,#9,state.index:2,':');
  a := state.firstAction;
  while a<>nil do
  begin
    if a.typ=ntChr then
       str := Format('(CurrInputCh = %s)',[CharToStr(a.sym)])
    else str := GenCharSelector(Coco.tab.CharClassSet[a.sym]);

    WriteLn(Coco.Output,#9#9,first,Format('if %s then',[str]));
    first := 'else ';
    c := 0;
    if a.Targets[0]<>state then
    begin
      Inc(c); str := Format('  state := %d',[a.Targets[0].index]);
      delim := ';'
    end  else str := '';
    if a.tc=contextTrans then
    begin
      Inc(c); str := Concat(str,delim,' BeginContext(apx)'); ctxEnd := False;
    end else if state.ctx then
    begin
      Inc(c); str := Concat(str,delim,' apx.Beg := -1');
    end;
    case c of
      1: WriteLn(Coco.Output,#9#9,str);
      2: WriteLn(Coco.Output,#9#9,Format('begin %s; end',[str]));
    end;
    a := a.next;
  end;

  if state.firstAction<>nil then
    WriteLn(Coco.Output,#9#9'else begin')
  else WriteLn(Coco.Output,#9#9'begin');

  if ctxEnd then
      WriteLn(Coco.Output,#9#9'  EndContext(apx);');

  if endOf=nil then
    WriteLn(Coco.Output,#9#9'  sym := ',GenTokenName(Coco.tab.noSym.index),';')
  else begin
    WriteLn(Coco.Output,#9#9'  sym := ',GenTokenName(endOf.index),';');
    if endOf.kind=tkClassLit then
      WriteLn(Coco.Output,#9#9'  CheckLiteral(sym);');
  end;
  WriteLn(Coco.Output,#9#9'  Exit;');

  WriteLn(Coco.Output,#9#9'end;');
end;

procedure TPacalParserGenerator.PrintTokens;
var I,J :Integer;
begin
  fSymbolNamesHadGenerated := True;
  with Coco.tab do
  begin
    J := 0;
    for I := 1 to TerminalCount-1 do
    begin
      Write(Coco.Output,#9,Terminals[I].symID,' = ',I,';');
      if J=4 then PrintLn;
      Inc(J); if J>4 then J := 0;
    end;
    for I := 0 to PragmaCount-1 do
    begin
      Write(Coco.Output,#9,Pragmas[I].symID,' = ',I+TerminalCount,';');
      if J=4 then PrintLn;
      Inc(J); if J>4 then J := 0;
    end;
  end;
end;

procedure TPacalParserGenerator.PrintTokenStrings;
var I,J :Integer; str: String;
begin
  WriteLn(Coco.Output,'''EOF''');
  J := 0;
  with Coco.tab do
  for I := 1 to TerminalCount-2 do
  begin
    str := Terminals[I].TokenString;
    Write(Coco.Output,#9',''',str,'''');
    if J=4 then PrintLn;
    Inc(J); if J>4 then J := 0;
  end;
  Write(Coco.Output,'  ,''not''');
end;

initialization
  RegisterParserGenerator('Delphi',TPacalParserGenerator);
end.
