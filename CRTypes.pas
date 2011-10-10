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

unit CRTypes;

interface
uses CocoSets, CocoAncestor;

const
  normalTrans  = 0;
  contextTrans = 1;

type
  TSymbol = class;

  TNodeType = (ntUnknown,
     ntCharClass, ntChr,
     ntTerminal, ntWeakTerminal, ntNonTerminal,
     ntAlt, ntIter, ntOpt, ntEps,
     ntAny, ntSync, ntIf, ntSem
     );

  TSymbolType = (stUnknown, stTerminal, stNonterminal, stPragma);

  TTokenKind = (
    tkFixed,       // e.g. 'a' ('b' | 'c') (structure of literals)
    tkClass,       // e.g. digit {digit}   (at least one char class)
    tkLit,         // e.g. "while"
    tkClassLit     // e.g. letter {letter} but without literals that have the same structure
    );

  TNode = class
  private
    fTyp: TNodeType;
    fSet: TCocoSet;
    function getNum: Integer;
    procedure setSet(const Value: TCocoSet);
  public
    index: Integer;
    line: Integer;
    next,           // to successor node
    down,           // ntAlt: to next alternative
    sub: TNode;     // ntAlt, ntIter, ntOpt: to first node of substructure
    up: Boolean;    // true: "next" leads to successor in enclosing structure
    sym: TSymbol;   // ntTerminal, ntWeakTerminal, ntNonTerminal: symbol represented by this node
    val,            // ntChr: Ord(Char), ntCharClass: index of char class
    code: Integer;  // ntChr, ntCharClass: transition code
    pos: TSymbolRec; // source
    state: TObject; // DFA state corresponding to this node
    NoWarning: Boolean;
    constructor Create(aType: TNodeType; aSym: TSymbol; aLine: Integer);
    destructor Destroy; override;
    procedure FinishGraph;
    function IsDeletableNode: Boolean;
    function IsDeletableGraph: Boolean;
    function IsDeletableSubGraph: Boolean;

    property typ: TNodeType read fTyp;
    property Num: Integer read getNum;
    property aset: TCocoSet read fSet write setSet; //ntAny, ntSync: the set represented by this node
  end;

  TSymbol = class
  private
    fTyp: TSymbolType;
    fName: String;
    fPositions: array[0..1] of CocoAncestor.PSymbol;
    function getHasPos(const Index: Integer): Boolean;
    function getPos(const Index: Integer): PSymbol;
    function getGraph: TNode;
  public
    index: Integer;
    line: Integer;
    kind: TTokenKind;

    constructor Create(aType: TSymbolType; const aName: String; aLine: Integer);
    destructor Destroy; override;

    property typ: TSymbolType read fTyp;
    property Name: String read fName;
    property attrPos: PSymbol index 0 read getPos; //stNonterminal: position of attributes in source text (or null)
    property semPos: PSymbol index 1 read getPos;  // stPragma: pos of semantic action in source text (or null)
                                                           // stNonterminal: pos of local declarations in source text (or null)
    property hasAttrPos: Boolean index 0 read getHasPos;
    property hasSemPos: Boolean  index 1 read getHasPos;
    property graph: TNode read getGraph; // only for nonterminals
  end;

  TNtSymbol = class(TSymbol)
  public
    fGraph: TNode;       //to first node of syntax graph
    first: TCocoSet;         // terminal start symbols
    follow: TCocoSet;        // terminal followers
    nts: TCocoSet;           // nonterminals whose followers have to be added to this sym
    deletable: Boolean;  // true if nonterminal is deletable
    destructor Destroy; override;
  end;

  TtSymbol = class(TSymbol)
  public
    literal, symID: String;
    homograph: Boolean;  // for homograph terminals

    constructor Create(aType: TSymbolType; const aName: String; aLine: Integer);
    function TokenString: String;
  end;


function NodeTypeForSymType(typ: TSymbolType): TNodeType;


implementation
uses SysUtils;

{ TSymbol }

constructor TSymbol.Create(aType: TSymbolType; const aName: String;
  aLine: Integer);
begin
  fTyp := aType;
  fName := aName;
  line := aLine;
end;

destructor TSymbol.Destroy;
begin
  if fPositions[0]<>nil then Dispose(fPositions[0]);
  if fPositions[1]<>nil then Dispose(fPositions[1]);
  inherited;
end;

function TSymbol.getGraph: TNode;
begin
  if Self is TNtSymbol then
    Result :=  TNtSymbol(Self).fGraph
  else Result := nil;
end;

function TSymbol.getHasPos(const Index: Integer): Boolean;
begin
  Result := (fPositions[Index]<>nil)and(fPositions[Index]^.Beg>=0);
end;

function TSymbol.getPos(const Index: Integer): PSymbol;
begin
  if fPositions[Index]=nil then
  begin
    New(fPositions[Index]);
    fPositions[Index]^.Beg := -1;
  end;
  Result := fPositions[Index];  
end;

{ TNtSymbol }

destructor TNtSymbol.Destroy;
begin
  first.Free;
  follow.Free;
  nts.Free;
  inherited;
end;

{ TNode }

constructor TNode.Create(aType: TNodeType; aSym: TSymbol; aLine: Integer);
begin
  fTyp := aType;
  sym := aSym;
  line := aLine;
  pos.Beg := -1;
end;

function TNode.getNum: Integer;
begin
  if Self=nil then Result := 0
  else Result := index;
end;

function TNode.IsDeletableNode: Boolean;
begin
  case typ of
    ntNonTerminal:
       Result := TNtSymbol(sym).deletable;
    ntAlt:
       Result := sub.IsDeletableSubGraph or
         ((down<>nil)and down.IsDeletableSubGraph);
    ntIter, ntOpt, ntEps, ntSem, ntIf, ntSync:
      Result := True;
    else Result := False;
  end;
end;

function TNode.IsDeletableGraph: Boolean;
begin
  Result := (Self=nil) or (IsDeletableNode and next.IsDeletableGraph);
end;

function TNode.IsDeletableSubGraph: Boolean;
begin
  Result := (Self=nil)or (IsDeletableNode and (up or next.IsDeletableSubGraph));
end;

procedure TNode.setSet(const Value: TCocoSet);
begin
  if fSet=nil then
    fSet := Value.Clone
  else fSet.Assign(Value);
end;

destructor TNode.Destroy;
begin
  fSet.Free;
  inherited;
end;

procedure TNode.FinishGraph;
var I,J: TNode;
begin
  I := Self;
  while I <> nil do
  begin
    J := I.Next;
    I.Next := nil;
    I := J;
  end;
end;

function NodeTypeForSymType(typ: TSymbolType): TNodeType;
begin
  case typ of
    stTerminal:
      Result := ntTerminal;
    stNonterminal:
      Result := ntNonTerminal;
    else
      Result := ntUnknown;
  end;
end;

{ TtSymbol }

constructor TtSymbol.Create(aType: TSymbolType; const aName: String;
  aLine: Integer);

  function ASCIIName(const ASCII: Char): string;
  begin
   case ASCII of
    #00 : Result := '_nul';
    #01 : Result := '_soh';
    #02 : Result := '_stx';
    #03 : Result := '_etx';
    #04 : Result := '_eot';
    #05 : Result := '_enq';
    #06 : Result := '_ack';
    #07 : Result := '_bel';
    #08 : Result := '_bs';
    #09 : Result := '_ht';
    #10 : Result := '_lf';
    #11 : Result := '_vt';
    #12 : Result := '_ff';
    #13 : Result := '_cr';
    #14 : Result := '_so';
    #15 : Result := '_si';
    #16 : Result := '_dle';
    #17 : Result := '_dc1';
    #18 : Result := '_dc2';
    #19 : Result := '_dc3';
    #20 : Result := '_dc4';
    #21 : Result := '_nak';
    #22 : Result := '_syn';
    #23 : Result := '_etb';
    #24 : Result := '_can';
    #25 : Result := '_em';
    #26 : Result := '_sub';
    #27 : Result := '_esc';
    #28 : Result := '_fs';
    #29 : Result := '_gs';
    #30 : Result := '_rs';
    #31 : Result := '_us';
    ' ' : Result := '_sp';
    '!' : Result := '_bang';
    '"' : Result := '_dquote';
    '#' : Result := '_hash';
    '$' : Result := '_dollar';
    '%' : Result := '_percent';
    '&' : Result := '_and';
    #39 : Result := '_squote';
    '(' : Result := '_lparen';
    ')' : Result := '_rparen';
    '*' : Result := '_star';
    '+' : Result := '_plus';
    ',' : Result := '_comma';
    '-' : Result := '_minus';
    '.' : Result := '_point';
    '/' : Result := '_slash';
    '0' : Result := '_zero';
    '1' : Result := '_one';
    '2' : Result := '_two';
    '3' : Result := '_three';
    '4' : Result := '_four';
    '5' : Result := '_five';
    '6' : Result := '_six';
    '7' : Result := '_seven';
    '8' : Result := '_eight';
    '9' : Result := '_nine';
    ':' : Result := '_colon';
    ';' : Result := '_semicolon';
    '<' : Result := '_less';
    '=' : Result := '_equal';
    '>' : Result := '_greater';
    '?' : Result := '_query';
    '@' : Result := '_at';
    'A'..'Z', 'a'..'z' : Result := '_' + ASCII + '_';
    '[' : Result := '_lbrack';
    '\' : Result := '_backslash';
    ']' : Result := '_rbrack';
    '^' : Result := '_uparrow';
    '_' : Result := '_underscore';
    '`' : Result := '_accent';
    '{' : Result := '_lbrace';
    '|' : Result := '_bar';
    '}' : Result := '_rbrace';
    '~' : Result := '_tilde';
    #127 : Result := '_delete';
    else Result := Format('U_%4x',[Ord(ASCII)]);
   end;
  end;

var id: String;
  I: Integer; ascName: String;
begin
  inherited;
  if (aName[1] = '''') or (aName[1] = '"') then
  begin
    if Length(aName) = 3 then
      id := ASCIIName(aName[2])
    else begin
      id := AnsiDequotedStr(aName,aName[1]);
      I := 1;
      while I<=Length(id) do
      case id[I] of
        'A'..'Z', 'a'..'z' : Inc(I);
        else begin
          ascName := ASCIIName(id[I]);
          id := StringReplace(id, id[I], ascName,[]);
          Inc(I,Length(ascName));
        end;
      end;
      if Length(id)>255 then
        id := Copy(id,1,255);
    end;
  end else id := aName;
  symID := id+'Sym';
end;

function TtSymbol.TokenString: String;
begin
  if literal='' then
    Result := Name
  else Result := literal;
end;

end.
