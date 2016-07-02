unit SynHighlighterVerilog;
{$mode objfpc}{$H+}
{**
 *  Syntax Highlighter for Verilog
 *
 * @url       http://www.sourceforge.net/projects/minilib
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{*
Verilog is case sensitive

identifiers: start with a letter or underscore "_"
             contain letters, numbers, "_" and dollar sign "$"
             may contain any non-whitespace escaped character \

comments: //  (slash slash) makes the remainder of the line a comment


*}
{*
todo to ask
example of escape identifire using \

*}
interface

uses
  SysUtils, Classes, Graphics,
  SynUtils, //exists in minilib at sourceforge
  SynEditTypes, SynEditHighlighter, SynHighlighterHashEntries;

type
  TtkTokenKind = (tkNull, tkComment, tkDirective, tkKeyword, tkIdentifier, tkNumber, tkSpace, tkString, tkSymbol, tkUnknown);

  TRangeState = (rsUnknown, rsComment);

  { TSynVerilogSyn }

  TSynVerilogSyn = class(TSynCustomHighlighter)
  private
    Run: LongInt;
    FProcTable: array[#0..#255] of TProcTableProc;
    FRange: TRangeState;
    FLine: PChar;
    FTokenPos: Integer;
    FTokenID: TtkTokenKind;
    FCommentAttri: TSynHighlighterAttributes;
    FKeywordAttri: TSynHighlighterAttributes;
    FDirectiveAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    procedure ScanTo(EndString: String; AKind: TtkTokenKind);
    procedure DQStringProc;
    procedure SingleCommentProc;
    procedure NormalCommentProc;
    procedure DirectiveProc;
    procedure CRProc;
    procedure EqualProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure SlashProc;
    procedure BlockOpenProc;
    procedure SpaceProc;
    procedure SymbolProc;
    procedure MakeProcTables;
    function IsIdentifiers(C: Char): Boolean;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetToken: string; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: Integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    function IsKeyword(const AKeyword: string): boolean; override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetLine(const NewValue: string; LineNumber: Integer); override;
    procedure SetRange(Value: Pointer); override;
  published
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri write FCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property KeywordAttri: TSynHighlighterAttributes read FKeywordAttri write FKeywordAttri;
    property DirectiveAttri: TSynHighlighterAttributes read FDirectiveAttri write FDirectiveAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri write FNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri write FStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri write FSymbolAttri;
  end;

const

  // Keyword
  sVerilogKeywords =
    'always,'+
    'and,'+
    'assign,'+
    'automatic,'+
    'begin,'+
    'buf,'+
    'bufif0,'+
    'bufif1,'+
    'case,'+
    'casex,'+
    'casez,'+
    'cell,'+
    'cmos,'+
    'config,'+
    'deassign,'+
    'default,'+
    'defparam,'+
    'design,'+
    'disable,'+
    'edge,'+
    'else,'+
    'end,'+
    'endcase,'+
    'endconfig,'+
    'endfunction,'+
    'endgenerate,'+
    'endmodule,'+
    'endprimitive,'+
    'endspecify,'+
    'endtable,'+
    'endtask,'+
    'event,'+
    'for,'+
    'force,'+
    'forever,'+
    'fork,'+
    'function,'+
    'generate,'+
    'genvar,'+
    'highz0,'+
    'highz1,'+
    'if,'+
    'ifnone,'+
    'incdir,'+
    'include,'+
    'initial,'+
    'inout,'+
    'input,'+
    'instance,'+
    'integer,'+
    'join,'+
    'large,'+
    'liblist,'+
    'library,'+
    'localparam,'+
    'macromodule,'+
    'medium,'+
    'module,'+
    'nand,'+
    'negedge,'+
    'nmos,'+
    'nor,'+
    'noshowcancelledn,'+
    'not,'+
    'notif0,'+
    'notif1,'+
    'or,'+
    'output,'+
    'parameter,'+
    'pmos,'+
    'posedge,'+
    'primitive,'+
    'pull0,'+
    'pull1,'+
    'pulldown,'+
    'pullup,'+
    'pulsestyle_oneventglitch,'+
    'pulsestyle_ondetectglitch,'+
    'remos,'+
    'real,'+
    'realtime,'+
    'reg,'+
    'release,'+
    'repeat,'+
    'rnmos,'+
    'rpmos,'+
    'rtran,'+
    'rtranif0,'+
    'rtranif1,'+
    'scalared,'+
    'showcancelled,'+
    'signed,'+
    'small,'+
    'specify,'+
    'specparam,'+
    'strong0,'+
    'strong1,'+
    'supply0,'+
    'supply1,'+
    'table,'+
    'task,'+
    'time,'+
    'tran,'+
    'tranif0,'+
    'tranif1,'+
    'tri,'+
    'tri0,'+
    'tri1,'+
    'triand,'+
    'trior,'+
    'trireg,'+
    'unsigned,'+
    'use,'+
    'vectored,'+
    'wait,'+
    'wand,'+
    'weak0,'+
    'weak1,'+
    'while,'+
    'wire,'+
    'wor,'+
    'xnor,'+
    'xor';

  sVerilogDirectives =
    'include'+
    'define'+
    'define'+
    'undef'+
    'ifdef'+
    'elsif'+
    'else'+
    'endif'+
    'ifndef macro_name'+
    'timescale 1ns/1ns'+
    'celldefine'+
    'endcelldefine'+
    'default_nettype net_type'+
    'resetall'+
    'line number "filename" level'+
    'unconnected_drive pull0'+
    'unconnected_drive pull1'+
    'nounconnected_drive'+
    'default_decay_time a_time'+
    'default_trireg_strength val'+
    'delay_mode_distributed'+
    'delay_mode_path'+
    'delay_mode_unit'+
    'delay_mode_zero';


  // types
  //VerilogTypes = 'integer,float,string,boolean,color,datetime';

type

  { TVerilogSyn }

  TVerilogSyn = class(TSynPersistent)
   private
   protected
     function GetDefaultKind: Integer; override;
   public
     function IdentKind(MayBe: PChar; out L: Integer): TtkTokenKind; overload;
     function IdentKind(MayBe: PChar): TtkTokenKind; overload;
     constructor Create; override;
   end;

function VerilogSyn: TVerilogSyn;

implementation

uses
  SynEditStrConst;

var
  FVerilogSyn: TVerilogSyn = nil;

function VerilogSyn: TVerilogSyn;
begin
  if FVerilogSyn = nil then
    FVerilogSyn := TVerilogSyn.Create;
  Result := FVerilogSyn;
end;

{ TVerilogSyn }

function TVerilogSyn.GetDefaultKind: Integer;
begin
  Result := ord(tkIdentifier);
end;

function TVerilogSyn.IdentKind(MayBe: PChar; out L: Integer): TtkTokenKind;
begin
  Result := TtkTokenKind(GetIdentKind(MayBe, L));
end;

function TVerilogSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  L: Integer;
begin
  Result := TtkTokenKind(GetIdentKind(MayBe, L));
end;

constructor TVerilogSyn.Create;
begin
  inherited;
  EnumerateKeywords(Ord(tkKeyword), sVerilogKeywords, GetIdentChars, @DoAddKeyword);
end;

procedure TSynVerilogSyn.MakeProcTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      #0: FProcTable[I] := @NullProc;
      #10: FProcTable[I] := @LFProc;
      #13: FProcTable[I] := @CRProc;

      '=': FProcTable[I] := @EqualProc;
      '>': FProcTable[I] := @GreaterProc;
      '<': FProcTable[I] := @LowerProc;
      '/': FProcTable[I] := @SlashProc;
      '{': FProcTable[I] := @BlockOpenProc;
      '|': FProcTable[I] := @OrSymbolProc;
      '"': FProcTable[I] := @DQStringProc;
      //'`': FProcTable[I] := @DirectiveProc;

      '0'..'9':
        FProcTable[I] := @NumberProc;
      #1..#9, #11, #12, #14..#32:
        FProcTable[I] := @SpaceProc;
      '+', '-', '^', '%', '*', '!', '}', ':', '.', ',', ';', '?', '(', ')', '[', ']', '~', '&':
        FProcTable[I] := @SymbolProc;
    else
      FProcTable[I] := @IdentProc;
    end;
end;

constructor TSynVerilogSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  FCommentAttri.Foreground := clMaroon;
  FCommentAttri.Style := [];
  AddAttribute(FCommentAttri);
  FDirectiveAttri := TSynHighlighterAttributes.Create('Directive');
  FDirectiveAttri.Style := [fsBold];
  FDirectiveAttri.Foreground := $00C56A31;
  AddAttribute(FDirectiveAttri);
  FKeywordAttri := TSynHighlighterAttributes.Create('Keyword');
  FKeywordAttri.Style := [fsBold];
  FKeywordAttri.Foreground := $00C56A31;
  AddAttribute(FKeywordAttri);
  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  FIdentifierAttri.Foreground := clBlack;
  AddAttribute(FIdentifierAttri);
  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  AddAttribute(FNumberAttri);
  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(FSpaceAttri);
  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
  AddAttribute(FStringAttri);
  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(FSymbolAttri);
  SetAttributesOnChange(@DefHighlightChange);
  FDefaultFilter := 'Verilog Files (*.v)|*.Verilog';
  FRange := rsUnknown;
  MakeProcTables;
end;

destructor TSynVerilogSyn.Destroy;
begin
  inherited;
end;

procedure TSynVerilogSyn.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

procedure TSynVerilogSyn.SetLine(const NewValue: string; LineNumber: Integer);
begin
  inherited;
  FLine := PChar(NewValue);
  Run := 0;
  Next;
end;

procedure TSynVerilogSyn.DQStringProc;
begin
  Inc(Run);
  ScanTo('"', tkString);
end;

procedure TSynVerilogSyn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynVerilogSyn.EqualProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] in ['=', '>'] then
    Inc(Run);
end;

procedure TSynVerilogSyn.GreaterProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] in ['=', '>'] then
    Inc(Run);
end;

procedure TSynVerilogSyn.IdentProc;
begin
  FTokenID := VerilogSyn.IdentKind((FLine + Run));
  inc(Run);
  while not (FLine[Run] in [#0, #10, #13]) and IsIdentifiers(FLine[Run]) do
    Inc(Run);
end;

procedure TSynVerilogSyn.LFProc;
begin
  FTokenID := tkSpace;
  inc(Run);
end;

procedure TSynVerilogSyn.LowerProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  case FLine[Run] of
    '=': Inc(Run);
    '<':
      begin
        Inc(Run);
        if FLine[Run] = '=' then
          Inc(Run);
      end;
  end;
end;

procedure TSynVerilogSyn.NullProc;
begin
  FTokenID := tkNull;
end;

procedure TSynVerilogSyn.NumberProc;
begin
  inc(Run);
  FTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', '.', '''',  'a'..'z', 'A'..'Z'] do
    inc(Run);
end;

procedure TSynVerilogSyn.OrSymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] in ['=', '|'] then
    Inc(Run);
end;

procedure TSynVerilogSyn.SlashProc;
begin
  Inc(Run);
  case FLine[Run] of
    '/':
      begin
        SingleCommentProc;
      end;
    '*':
      begin
        FRange := rsComment;
        FTokenID := tkComment;
        NormalCommentProc;
      end;
  else
    FTokenID := tkSymbol;
  end;
end;

procedure TSynVerilogSyn.BlockOpenProc;
begin
  Inc(Run);
  case FLine[Run] of
    '`':
      begin
        FTokenID := tkDirective;
        Inc(Run);
        DirectiveProc;
      end;
  else
    FTokenID := tkSymbol;
  end;
end;

procedure TSynVerilogSyn.SpaceProc;
begin
  FTokenID := tkSpace;
  repeat
    Inc(Run);
  until (FLine[Run] > #32) or (FLine[Run] in [#0, #10, #13]);
end;

procedure TSynVerilogSyn.SymbolProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

function TSynVerilogSyn.IsIdentifiers(C: Char): Boolean;
begin
  Result := not (C in [' ', #13, #10, #0, '{', '}', ':', ',', ';', '?', '(', ')', '[', ']',
          '~', '^', '%', '*', '!', '=', '>', '<', '-', '|', '+', '/', '&',  '''', '"']);
end;

procedure TSynVerilogSyn.ScanTo(EndString: String; AKind: TtkTokenKind);
var
  l: Integer;
begin
  l := Length(EndString);
  FTokenID := AKind;
  while not (FLine[Run] in [#0, #10, #13]) do
  begin
    if (FLine[Run] = EndString[1]) and ((l < 2) or (FLine[Run + 1] = EndString[2])) then
    begin
      FRange := rsUnknown;
      Inc(Run, l);
      break;
    end;
    Inc(Run);
  end;
end;

procedure TSynVerilogSyn.SingleCommentProc;
begin
  FTokenID := tkComment;
  repeat
    Inc(Run);
  until FLine[Run] in [#0, #10, #13];
end;

procedure TSynVerilogSyn.NormalCommentProc;
begin
  ScanTo('*/', tkComment);
end;

procedure TSynVerilogSyn.DirectiveProc;
begin
  FTokenID := tkDirective;
  inc(Run);
  while not (FLine[Run] in [#0, #10, #13]) and IsIdentifiers(FLine[Run]) do
    Inc(Run);
end;

function TSynVerilogSyn.IsKeyword(const AKeyword: string): boolean;
var
  tk: TtkTokenKind;
begin
  tk := VerilogSyn.IdentKind(PChar(AKeyword));
  Result := tk in [tkKeyword];
end;

procedure TSynVerilogSyn.Next;
begin
  FTokenPos := Run;
  if (FLine[Run] in [#0, #10, #13]) then
    FProcTable[FLine[Run]]
  else
  begin
    case FRange of
      rsComment:
          NormalCommentProc;
    else
      FProcTable[FLine[Run]];
    end;
  end;
end;

function TSynVerilogSyn.GetDefaultAttribute(Index: integer):
  TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_KEYWORD: Result := FKeywordAttri;
    SYN_ATTR_DIRECTIVE: Result := FDirectiveAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynVerilogSyn.GetEol: Boolean;
begin
  Result := FTokenID = tkNull;
end;

function TSynVerilogSyn.GetRange: Pointer;
begin
  Result := Pointer(PtrUInt(FRange));
end;

function TSynVerilogSyn.GetToken: string;
var
  Len: LongInt;
begin
  Len := Run - FTokenPos;
  Setstring(Result, (FLine + FTokenPos), Len);
end;

procedure TSynVerilogSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: Integer);
begin
  TokenLength := Run - FTokenPos;
  TokenStart := FLine + FTokenPos;
end;

function TSynVerilogSyn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynVerilogSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := FCommentAttri;
    tkKeyword: Result := FKeywordAttri;
    tkDirective: Result := FDirectiveAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynVerilogSyn.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

function TSynVerilogSyn.GetTokenPos: Integer;
begin
  Result := FTokenPos;
end;

procedure TSynVerilogSyn.ResetRange;
begin
  FRange := rsUnknown;
end;

procedure TSynVerilogSyn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(PtrUInt(Value));
end;

function TSynVerilogSyn.GetIdentChars: TSynIdentChars;
begin
  Result := VerilogSyn.GetIdentChars
end;

class function TSynVerilogSyn.GetLanguageName: string;
begin
  Result := 'Verilog';
end;

function TSynVerilogSyn.GetSampleSource: string;
begin
  Result := '';//TODO
end;

initialization
  RegisterPlaceableHighlighter(TSynVerilogSyn);
finalization
  FreeAndNil(FVerilogSyn);
end.

