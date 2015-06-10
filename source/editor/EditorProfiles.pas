unit EditorProfiles;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{/$I SynEdit.inc}

interface

uses
  Messages, Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls, Registry, ExtCtrls, Buttons, ImgList,
  Contnrs, Menus, SynEdit, SynEditHighlighter, SynEditMiscClasses, SynEditPointClasses, SynGutterCodeFolding,
  SynGutter, SynEditKeyCmds, Classes, SysUtils;

const
  cDefaultOptions = [eoAltSetsColumnMode, eoAutoIndent, eoDragDropEditing, eoDropFiles, eoScrollPastEol,
    eoShowScrollHint, eoRightMouseMovesCursor, eoTabsToSpaces, eoTabIndent, eoTrimTrailingSpaces, eoKeepCaretX];

type
  IGlobalAttributes = interface
  ['{D4F16257-1809-4BD7-81D2-2D9951A5057D}']
  end;

  { TGlobalAttribute }

  TGlobalAttribute = class(TPersistent)
  private
    FBackground: TColor;
    FForeground: TColor;
    FStyle: TFontStyles;
    FName: string;
    FTitle: string;
  public
    constructor Create;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Title: string read FTitle write FTitle;
    property Name: string read FName write FName;
    property Background: TColor read FBackground write FBackground default clNone;
    property Foreground: TColor read FForeground write FForeground default clNone;
    property Style: TFontStyles read FStyle write FStyle default [];
  end;

  { TGlobalAttributes }

  TGlobalAttributes = class(TComponent)
  private
    FDatatype: TGlobalAttribute;
    FDirective: TGlobalAttribute;
    FList: TObjectList;

    FDocument: TGlobalAttribute;
    FDQ_string: TGlobalAttribute;
    FIdentifier: TGlobalAttribute;
    FKeyword: TGlobalAttribute;
    FML_comment: TGlobalAttribute;
    FNumber: TGlobalAttribute;
    FSelected: TGlobalAttribute;
    FSL_comment: TGlobalAttribute;
    FSQ_string: TGlobalAttribute;
    FSymbol: TGlobalAttribute;
    FValue: TGlobalAttribute;
    FVariable: TGlobalAttribute;
    FWhitespace: TGlobalAttribute;
    function GetCount: Integer;
    function GetItem(Index: Integer): TGlobalAttribute;
    function GetAttribute(Index: string): TGlobalAttribute;
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Find(vName: string): TGlobalAttribute;
    property Items[Index: Integer]: TGlobalAttribute read GetItem; default;
    property Attribute[Index: string]: TGlobalAttribute read GetAttribute;
    property Count: Integer read GetCount;
  published
    property Whitespace: TGlobalAttribute read FWhitespace;
    property Selected: TGlobalAttribute read FSelected;

    property Keyword: TGlobalAttribute read FKeyword;
    property Symbol: TGlobalAttribute read FSymbol;
    property Number: TGlobalAttribute read FNumber;
    property Directive: TGlobalAttribute read FDirective;
    property Identifier: TGlobalAttribute read FIdentifier;
    property Variable: TGlobalAttribute read FVariable;
    property Value: TGlobalAttribute read FValue;
    property Datatype: TGlobalAttribute read FDatatype;
    property Document: TGlobalAttribute read FDocument;
    property SL_comment: TGlobalAttribute read FSL_comment;
    property ML_comment: TGlobalAttribute read FML_comment;
    property SQ_string: TGlobalAttribute read FSQ_string;
    property DQ_string: TGlobalAttribute read FDQ_string;

  end;

  { TGutterOptions }

  TGutterOptions = class(TPersistent)
  private
    FAutoSize: boolean;
    FBackcolor: TColor;
    FForecolor: TColor;
    FSavedColor: TColor;
    FSeparatorColor: TColor;
    FLeftOffset: integer;
    FRightOffset: integer;
    FShowLineNumbers: Boolean;
    FShowModifiedLines: Boolean;
    FUnsavedColor: TColor;
    FVisible: Boolean;
    FLeadingZeros: Boolean;
    FZeroStart: Boolean;
    FShowSeparator: Boolean;
    FWidth: Integer;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    constructor AssignFrom(SynGutter: TSynGutter);
    procedure Reset; virtual;
  published
    property AutoSize: boolean read FAutoSize write FAutoSize default True;
    property Backcolor: TColor read FBackcolor write FBackcolor default clBtnFace;
    property Forecolor: TColor read FForecolor write FForecolor default clBtnText;
    property SeparatorColor: TColor read FSeparatorColor write FSeparatorColor default clBtnText;
    property ShowSeparator: Boolean read FShowSeparator write FShowSeparator default True;

    property SavedColor: TColor read FSavedColor write FSavedColor default clGreen;
    property UnsavedColor: TColor read FUnsavedColor write FUnsavedColor default clYellow;
    property ShowModifiedLines: Boolean read FShowModifiedLines write FShowModifiedLines default True;

    property LeftOffset: integer read FLeftOffset write FLeftOffset default 0;
    property RightOffset: integer read FRightOffset write FRightOffset default 0;
    property Visible: boolean read FVisible write FVisible default True;
    property Width: integer read FWidth write FWidth default 30;
    property ShowLineNumbers: Boolean read FShowLineNumbers write FShowLineNumbers default True;
    property LeadingZeros: Boolean read FLeadingZeros write FLeadingZeros default False;
    property ZeroStart: Boolean read FZeroStart write FZeroStart default False;
  end;

  //This class is assignable to a SynEdit without modifying key properties that affect function

  { TEditorProfile }

  TEditorProfile = class(TComponent) //make it as object
  private
    FCodeFolding: Boolean;
    FExtOptions: TSynEditorOptions2;
    FMaxUndo: Integer;
    FExtraLineSpacing: Integer;
    FTabWidth: Integer;
    FRightEdge: Integer;
    FRightEdgeColor: TColor;
    FFontName: String;
    FFontSize: Integer;
    FFontNoAntialiasing: Boolean;
    FBookmarks: TSynBookMarkOpt;
    FOverwriteCaret: TSynEditCaretType;
    FInsertCaret: TSynEditCaretType;
    FOptions: TSynEditorOptions;
    FGutterOptions: TGutterOptions;
    FInsertMode: Boolean;
    FAttributes: TGlobalAttributes;
    procedure SetExtOptions(const AValue: TSynEditorOptions2);
    procedure SetOptions(const Value: TSynEditorOptions);
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetChildOwner: TComponent; override;
    function GetChildParent: TComponent; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Reset;
  published
    property Attributes: TGlobalAttributes read FAttributes;
    property Options: TSynEditorOptions read FOptions write SetOptions default cDefaultOptions;
    property ExtOptions: TSynEditorOptions2 read FExtOptions write SetExtOptions default [];
    property FontName: String read FFontName write FFontName;
    property FontSize: Integer read FFontSize write FFontSize;
    property FontNoAntialiasing: Boolean read FFontNoAntialiasing write FFontNoAntialiasing default False;
    property Gutter: TGutterOptions read FGutterOptions write FGutterOptions;
    property ExtraLineSpacing: Integer read FExtraLineSpacing write FExtraLineSpacing default 0;
    property RightEdge: Integer read FRightEdge write FRightEdge default 80;
    property RightEdgeColor: TColor read FRightEdgeColor write FRightEdgeColor default clSilver;
    property InsertMode: Boolean read FInsertMode write FInsertMode default True;
    property InsertCaret: TSynEditCaretType read FInsertCaret write FInsertCaret default ctVerticalLine;
    property OverwriteCaret: TSynEditCaretType read FOverwriteCaret write FOverwriteCaret default ctBlock;
    property MaxUndo: Integer read FMaxUndo write FMaxUndo default 1024;
    property TabWidth: Integer read FTabWidth write FTabWidth default 2;
    property CodeFolding: Boolean read FCodeFolding write FCodeFolding default False;
  end;

implementation

uses
  SynGutterBase, SynGutterLineNumber, SynGutterChanges;

{ TEditorProfile }

procedure TEditorProfile.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TCustomSynEdit) then
  begin
    //TODO assign attributes
    Self.FontName := TCustomSynEdit(Source).Font.Name;
    Self.FontSize := TCustomSynEdit(Source).Font.Size;
    Self.FontNoAntialiasing := TCustomSynEdit(Source).Font.Quality = fqNonAntialiased;

    Self.Gutter.Assign(TCustomSynEdit(Source).Gutter);

    Self.Options := TCustomSynEdit(Source).Options;
    Self.ExtraLineSpacing := TCustomSynEdit(Source).ExtraLineSpacing;
    Self.InsertCaret := TCustomSynEdit(Source).InsertCaret;
    Self.OverwriteCaret := TCustomSynEdit(Source).OverwriteCaret;
    Self.MaxUndo := TCustomSynEdit(Source).MaxUndo;
    Self.RightEdge := TCustomSynEdit(Source).RightEdge;
    Self.RightEdgeColor := TCustomSynEdit(Source).RightEdgeColor;
    Self.TabWidth := TCustomSynEdit(Source).TabWidth;
  end
  else
    inherited;
end;

procedure TEditorProfile.AssignTo(Dest: TPersistent);
var
  cf: TSynGutterCodeFolding;
  OldCF: Boolean;
begin
  if Assigned(Dest) and (Dest is TCustomSynEdit) then
  begin
    TCustomSynEdit(Dest).Font.Name := Self.FontName;
    TCustomSynEdit(Dest).Font.Size := Self.FontSize;
    if Self.FontNoAntialiasing then
      TCustomSynEdit(Dest).Font.Quality := fqNonAntialiased
    else
      TCustomSynEdit(Dest).Font.Quality := fqDefault;

    TCustomSynEdit(Dest).Font.Color := Attributes.Find('Whitespace').Foreground;
    TCustomSynEdit(Dest).Color := Attributes.Find('Whitespace').Background;
    //TCustomSynEdit(Dest).SelectedColor.

    TCustomSynEdit(Dest).Options := TCustomSynEdit(Dest).Options - [eoDropFiles]; //make main window accept the files
    TCustomSynEdit(Dest).Gutter.Assign(Self.Gutter);

    cf := TCustomSynEdit(Dest).Gutter.Parts.ByClass[TSynGutterCodeFolding, 0] as TSynGutterCodeFolding;
    if cf <> nil then
    begin
      OldCF := cf.Visible;
      cf.Visible := CodeFolding and (TCustomSynEdit(Dest).Highlighter <> nil) and (hcCodeFolding in TCustomSynEdit(Dest).Highlighter.Capabilities);
      if (cf.Visible) and (cf.Visible <> OldCF) then
        TCustomSynEdit(Dest).UnfoldAll;
    end;

    TCustomSynEdit(Dest).Options := Self.Options;
    TCustomSynEdit(Dest).ExtraLineSpacing := Self.ExtraLineSpacing;
    TCustomSynEdit(Dest).InsertCaret := Self.InsertCaret;
    TCustomSynEdit(Dest).OverwriteCaret := Self.OverwriteCaret;
    TCustomSynEdit(Dest).MaxUndo := Self.MaxUndo;
    TCustomSynEdit(Dest).RightEdge := Self.RightEdge;
    TCustomSynEdit(Dest).RightEdgeColor := Self.RightEdgeColor;
    TCustomSynEdit(Dest).TabWidth := Self.TabWidth;
  end
  else
    inherited;
end;

constructor TEditorProfile.Create(AOwner: TComponent);
begin
  inherited;
  FComponentStyle := FComponentStyle + [csSubComponent];
  FBookmarks := TSynBookMarkOpt.Create(Self);
  FGutterOptions := TGutterOptions.Create;//ToDO check the Create params
  FAttributes := TGlobalAttributes.Create(Self);
  CodeFolding := False;
  Reset;
end;

destructor TEditorProfile.Destroy;
begin
  FBookMarks.Free;
  FGutterOptions.Free;
  FAttributes.Free;
  inherited;
end;

function TEditorProfile.GetChildOwner: TComponent;
begin
  Result := Self;
end;

function TEditorProfile.GetChildParent: TComponent;
begin
  Result := Self;
end;

procedure TEditorProfile.GetChildren(Proc: TGetChildProc;
  Root: TComponent);
var
  i: Integer;
begin
  inherited;
  for i := 0 to ComponentCount - 1 do
    Proc(Components[i]);
end;

procedure TEditorProfile.Reset;
begin
  Gutter.Reset;
  FFontName := 'Courier New';
  FFontSize := 10;
  FFontNoAntialiasing := False;
  Options := cDefaultOptions;
  ExtraLineSpacing := 0;
  InsertCaret := ctVerticalLine;
  OverwriteCaret := ctBlock;
  MaxUndo := 1024;
  RightEdge := 80;
  RightEdgeColor := clSilver;
  FInsertMode := True;
  TabWidth := 2;
end;

procedure TEditorProfile.SetExtOptions(const AValue: TSynEditorOptions2);
begin
  if FExtOptions =AValue then exit;
  FExtOptions :=AValue;
end;

procedure TEditorProfile.SetOptions(const Value: TSynEditorOptions);
begin
  FOptions := Value;
end;

{ TGlobalAttributes }

function TGlobalAttributes.GetAttribute(Index: string): TGlobalAttribute;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Name, Index) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;


constructor TGlobalAttributes.Create(AOwner: TComponent);

  procedure Add(var Item: TGlobalAttribute; Name, Title: string; Foreground, Background: TColor; Style: TFontStyles);
  begin
    Item := TGlobalAttribute.Create;
    Item.Name := Name;
    Item.Title := Title;
    Item.Foreground := Foreground;
    Item.Background := Background;
    Item.Style := Style;
    FList.Add(Item);
  end;

begin
  inherited Create(AOwner);
  FComponentStyle := FComponentStyle + [csSubComponent];
  FList := TObjectList.Create;

  Add(FWhitespace, 'Whitespace', 'Whitespace', clWhite, TColor($0F192A), []);
  Add(FSelected, 'Selected', 'Selected', TColor($0F192A), clWhite, []);

  Add(FKeyword, 'Keyword', 'Keyword', clWhite, TColor($0F192A), []);
  Add(FSymbol, 'Symbol', 'Symbol', clWhite, TColor($0F192A), []);
  Add(FNumber, 'Number', 'Number', clWhite, TColor($0F192A), []);
  Add(FDirective, 'Directive', 'Directive', clWhite, TColor($0F192A), []);
  Add(FIdentifier, 'Identifier', 'Identifier', clWhite, TColor($0F192A), []);
  Add(FVariable, 'Variable', 'Variable', clWhite, TColor($0F192A), []);
  Add(FValue, 'Value', 'Value', clWhite, TColor($0F192A), []);
  Add(FDatatype, 'Datatype', 'Datatype', clWhite, TColor($0F192A), []);
  Add(FDocument, 'Document', 'Document', clWhite, TColor($0F192A), []);
  Add(FSL_comment, 'SL_comment', 'Single Line comment', clWhite, TColor($0F192A), []);
  Add(FML_comment, 'ML_comment', 'Multi Line comment', clWhite, TColor($0F192A), []);
  Add(FSQ_string, 'SQ_string', 'Single quite string', clWhite, TColor($0F192A), []);
  Add(FDQ_string, 'DQ_string', 'Double quite string', clWhite, TColor($0F192A), []);
end;

destructor TGlobalAttributes.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

function TGlobalAttributes.Find(vName: string): TGlobalAttribute;
var
  i: integer;
  S: String;
begin
  Result := nil;
  if vName <> '' then
    for i := 0 to Count - 1 do
    begin
      S := Items[i].Name;
      if SameText(S, vName) then
      begin
        Result := Items[i];
        break;
      end;
    end;
end;

function TGlobalAttributes.GetItem(Index: Integer): TGlobalAttribute;
begin
  Result := FList[Index] as TGlobalAttribute;
end;

function TGlobalAttributes.GetCount: Integer;
begin
  Result := FList.Count;
end;

{ TGlobalAttribute }

procedure TGlobalAttribute.Assign(Source: TPersistent);
begin
  if Source is TSynHighlighterAttributes then
  begin
    Name := TSynHighlighterAttributes(Source).Name;
    Background := TSynHighlighterAttributes(Source).Background;
    Foreground := TSynHighlighterAttributes(Source).Foreground;
    Style := TSynHighlighterAttributes(Source).Style;
  end
  else
    inherited;
end;

procedure TGlobalAttribute.AssignTo(Dest: TPersistent);
begin
  if Dest is TSynHighlighterAttributes then
  begin
  //  TSynHighlighterAttributes(Source).Name := Name;
    TSynHighlighterAttributes(Dest).Background := Background;
    TSynHighlighterAttributes(Dest).Foreground := Foreground;
    TSynHighlighterAttributes(Dest).Style := Style;
  end
  else
    inherited;
end;

constructor TGlobalAttribute.Create;
begin
  inherited;
  FBackground := clNone;
  FForeground := clNone;
end;

{ TGutterOptions }

constructor TGutterOptions.Create;
begin
  inherited;
  Reset;
end;

procedure TGutterOptions.Assign(Source: TPersistent);
begin
  if Source is TSynGutter then
    AssignFrom(Source as TSynGutter)
  else
    inherited Assign(Source);
end;

procedure TGutterOptions.AssignTo(Dest: TPersistent);
var
  SynGutter: TSynGutter;
  i: Integer;
  gp: TSynGutterLineNumber;
  sp: TSynGutterSeparator;
  ch: TSynGutterChanges;
begin
  if Dest is TSynGutter then
  begin
    SynGutter := Dest as TSynGutter;

    SynGutter.AutoSize := FAutoSize;
    SynGutter.Color := FBackcolor;
    for i := 0 to SynGutter.Parts.Count -1 do
    begin
      SynGutter.Parts[i].MarkupInfo.Foreground := FForecolor;
      SynGutter.Parts[i].MarkupInfo.Background := FBackcolor;
    end;
    gp := SynGutter.Parts.ByClass[TSynGutterLineNumber, 0] as TSynGutterLineNumber;
    if gp <> nil then
    begin
      gp.Visible := FShowLineNumbers;
      gp.LeadingZeros := FLeadingZeros;
      gp.ZeroStart := FZeroStart;
    end;
    sp := SynGutter.Parts.ByClass[TSynGutterSeparator, 0] as TSynGutterSeparator;
    if sp <> nil then
    begin
      sp.Visible := FShowSeparator;
      sp.MarkupInfo.Foreground := FSeparatorColor;
      sp.MarkupInfo.Background := FSeparatorColor;
    end;
    ch := SynGutter.Parts.ByClass[TSynGutterChanges, 0] as TSynGutterChanges;
    if ch <> nil then
    begin
      ch.Visible := FShowModifiedLines;
      ch.SavedColor := FSavedColor;
      ch.ModifiedColor := FUnsavedColor;
    end;
    SynGutter.LeftOffset := FLeftOffset;
    SynGutter.RightOffset := FRightOffset;
    SynGutter.Visible := FVisible;
    SynGutter.Width := FWidth;
  end
  else
    inherited AssignTo(Dest);
end;

constructor TGutterOptions.AssignFrom(SynGutter: TSynGutter);
var
  gp: TSynGutterLineNumber;
  sp: TSynGutterSeparator;
  ch: TSynGutterChanges;
begin
  FAutoSize := SynGutter.AutoSize;
  FBackcolor := SynGutter.Color;
  FLeftOffset := SynGutter.LeftOffset;
  FRightOffset := SynGutter.RightOffset;
  FVisible := SynGutter.Visible;
  FWidth := SynGutter.Width;
  gp := SynGutter.Parts.ByClass[TSynGutterLineNumber, 0] as TSynGutterLineNumber;
  if gp <> nil then
  begin
    FShowLineNumbers := gp.Visible;
    FLeadingZeros := gp.LeadingZeros;
    FZeroStart := gp.ZeroStart;
  end;
  sp := SynGutter.Parts.ByClass[TSynGutterSeparator, 0] as TSynGutterSeparator;
  if sp <> nil then
  begin
    FShowSeparator := sp.Visible;
    FSeparatorColor := sp.MarkupInfo.Foreground;
  end;
  ch := SynGutter.Parts.ByClass[TSynGutterChanges, 0] as TSynGutterChanges;
  if ch <> nil then
  begin
    FShowModifiedLines := ch.Visible;
    FSavedColor := ch.SavedColor;
    FUnsavedColor := ch.ModifiedColor;
  end;
end;

procedure TGutterOptions.Reset;
begin
  FAutoSize := True;
  FBackcolor := clBtnFace;
  FForecolor := clBtnText;
  FSeparatorColor := clBtnFace;
  FShowSeparator := True;
  FLeftOffset := 0;
  FRightOffset := 0;
  FVisible := True;
  FWidth := 30;
  FShowLineNumbers := True;
  FLeadingZeros := False;
  FZeroStart := False;
  FShowModifiedLines := True;
  FSavedColor := clGreen;
  FUnsavedColor := clYellow;
end;

end.

