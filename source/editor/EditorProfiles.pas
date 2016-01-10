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
  mnXMLRttiProfile, SynEditMarkupWordGroup,
  Contnrs, Menus, SynEdit, SynEditHighlighter, SynEditMiscClasses, SynEditPointClasses, SynGutterCodeFolding,
  SynGutter, SynEditKeyCmds, Classes, SysUtils;

type
  TAttributeType = (
    attUI,
    attPanel,
    attURL,
    attGutter,
    attSeparator,
    attSelected,
    attModified,
    attWhitespace,
    attKeyword,
    attString,
    attDocument,
    attComment,
    attSymbol,
    attStandard,
    attNumber,
    attDirective,
    attIdentifier,
    attText,
    attOutter,
    attInner,
    attVariable,
    attType,
    attName,
    attValue
   );

const
  cSynRequiredOptions = [eoDragDropEditing, eoTrimTrailingSpaces, eoDropFiles, eoShowCtrlMouseLinks, eoAltSetsColumnMode, eoScrollPastEol, eoRightMouseMovesCursor, eoHideRightMargin];

  cSynRemoveOptions = [eoRightMouseMovesCursor, eoScrollPastEof];

  cSynOverridedOptions = [eoTabsToSpaces];

  cSynDefaultOptions = cSynRequiredOptions + cSynRemoveOptions + [eoAutoIndent, eoBracketHighlight,
    eoShowScrollHint, eoTabsToSpaces, eoTabIndent, eoTrimTrailingSpaces, eoKeepCaretX];

type
  TGlobalAttributes = class;

  { TGlobalAttribute }

  TGlobalAttribute = class(TPersistent)
  private
    FBackground: TColor;
    FForeground: TColor;
    FIndex: Integer;
    FParent: TGlobalAttributes;
    FStyle: TFontStyles;
    FAttType: TAttributeType;
    FTitle: string;
  protected
    property Parent: TGlobalAttributes read FParent;
  public
    constructor Create;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    property Index: Integer read FIndex;
  published
    property Title: string read FTitle write FTitle;
    property AttType: TAttributeType read FAttType write FAttType;
    property Background: TColor read FBackground write FBackground default clNone;
    property Foreground: TColor read FForeground write FForeground default clNone;
    property Style: TFontStyles read FStyle write FStyle default [];
  end;

  { TGlobalAttributes }
  TGlobalAttributesInfo = record
    FontName: String;
    FontNoAntialiasing: Boolean;
    FontSize: Integer;
    GutterAutoSize: boolean;
    GutterLeftOffset: integer;
    GutterRightOffset: integer;
    GutterShowModifiedLines: Boolean;
    GutterLeadingZeros: Boolean;
    GutterShowSeparator: Boolean;
    GutterWidth: Integer;
    CodeFolding: Boolean;
  end;

  TGlobalAttributes = class(TComponent)
  private
    FInfo: TGlobalAttributesInfo;
    FInner: TGlobalAttribute;
    FOutter: TGlobalAttribute;
    FDataName: TGlobalAttribute;
    FDataType: TGlobalAttribute;
    FDirective: TGlobalAttribute;

    FDocument: TGlobalAttribute;
    FQuotedString: TGlobalAttribute;
    FIdentifier: TGlobalAttribute;
    FKeyword: TGlobalAttribute;

    FNumber: TGlobalAttribute;
    FSelected: TGlobalAttribute;
    FModified: TGlobalAttribute;
    FGutter: TGlobalAttribute;
    FSeparator: TGlobalAttribute;
    FComment: TGlobalAttribute;
    FSymbol: TGlobalAttribute;
    FText: TGlobalAttribute;
    FUI: TGlobalAttribute;
    FPanel: TGlobalAttribute;
    FURL: TGlobalAttribute;
    FValue: TGlobalAttribute;
    FVariable: TGlobalAttribute;
    FStandard: TGlobalAttribute;
    FWhitespace: TGlobalAttribute;

    FList: TObjectList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TGlobalAttribute;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Reset;
    function Find(AttType: TAttributeType): TGlobalAttribute;
    property Items[Index: Integer]: TGlobalAttribute read GetItem; default;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    constructor AssignFrom(SynGutter: TSynGutter);
    property Count: Integer read GetCount;
  public
    property Info: TGlobalAttributesInfo read FInfo write FInfo;
  published
    property UI: TGlobalAttribute read FUI;
    property URL: TGlobalAttribute read FURL;
    property Selected: TGlobalAttribute read FSelected;
    property Gutter: TGlobalAttribute read FGutter;
    property Separator: TGlobalAttribute read FSeparator;
    property Modified: TGlobalAttribute read FModified;
    property Whitespace: TGlobalAttribute read FWhitespace;
    property Keyword: TGlobalAttribute read FKeyword;
    property Symbol: TGlobalAttribute read FSymbol;
    property Number: TGlobalAttribute read FNumber;
    property Directive: TGlobalAttribute read FDirective;
    property Identifier: TGlobalAttribute read FIdentifier;
    property Variable: TGlobalAttribute read FVariable;
    property Standard: TGlobalAttribute read FStandard;
    property Value: TGlobalAttribute read FValue;
    property DataType: TGlobalAttribute read FDataType;
    property DataName: TGlobalAttribute read FDataName;
    property Document: TGlobalAttribute read FDocument;
    property Text: TGlobalAttribute read FText;
    property Outter: TGlobalAttribute read FOutter;
    property Inner: TGlobalAttribute read FInner;
    property Comment: TGlobalAttribute read FComment;
    property QuotedString: TGlobalAttribute read FQuotedString;
    property FontName: String read FInfo.FontName write FInfo.FontName;
    property FontSize: Integer read FInfo.FontSize write FInfo.FontSize;
    property FontNoAntialiasing: Boolean read FInfo.FontNoAntialiasing write FInfo.FontNoAntialiasing default False;
    property CodeFolding: Boolean read FInfo.CodeFolding write FInfo.CodeFolding default False;

    property GutterAutoSize: boolean read FInfo.GutterAutoSize write FInfo.GutterAutoSize default True;
    property GutterShowSeparator: Boolean read FInfo.GutterShowSeparator write FInfo.GutterShowSeparator default True;
    property GutterShowModifiedLines: Boolean read FInfo.GutterShowModifiedLines write FInfo.GutterShowModifiedLines default True;
    property GutterWidth: integer read FInfo.GutterWidth write FInfo.GutterWidth default 30;
    property GutterLeadingZeros: Boolean read FInfo.GutterLeadingZeros write FInfo.GutterLeadingZeros default False;
    property GutterLeftOffset: integer read FInfo.GutterLeftOffset write FInfo.GutterLeftOffset default 0;
    property GutterRightOffset: integer read FInfo.GutterRightOffset write FInfo.GutterRightOffset default 0;
  end;

  //This class is assignable to a SynEdit without modifying key properties that affect function

  TEditorProfileInfo = record
    DrawDivider: Boolean;
    MaxUndo: Integer;
    ExtraLineSpacing: Integer;
    TabsToSpaces: Boolean;
    TabWidth: Integer;
    EditorOptions: TSynEditorOptions;
    ExtEditorOptions: TSynEditorOptions2;
  end;

  { TEditorProfile }

  TEditorProfile = class(TmnXMLProfile)
  private
    FInfo: TEditorProfileInfo;
    FAttributes: TGlobalAttributes;
    FBookmarks: TSynBookMarkOpt;
  protected
  public
    property Info: TEditorProfileInfo read FInfo write FInfo;
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    constructor AssignFrom(SynEdit: TSynEdit);
    procedure Reset;
  published
    property Attributes: TGlobalAttributes read FAttributes;

    property EditorOptions: TSynEditorOptions read FInfo.EditorOptions write FInfo.EditorOptions default cSynDefaultOptions;
    property ExtEditorOptions: TSynEditorOptions2 read FInfo.ExtEditorOptions write FInfo.ExtEditorOptions default [];
    property ExtraLineSpacing: Integer read FInfo.ExtraLineSpacing write FInfo.ExtraLineSpacing default 0;
    property MaxUndo: Integer read FInfo.MaxUndo write FInfo.MaxUndo default 1024;
    property DrawDivider: Boolean read FInfo.DrawDivider write FInfo.DrawDivider default False; //TODO not yet
    //Can be overriden by project options
    property TabWidth: Integer read FInfo.TabWidth write FInfo.TabWidth default 4;
  end;

implementation

uses
  SynGutterBase, SynGutterLineNumber, SynGutterChanges;

{ TEditorProfile }

constructor TEditorProfile.Create;
begin
  inherited;
  FBookmarks := TSynBookMarkOpt.Create(nil);
  FAttributes := TGlobalAttributes.Create(nil);
  DrawDivider := False;
  Reset;
end;

destructor TEditorProfile.Destroy;
begin
  FBookMarks.Free;
  FAttributes.Free;
  inherited;
end;

procedure TEditorProfile.Assign(Source: TPersistent);
begin
  if Source is TSynEdit then
    AssignFrom(Source as TSynEdit)
  else if Source is TEditorProfile then
  begin
    Info := (Source as TEditorProfile).Info;
    Attributes.Assign((Source as TEditorProfile).Attributes);
  end
  else
    inherited Assign(Source);
end;

procedure TEditorProfile.AssignTo(Dest: TPersistent);
var
  SynEdit: TSynEdit;
begin
  if Dest is TSynEdit then
  begin
    SynEdit := Dest as TSynEdit;

    SynEdit.Font.Name := Attributes.FontName;
    SynEdit.Font.Size := Attributes.FontSize;
    if Attributes.FontNoAntialiasing then
      SynEdit.Font.Quality := fqNonAntialiased
    else
      SynEdit.Font.Quality := fqDefault;

    SynEdit.Font.Color := Attributes.Whitespace.Foreground;
    SynEdit.Color := Attributes.Whitespace.Background;
    SynEdit.SelectedColor.Foreground := Attributes.Selected.Foreground;
    SynEdit.SelectedColor.Background := Attributes.Selected.Background;
    SynEdit.BracketMatchColor.Foreground := Attributes.Selected.Foreground;
    SynEdit.BracketMatchColor.Background := Attributes.Selected.Background;

    SynEdit.MarkupManager.MarkupByClass[TSynEditMarkupWordGroup].MarkupInfo.FrameColor := Attributes.Selected.Background;

    SynEdit.Options := EditorOptions + cSynRequiredOptions - cSynRemoveOptions;
    SynEdit.ExtraLineSpacing := ExtraLineSpacing;
    SynEdit.InsertCaret := ctVerticalLine;
    SynEdit.OverwriteCaret := ctBlock;
    SynEdit.MaxUndo := MaxUndo;
    SynEdit.RightEdge := 80;
    SynEdit.RightEdgeColor := clSilver;

    SynEdit.Gutter.Assign(Attributes);

    SynEdit.TabWidth := TabWidth;
  end
  else
    inherited AssignTo(Dest);
end;

constructor TEditorProfile.AssignFrom(SynEdit: TSynEdit);
begin
end;

procedure TEditorProfile.Reset;
begin
  Attributes.Reset;
  EditorOptions := cSynDefaultOptions;
  //ExtEditorOptions :=
  ExtraLineSpacing := 0;
  MaxUndo := 1024;
  TabWidth := 4;
end;

{ TGlobalAttributes }

constructor TGlobalAttributes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FComponentStyle := FComponentStyle + [csSubComponent];
  FList := TObjectList.Create(True);
  Reset;
end;

destructor TGlobalAttributes.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TGlobalAttributes.Reset;
  procedure Add(var Item: TGlobalAttribute; AttType: TAttributeType; Title: string; Foreground, Background: TColor; Style: TFontStyles);
  begin
    Item := TGlobalAttribute.Create;
    Item.AttType := AttType;
    Item.Title := Title;
    Item.Foreground := Foreground;
    Item.Background := Background;
    Item.Style := Style;
    Item.FParent := Self;
    Item.FIndex := FList.Add(Item);
  end;

begin
  FontName := 'Courier New';
  FontSize := 10;
  FontNoAntialiasing := False;
  GutterAutoSize := True;
  GutterShowSeparator := True;
  GutterLeftOffset := 0;
  GutterRightOffset := 0;
  GutterWidth := 30;
  GutterLeadingZeros := False;
  GutterShowModifiedLines := True;

  FList.Clear;
  Add(FUI, attUI, 'User Interface', clNone, clNone, []);
  Add(FPanel, attPanel, 'Panel', clNone, clNone, []);
  Add(FURL, attURL, 'URL', clWhite, TColor($2A190F), []);

  Add(FWhitespace, attWhitespace, 'Whitespace', clWhite, TColor($2A190F), []);
  Add(FSelected, attSelected, 'Selected', clBlack, TColor($DD8B42), []);
  Add(FModified, attModified, 'Modified', clYellow, clGreen, []);
  Add(FGutter, attGutter, 'Gutter', clWhite, $4b4b4b, []);
  Add(FSeparator, attSeparator, 'Separator', clWhite, $4b4b4b, []);

  Add(FKeyword, attKeyword, 'Keyword', TColor($3737E8), clNone, []);
  Add(FQuotedString, attString, 'String', TColor($16C11D), clNone, []);
  Add(FDocument, attDocument, 'Document', TColor($DD8B42), clNone, []);
  Add(FComment, attComment, 'Comment', TColor($94541B), clNone, []);
  Add(FSymbol, attSymbol, 'Symbol', TColor($FFEDD1), clNone, []);
  Add(FStandard, attStandard, 'Standard', TColor($3EAAFF), clNone, []);
  Add(FNumber, attNumber, 'Number', TColor($0FDFEA), clNone, []);
  Add(FDirective, attDirective, 'Directive', TColor($3737E8), clNone, []);
  Add(FIdentifier, attIdentifier, 'Identifier', clNone, clNone, []);
  Add(FText, attText, 'Text', clNone, clNone, []);
  Add(FOutter, attOutter, 'Outter', TColor($DD8B42), clNone, []);
  Add(FInner, attInner, 'Inner', TColor($16C11D), clNone, []);
  Add(FVariable, attVariable, 'Variable', clSkyBlue, clNone, []);
  Add(FDataType, attType, 'Type', TColor($2f7adf), clNone, []);
  Add(FDataName, attName, 'Name', TColor($16C11D), clNone, []);
  Add(FValue, attValue, 'Value',  TColor($16C11D), clNone, []);
end;

function TGlobalAttributes.Find(AttType: TAttributeType): TGlobalAttribute;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Items[i].AttType = AttType then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

procedure TGlobalAttributes.Assign(Source: TPersistent);
var
  i: Integer;
begin
  if Source is TSynGutter then
    AssignFrom(Source as TSynGutter)
  else if Source is TGlobalAttributes then
  begin
    Info := (Source as TGlobalAttributes).Info;
    for i := 0 to Count -1 do
      Items[i].Assign((Source as TGlobalAttributes).Items[i]);
  end
  else
    inherited Assign(Source);
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
    Background := TSynHighlighterAttributes(Source).Background;
    Foreground := TSynHighlighterAttributes(Source).Foreground;
    Style := TSynHighlighterAttributes(Source).Style - [fsItalic]; //removed old font style from old version of miniedit
  end
  else if Source is TGlobalAttribute then
  begin
    Background := (Source as TGlobalAttribute).Background;
    Foreground := (Source as TGlobalAttribute).Foreground;
    Style := (Source as TGlobalAttribute).Style - [fsItalic]; //removed old font style from old version of miniedit
  end
  else
    inherited;
end;

procedure TGlobalAttribute.AssignTo(Dest: TPersistent);
begin
  if Dest is TSynHighlighterAttributes then
  begin
    TSynHighlighterAttributes(Dest).Background := Background;
    TSynHighlighterAttributes(Dest).Foreground := Foreground;
    TSynHighlighterAttributes(Dest).Style := Style - [fsItalic]; //removed old font style from old version of miniedit
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

procedure TGlobalAttributes.AssignTo(Dest: TPersistent);
var
  SynGutter: TSynGutter;
  i: Integer;
  gp: TSynGutterLineNumber;
  sp: TSynGutterSeparator;
  ch: TSynGutterChanges;
  cf: TSynGutterCodeFolding;
  OldCF: Boolean;
begin
  if Dest is TSynGutterBase then
  begin
    SynGutter := Dest as TSynGutter;
    SynGutter.AutoSize := GutterAutoSize;
    SynGutter.Color := Gutter.Background;
    for i := 0 to SynGutter.Parts.Count -1 do
    begin
      SynGutter.Parts[i].MarkupInfo.Foreground := Gutter.Foreground;
      SynGutter.Parts[i].MarkupInfo.Background := Gutter.Background;
    end;

    gp := SynGutter.Parts.ByClass[TSynGutterLineNumber, 0] as TSynGutterLineNumber;
    if gp <> nil then
    begin
      gp.Visible := True;
      gp.ZeroStart := False;
      gp.LeadingZeros := GutterLeadingZeros;
    end;

    sp := SynGutter.Parts.ByClass[TSynGutterSeparator, 0] as TSynGutterSeparator;
    if sp <> nil then
    begin
      sp.Visible := GutterShowSeparator;
      sp.MarkupInfo.Foreground := Separator.Background;
      sp.MarkupInfo.Background := Separator.Foreground;
    end;

    ch := SynGutter.Parts.ByClass[TSynGutterChanges, 0] as TSynGutterChanges;
    if ch <> nil then
    begin
      ch.Visible := GutterShowModifiedLines;
      ch.SavedColor := Modified.Background;
      ch.ModifiedColor := Modified.Foreground;
    end;

    cf := SynGutter.Parts.ByClass[TSynGutterCodeFolding, 0] as TSynGutterCodeFolding;
    if cf <> nil then
    begin
      OldCF := cf.Visible;
      cf.Visible := CodeFolding and ((SynGutter.SynEdit as TSynEdit).Highlighter <> nil) and (hcCodeFolding in (SynGutter.SynEdit as TSynEdit).Highlighter.Capabilities);
      if (cf.Visible) and (cf.Visible <> OldCF) then
        (SynGutter.SynEdit as TSynEdit).UnfoldAll;
    end;

    SynGutter.LeftOffset := GutterLeftOffset;
    SynGutter.RightOffset := GutterRightOffset;
    SynGutter.Width := GutterWidth;
    SynGutter.Visible := True; //TODO
  end
  else if Dest is TGlobalAttributes then
    (Dest as TGlobalAttributes).Info := Info
  else
    inherited AssignTo(Dest);
end;

constructor TGlobalAttributes.AssignFrom(SynGutter: TSynGutter);
var
  gp: TSynGutterLineNumber;
  sp: TSynGutterSeparator;
  ch: TSynGutterChanges;
begin
  GutterAutoSize := SynGutter.AutoSize;
  GutterLeftOffset := SynGutter.LeftOffset;
  GutterRightOffset := SynGutter.RightOffset;
  GutterWidth := SynGutter.Width;

  gp := SynGutter.Parts.ByClass[TSynGutterLineNumber, 0] as TSynGutterLineNumber;
  if gp <> nil then
  begin
    GutterLeadingZeros := gp.LeadingZeros;
  end;
  sp := SynGutter.Parts.ByClass[TSynGutterSeparator, 0] as TSynGutterSeparator;
  if sp <> nil then
  begin
    GutterShowSeparator := sp.Visible;
  end;
  ch := SynGutter.Parts.ByClass[TSynGutterChanges, 0] as TSynGutterChanges;
  if ch <> nil then
  begin
    GutterShowModifiedLines := ch.Visible;
  end;
end;


end.

