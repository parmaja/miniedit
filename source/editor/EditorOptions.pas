unit EditorOptions;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  Messages, Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls,
  Registry, ExtCtrls, Buttons, ImgList, Menus, ColorBox, SynEdit, SynGutter, SynEditMarkupWordGroup,
  SynEditHighlighter, SynEditMiscClasses, SynEditKeyCmds, Classes, SysUtils,
  EditorProfiles, SynGutterBase, SynEditMarks;

type
  TSynEditorOptionsUserCommand = procedure(AUserCommand: integer; var ADescription: string) of object;

//NOTE: in order for the user commands to be recorded correctly, you must
//      put the command itself in the object property.
//      you can do this like so:
//
//      StringList.AddObject('ecSomeCommand', TObject(ecSomeCommand))
//
//      where ecSomeCommand is the command that you want to add

type
  TSynEditorOptionsAllUserCommands = procedure(ACommands: TStrings) of object;

  { TEditorOptionsForm }

  TEditorOptionsForm = class(TForm)
    AutoIndentChk: TCheckBox;
    BracketHighlightChk: TCheckBox;
    CodeFoldingChk: TCheckBox;
    EnhanceHomeKeyChk: TCheckBox;
    GroupUndoChk: TCheckBox;
    GutterAutosizeChk: TCheckBox;
    GutterGrp: TGroupBox;
    GutterShowLeaderZerosChk: TCheckBox;
    HalfPageScrollChk: TCheckBox;
    Label8: TLabel;
    Label9: TLabel;
    LineSpacingEdit: TEdit;
    NoAntialiasingChk: TCheckBox;
    Bevel1: TBevel;
    BoldChk: TCheckBox;
    FontBtn: TButton;
    FontLbl: TLabel;
    OpenDialog: TOpenDialog;
    ResetBtn: TButton;
    RevertBtn: TButton;
    SampleEdit: TSynEdit;
    SaveBtn: TButton;
    LoadBtn: TButton;
    SaveDialog: TSaveDialog;
    ScrollByOneLessChk: TCheckBox;
    ScrollHintFollowsChk: TCheckBox;
    ShowModifiedLinesChk: TCheckBox;
    ShowScrollHintChk: TCheckBox;
    ShowSeparatorChk: TCheckBox;
    ShowSpecialCharsChk: TCheckBox;
    SmartTabDeleteChk: TCheckBox;
    SmartTabsChk: TCheckBox;
    TabIndentChk: TCheckBox;
    TabsToSpacesChk: TCheckBox;
    TabWidthEdit: TEdit;
    ItalicChk: TCheckBox;
    PageControl: TPageControl;
    OkBtn: TButton;
    CancelBtn: TButton;
    OptionsTab: TTabSheet;
    FontDialog: TFontDialog;
    ColorTab: TTabSheet;
    Label11: TLabel;
    BackgroundCbo: TColorBox;
    ForegroundCbo: TColorBox;
    AttributeCbo: TComboBox;
    BackgroundChk: TCheckBox;
    ForegroundChk: TCheckBox;
    Label12: TLabel;
    CategoryCbo: TComboBox;
    UnderlineChk: TCheckBox;
    WordWrapChk: TCheckBox;
    procedure BackgroundCboChange(Sender: TObject);
    procedure ForegroundCboChange(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure NoAntialiasingChkChange(Sender: TObject);
    procedure DefaultBackgroundCboSelect(Sender: TObject);
    procedure DefaultForegroundCboSelect(Sender: TObject);
    procedure AttributeCboSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FontBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CategoryCboSelect(Sender: TObject);
    procedure GutterFontChkChange(Sender: TObject);
    procedure KeyListEditing(Sender: TObject; Item: TListItem; var AllowEdit: boolean);
    procedure OkBtnClick(Sender: TObject);
    procedure GutterFontBtnClick(Sender: TObject);
    procedure GutterFontChkClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure ResetBtnClick(Sender: TObject);
    procedure RevertBtnClick(Sender: TObject);

    procedure SampleEditGutterClick(Sender: TObject; X, Y, Line: integer; mark: TSynEditMark);
    procedure SampleEditMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure BoldChkClick(Sender: TObject);
    procedure BackgroundChkClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
  private
    FProfile: TEditorProfile;
    InChanging: boolean;
    procedure ApplyElement;
    procedure RetrieveElement;

    procedure Retrieve;
    procedure Apply;

  public
    function Execute(Profile: TEditorProfile; Select: string): boolean;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

uses
  mnXMLRttiProfile,EditorEngine, SynEditTypes;

{ TEditorOptionsForm }

function TEditorOptionsForm.Execute(Profile: TEditorProfile; Select: string): boolean;
var
  i: integer;
  n: Integer;
  aHighlighter: TSynCustomHighlighter;
  aFileCategory: TFileCategory;
  S: string;
begin
  if (Profile <> nil) then
  begin
    FProfile := TEditorProfile.Create;
    try
      FProfile.Assign(Profile);

      n := 0;
      for i := 0 to Engine.Categories.Count - 1 do
      begin
        aFileCategory := Engine.Categories[i];
        if aFileCategory.Highlighter <> nil then
        begin
          S := aFileCategory.Highlighter.GetLanguageName;
          CategoryCbo.Items.AddObject(S, aFileCategory);
          if SameText(Select, S) then
            n := CategoryCbo.Items.Count - 1;
        end;
      end;
      CategoryCbo.ItemIndex := n;

      for i := 0 to FProfile.Attributes.Count - 1 do
        AttributeCbo.Items.AddObject(FProfile.Attributes.Items[i].Title, FProfile.Attributes.Items[i]);
      AttributeCbo.ItemIndex := 0;

      Retrieve;
      //Show the form
      Result := ShowModal = mrOk;

      if Result then
      begin
        Apply;
        Profile.Assign(FProfile);
      end;
    finally
      FProfile.Free;
    end;
  end
  else
    Result := False;
end;

destructor TEditorOptionsForm.Destroy;
var
  aHighlighter : TSynCustomHighlighter;
begin
  aHighlighter := SampleEdit.Highlighter;
  SampleEdit.Highlighter := nil;
  aHighlighter.Free;
  inherited Destroy;
end;

procedure TEditorOptionsForm.FormCreate(Sender: TObject);
begin
  InChanging := False;
end;

procedure TEditorOptionsForm.AttributeCboSelect(Sender: TObject);
begin
  RetrieveElement;
end;

procedure TEditorOptionsForm.NoAntialiasingChkChange(Sender: TObject);
begin
  if NoAntialiasingChk.Checked then
  SampleEdit.Font.Quality := fqNonAntialiased
  else
    SampleEdit.Font.Quality := fqDefault;
end;

procedure TEditorOptionsForm.LoadBtnClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    XMLReadObjectFile(FProfile.Attributes, OpenDialog.FileName);
    Retrieve;
  end;
end;

procedure TEditorOptionsForm.ForegroundCboChange(Sender: TObject);
begin
  if not InChanging then
  begin
    ForegroundChk.Checked := True;
    ApplyElement;
  end;
end;

procedure TEditorOptionsForm.BackgroundCboChange(Sender: TObject);
begin
  if not InChanging then
  begin
    BackgroundChk.Checked := True;
    ApplyElement;
  end;
end;

procedure TEditorOptionsForm.DefaultBackgroundCboSelect(Sender: TObject);
begin
  ApplyElement;
end;

procedure TEditorOptionsForm.DefaultForegroundCboSelect(Sender: TObject);
begin
  ApplyElement;
end;

procedure TEditorOptionsForm.FontBtnClick(Sender: TObject);
begin
  FontDialog.Font.Assign(SampleEdit.Font);
  FontDialog.Options := FontDialog.Options - [fdNoStyleSel];
  if FontDialog.Execute then
  begin
    SampleEdit.Font.Name := FontDialog.Font.Name;
    SampleEdit.Font.Size := FontDialog.Font.Size;
    FontLbl.Caption := FontDialog.Font.Name + ' ' + IntToStr(FontDialog.Font.Size) + ' pt';
    ApplyElement;
  end;
end;

procedure TEditorOptionsForm.FormShow(Sender: TObject);
begin
  PageControl.TabIndex := 0;
end;

procedure TEditorOptionsForm.CategoryCboSelect(Sender: TObject);
begin
  Retrieve;
end;

procedure TEditorOptionsForm.GutterFontChkChange(Sender: TObject);
begin

end;

procedure TEditorOptionsForm.KeyListEditing(Sender: TObject; Item: TListItem; var AllowEdit: boolean);
begin
  AllowEdit := False;
end;

procedure TEditorOptionsForm.OkBtnClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TEditorOptionsForm.GutterFontBtnClick(Sender: TObject);
begin

end;

procedure TEditorOptionsForm.GutterFontChkClick(Sender: TObject);
begin
end;

procedure TEditorOptionsForm.PageControlChange(Sender: TObject);
begin

end;

procedure TEditorOptionsForm.ResetBtnClick(Sender: TObject);
begin
  InChanging := True;
  try
    FProfile.Attributes.Reset;
  finally
    InChanging := False;
  end;
  Retrieve;
end;

procedure TEditorOptionsForm.RevertBtnClick(Sender: TObject);
begin
  InChanging := True;
  try
    Retrieve;//TODO
  finally
    InChanging := False;
  end;
end;

procedure TEditorOptionsForm.SampleEditGutterClick(Sender: TObject; X, Y, Line: integer; mark: TSynEditMark);
var
  M: TMap;
  G: TGlobalAttribute;
  aFileCategory: TFileCategory;
  s:string;
begin
  aFileCategory := TFileCategory(CategoryCbo.Items.Objects[CategoryCbo.ItemIndex]);

  G := FProfile.Attributes.Find(attGutter);

  if G = nil then
    G := FProfile.Attributes.Whitespace;

  AttributeCbo.ItemIndex := G.Index;
  RetrieveElement;
end;

procedure TEditorOptionsForm.SampleEditMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  Attributes: TSynHighlighterAttributes;
  M: TMap;
  G: TGlobalAttribute;
  p: TPoint;
  s: string;
  aFileCategory: TFileCategory;
begin
  p := SampleEdit.PixelsToRowColumn(Point(X, Y));
  if p.x > 0 then //not in the gutter
  begin
    Attributes := nil;
    if not SampleEdit.GetHighlighterAttriAtRowCol(Point(p.x, p.y), s, Attributes) then
      Attributes := nil;
    if Attributes = nil then
      Attributes := SampleEdit.Highlighter.WhitespaceAttribute;
    if Attributes <> nil then
    begin
      aFileCategory := TFileCategory(CategoryCbo.Items.Objects[CategoryCbo.ItemIndex]);
      s := Attributes.Name;
      M := aFileCategory.Mapper.Find(s);
      if M <> nil then
        G := FProfile.Attributes.Find(M.AttType);

      if G = nil then
        G := FProfile.Attributes.Whitespace;

      AttributeCbo.ItemIndex := G.Index;
    end
    else
      AttributeCbo.ItemIndex := -1;
    RetrieveElement;
  end;
end;

procedure TEditorOptionsForm.RetrieveElement;
var
  aColor: TColor;
  aGlobalAttribute: TGlobalAttribute;
begin
  if not InChanging and (AttributeCbo.ItemIndex >= 0) then
  begin
    aGlobalAttribute := (AttributeCbo.Items.Objects[AttributeCbo.ItemIndex] as TGlobalAttribute);
    InChanging := True;
    try
      aColor := aGlobalAttribute.Foreground;
      if aColor = clNone then
      begin
        ForegroundChk.Checked := False;
        ForegroundCbo.Selected := clBlack;
      end
      else
      begin
        ForegroundChk.Checked := True;
        ForegroundCbo.Selected := aColor;
        ForegroundCbo.Refresh;//bug when custom and then custom colors
      end;

      aColor := aGlobalAttribute.Background;
      if aColor = clNone then
      begin
        BackgroundChk.Checked := False;
        BackgroundCbo.Selected := clWindow;
      end
      else
      begin
        BackgroundChk.Checked := True;
        BackgroundCbo.Selected := aColor;
        BackgroundCbo.Refresh;//bug when custom and then custom colors
      end;

      BoldChk.Checked := (fsBold in aGlobalAttribute.Style);
      ItalicChk.Checked := (fsItalic in aGlobalAttribute.Style);
      UnderlineChk.Checked := (fsUnderline in aGlobalAttribute.Style);
    finally
      InChanging := False;
    end;
  end;
end;

procedure TEditorOptionsForm.ApplyElement;
var
  i: Integer;
  aFontStyle: TFontStyles;
  aFileCategory: TFileCategory;
  aGlobalAttribute: TGlobalAttribute;
  sp: TSynGutterSeparator;
begin
  if not InChanging and (AttributeCbo.ItemIndex >= 0) then
  begin
    InChanging := True;
    try
      aFileCategory := TFileCategory(CategoryCbo.Items.Objects[CategoryCbo.ItemIndex]);
      aGlobalAttribute := (AttributeCbo.Items.Objects[AttributeCbo.ItemIndex] as TGlobalAttribute);

      //Copy some from TGutterOptions.AssignTo(Dest: TPersistent);

      if ForegroundChk.Checked then
        aGlobalAttribute.Foreground := ForegroundCbo.Selected
      else
        aGlobalAttribute.Foreground := clNone;

      if BackgroundChk.Checked then
        aGlobalAttribute.Background := BackgroundCbo.Selected
      else
        aGlobalAttribute.Background := clNone;

      aFontStyle := [];
      if BoldChk.Checked then
        aFontStyle := aFontStyle + [fsBold];
      if UnderlineChk.Checked then
        aFontStyle := aFontStyle + [fsUnderline];
      {if ItalicChk.Checked then
        aFontStyle := aFontStyle + [fsItalic];}

      aGlobalAttribute.Style := aFontStyle;

      SampleEdit.Gutter.Color := FProfile.Attributes.Gutter.Background;
      for i := 0 to SampleEdit.Gutter.Parts.Count -1 do
      begin
        SampleEdit.Gutter.Parts[i].MarkupInfo.Foreground := FProfile.Attributes.Gutter.Foreground;
        SampleEdit.Gutter.Parts[i].MarkupInfo.Background := FProfile.Attributes.Gutter.Background;
      end;

      sp := SampleEdit.Gutter.Parts.ByClass[TSynGutterSeparator, 0] as TSynGutterSeparator;
      if sp <> nil then
      begin
        sp.Visible := ShowSeparatorChk.Checked;
        sp.MarkupInfo.Foreground := FProfile.Attributes.Separator.Background;
        sp.MarkupInfo.Background := FProfile.Attributes.Separator.Foreground;
      end;

      SampleEdit.Font.Color := FProfile.Attributes.Whitespace.Foreground;
      SampleEdit.Color := FProfile.Attributes.Whitespace.Background;
      SampleEdit.SelectedColor.Foreground := FProfile.Attributes.Selected.Foreground;
      SampleEdit.SelectedColor.Background := FProfile.Attributes.Selected.Background;
      SampleEdit.BracketMatchColor.Foreground := FProfile.Attributes.Selected.Foreground;
      SampleEdit.BracketMatchColor.Background := FProfile.Attributes.Selected.Background;

      SampleEdit.MarkupManager.MarkupByClass[TSynEditMarkupWordGroup].MarkupInfo.FrameColor := FProfile.Attributes.Selected.Background;

      if FProfile.Attributes.FontNoAntialiasing then
        SampleEdit.Font.Quality := fqNonAntialiased
      else
        SampleEdit.Font.Quality := fqDefault;

      aFileCategory := TFileCategory(CategoryCbo.Items.Objects[CategoryCbo.ItemIndex]);
      if (SampleEdit.Highlighter = nil) or (SampleEdit.Highlighter.ClassType <> aFileCategory.Highlighter.ClassType) then
      begin
        SampleEdit.Highlighter.Free;
        SampleEdit.Highlighter := aFileCategory.CreateHighlighter;
        SampleEdit.Text := SampleEdit.Highlighter.SampleSource;
      end;
      aFileCategory.Apply(SampleEdit.Highlighter, FProfile.Attributes);

      FProfile.AssignTo(SampleEdit);

      if SampleEdit.Highlighter <> nil then //remove Divider
        for i := 0 to SampleEdit.Highlighter.DividerDrawConfigCount - 1 do
          SampleEdit.Highlighter.DividerDrawConfig[i].MaxDrawDepth := 0;
    finally
      InChanging := False;
    end;
  end;
end;

procedure TEditorOptionsForm.Retrieve;
var
  i: integer;
  aFileCategory: TFileCategory;
begin
  TabsToSpacesChk.Checked := eoTabsToSpaces in FProfile.EditorOptions;
  CodeFoldingChk.Checked := FProfile.Attributes.CodeFolding;

  FontLbl.Caption := SampleEdit.Font.Name + ' ' + IntToStr(SampleEdit.Font.Size) + ' pt';
  //Font
  SampleEdit.Font.Name := FProfile.Attributes.FontName;
  SampleEdit.Font.Size := FProfile.Attributes.FontSize;

  NoAntialiasingChk.Checked := FProfile.Attributes.FontNoAntialiasing;

  //Gutter
  GutterAutosizeChk.Checked := FProfile.Attributes.GutterAutoSize;
  ShowSeparatorChk.Checked := FProfile.Attributes.GutterShowSeparator;

  ShowModifiedLinesChk.Checked := FProfile.Attributes.GutterShowModifiedLines;

  GutterShowLeaderZerosChk.Checked := FProfile.Attributes.GutterLeadingZeros;

  //Line Spacing
  LineSpacingEdit.Text := IntToStr(FProfile.ExtraLineSpacing);

  //Options
  AutoIndentChk.Checked := eoAutoIndent in FProfile.EditorOptions;
  TabIndentChk.Checked := eoTabIndent in FProfile.EditorOptions;
  SmartTabsChk.Checked := eoSmartTabs in FProfile.EditorOptions;
  HalfPageScrollChk.Checked := eoHalfPageScroll in FProfile.EditorOptions;
  ScrollByOneLessChk.Checked := eoScrollByOneLess in FProfile.EditorOptions;
  ShowScrollHintChk.Checked := eoShowScrollHint in FProfile.EditorOptions;
  SmartTabDeleteChk.Checked := eoSmartTabDelete in FProfile.EditorOptions;
  EnhanceHomeKeyChk.Checked := eoEnhanceHomeKey in FProfile.EditorOptions;
  GroupUndoChk.Checked := eoGroupUndo in FProfile.EditorOptions;
  ShowSpecialCharsChk.Checked := eoShowSpecialChars in FProfile.EditorOptions;
  BracketHighlightChk.Checked := eoBracketHighlight in FProfile.EditorOptions;
  //Can be override by project options
  TabWidthEdit.Text := IntToStr(FProfile.TabWidth);

  RetrieveElement;
end;

procedure TEditorOptionsForm.Apply;
var
  aOptions: TSynEditorOptions;
  aExtOptions: TSynEditorOptions2;

  procedure SetFlag(aOption: TSynEditorOption; aValue: boolean);
  begin
    if aValue then
      Include(aOptions, aOption)
    else
      Exclude(aOptions, aOption);
  end;

  procedure SetExtFlag(aOption: TSynEditorOption2; aValue: boolean);
  begin
    if aValue then
      Include(aExtOptions, aOption)
    else
      Exclude(aExtOptions, aOption);
  end;
begin
  //Options
  aOptions := FProfile.EditorOptions; //Keep old values for unsupported options
  aExtOptions := FProfile.ExtEditorOptions;
  SetFlag(eoAutoIndent, AutoIndentChk.Checked);
  SetFlag(eoTabIndent, TabIndentChk.Checked);
  SetFlag(eoSmartTabs, SmartTabsChk.Checked);
  SetFlag(eoHalfPageScroll, HalfPageScrollChk.Checked);
  SetFlag(eoScrollByOneLess, ScrollByOneLessChk.Checked);
  SetFlag(eoShowScrollHint, ShowScrollHintChk.Checked);
  SetFlag(eoSmartTabDelete, SmartTabDeleteChk.Checked);
  SetFlag(eoEnhanceHomeKey, EnhanceHomeKeyChk.Checked);
  SetFlag(eoGroupUndo, GroupUndoChk.Checked);
  SetFlag(eoShowSpecialChars, ShowSpecialCharsChk.Checked);
  SetFlag(eoBracketHighlight, BracketHighlightChk.Checked);
  SetFlag(eoTabsToSpaces, TabsToSpacesChk.Checked);

  FProfile.EditorOptions := aOptions;
  FProfile.ExtEditorOptions := aExtOptions;

  //Gutter
  FProfile.Attributes.GutterAutoSize := GutterAutosizeChk.Checked;
  FProfile.Attributes.GutterShowSeparator := ShowSeparatorChk.Checked;
  FProfile.Attributes.GutterShowModifiedLines := ShowModifiedLinesChk.Checked;
  FProfile.Attributes.GutterLeadingZeros := GutterShowLeaderZerosChk.Checked;
  FProfile.Attributes.CodeFolding := CodeFoldingChk.Checked;

  //Spacing
  FProfile.ExtraLineSpacing := StrToIntDef(LineSpacingEdit.Text, 0);
  FProfile.TabWidth := StrToIntDef(TabWidthEdit.Text, 4);

  //Font
  FProfile.Attributes.FontName := SampleEdit.Font.Name;
  FProfile.Attributes.FontSize := SampleEdit.Font.Size;
  FProfile.Attributes.FontNoAntialiasing := SampleEdit.Font.Quality = fqNonAntialiased;
  FProfile.Attributes.Assign(FProfile.Attributes);
end;

procedure TEditorOptionsForm.BoldChkClick(Sender: TObject);
begin
  ApplyElement;
end;

procedure TEditorOptionsForm.BackgroundChkClick(Sender: TObject);
begin
  if not InChanging then
    BackgroundChk.Checked := True;
  ApplyElement;
end;

procedure TEditorOptionsForm.SaveBtnClick(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    Apply;
    XMLWriteObjectFile(FProfile.Attributes, SaveDialog.FileName);
  end;
end;

end.
