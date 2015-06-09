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
  Registry, ExtCtrls, Buttons, ImgList, Menus, ColorBox, SynEdit,
  SynEditHighlighter, SynEditMiscClasses, SynEditKeyCmds, Classes, SysUtils,
  EditorProfiles;

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
    NoAntialiasingChk: TCheckBox;
    Bevel1: TBevel;
    BracketHighlightChk: TCheckBox;
    DefBackgroundCbo: TColorBox;
    BoldChk: TCheckBox;
    FontBtn: TButton;
    FontLbl: TLabel;
    DefForegroundCbo: TColorBox;
    CodeFoldingChk: TCheckBox;
    Label15: TLabel;
    Label7: TLabel;
    SavedColorCbo: TColorBox;
    UnsavedColorCbo: TColorBox;
    ShowSeparatorChk: TCheckBox;
    SeparatorColorCbo: TColorBox;
    GutterForecolorCbo: TColorBox;
    ItalicChk: TCheckBox;
    Label13: TLabel;
    Label14: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    PageControl: TPageControl;
    OkBtn: TButton;
    CancelBtn: TButton;
    GutterTab: TTabSheet;
    OptionsTab: TTabSheet;
    LineSpacingGrp: TGroupBox;
    LineSpacingEdit: TEdit;
    GutterGrp: TGroupBox;
    Label1: TLabel;
    GutterAutosizeChk: TCheckBox;
    GutterShowLineNumbersChk: TCheckBox;
    GutterShowLeaderZerosChk: TCheckBox;
    GutterStartAtZeroChk: TCheckBox;
    GutterVisibleChk: TCheckBox;
    RightEdgeGrp: TGroupBox;
    Label3: TLabel;
    RightEdgeEdit: TEdit;
    OptionsGrp: TGroupBox;
    AutoIndentChk: TCheckBox;
    DragAndDropEditingChk: TCheckBox;
    AutoSizeMaxWidthChk: TCheckBox;
    HalfPageScrollChk: TCheckBox;
    EnhanceEndKeyChk: TCheckBox;
    ScrollByOneLessChk: TCheckBox;
    ScrollPastEOFChk: TCheckBox;
    ScrollPastEOLChk: TCheckBox;
    ShowScrollHintChk: TCheckBox;
    ShowModifiedLinesChk: TCheckBox;
    SmartTabsChk: TCheckBox;
    TabsToSpacesChk: TCheckBox;
    TrimTrailingSpacesChk: TCheckBox;
    CaretGrp: TGroupBox;
    InsertCaretCbo: TComboBox;
    Label2: TLabel;
    Label4: TLabel;
    OverwriteCaretCbo: TComboBox;
    FontDialog: TFontDialog;
    AltSetsColumnModeChk: TCheckBox;
    KeepCaretXChk: TCheckBox;
    TabWidthEdit: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    ScrollHintFollowsChk: TCheckBox;
    GroupUndoChk: TCheckBox;
    SmartTabDeleteChk: TCheckBox;
    EnhanceHomeKeyChk: TCheckBox;
    HideShowScrollbarsChk: TCheckBox;
    DisableScrollArrowsChk: TCheckBox;
    ShowSpecialCharsChk: TCheckBox;
    ColorTab: TTabSheet;
    SampleEdit: TSynEdit;
    Label11: TLabel;
    BackgroundCbo: TColorBox;
    ForegroundCbo: TColorBox;
    AttributeCbo: TComboBox;
    GutterBackcolorCbo: TColorBox;
    RightEdgeColorCbo: TColorBox;
    BackgroundChk: TCheckBox;
    ForegroundChk: TCheckBox;
    RightMouseMovesChk: TCheckBox;
    InsertModeChk: TCheckBox;
    TabIndentChk: TCheckBox;
    ResetBtn: TButton;
    Label12: TLabel;
    CategoryCbo: TComboBox;
    UnderlineChk: TCheckBox;
    WordWrapChk: TCheckBox;
    procedure NoAntialiasingChkChange(Sender: TObject);
    procedure BackgroundCboSelect(Sender: TObject);
    procedure DefaultBackgroundCboSelect(Sender: TObject);
    procedure DefaultForegroundCboSelect(Sender: TObject);
    procedure AttributeCboSelect(Sender: TObject);
    procedure ForegroundCboSelect(Sender: TObject);
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
    procedure SampleEditMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure GroupCboClick(Sender: TObject);
    procedure BoldChkClick(Sender: TObject);
    procedure BackgroundChkClick(Sender: TObject);
    procedure ForegroundCboChange(Sender: TObject);
    procedure BackgroundCboChange(Sender: TObject);
  private
    FProfile: TEditorProfile;
    InChanging: boolean;
    procedure ApplyCategory;
    procedure RetrieveElement;
    procedure ApplyElement;
    procedure GetData;
    procedure PutData;
  public
    function Execute(Profile: TEditorProfile; Select: string): boolean;
  end;

implementation

{$R *.lfm}

uses
  EditorEngine, SynEditTypes;

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
    FProfile := Profile;
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

    AttributeCbo.Clear;
    for i := 0 to Profile.Attributes.Count - 1 do
    begin
      AttributeCbo.Items.AddObject(Profile.Attributes.Items[i].Name, Profile.Attributes.Items[i]);
    end;
    AttributeCbo.ItemIndex := 0;

    ApplyCategory;
    //Get Data
    GetData;
    //Show the form
    Result := ShowModal = mrOk;
    //PutData
    if Result then
    begin
      PutData;
      //TODO apply to profile.attributes
    end;
  end
  else
    Result := False;
end;

procedure TEditorOptionsForm.GetData;
begin
  DefForegroundCbo.Selected := FProfile.ForegroundColor;
  DefBackgroundCbo.Selected := FProfile.BackgroundColor;
  //Gutter
  GutterVisibleChk.Checked := FProfile.Gutter.Visible;
  GutterAutosizeChk.Checked := FProfile.Gutter.AutoSize;
  GutterForecolorCbo.Selected := FProfile.Gutter.Forecolor;
  GutterBackcolorCbo.Selected := FProfile.Gutter.Backcolor;
  SeparatorColorCbo.Selected := FProfile.Gutter.SeparatorColor;
  ShowSeparatorChk.Checked := FProfile.Gutter.ShowSeparator;

  SavedColorCbo.Selected := FProfile.Gutter.Savedcolor;
  UnsavedColorCbo.Selected := FProfile.Gutter.Unsavedcolor;
  ShowModifiedLinesChk.Checked := FProfile.Gutter.ShowModifiedLines;

  GutterShowLineNumbersChk.Checked := FProfile.Gutter.ShowLineNumbers;
  GutterShowLeaderZerosChk.Checked := FProfile.Gutter.LeadingZeros;
  GutterStartAtZeroChk.Checked := FProfile.Gutter.ZeroStart;

  //Right Edge
  RightEdgeEdit.Text := IntToStr(FProfile.RightEdge);
  RightEdgeColorCbo.Selected := FProfile.RightEdgeColor;
  //Line Spacing
  LineSpacingEdit.Text := IntToStr(FProfile.ExtraLineSpacing);
  TabWidthEdit.Text := IntToStr(FProfile.TabWidth);
  //Font
  SampleEdit.Font.Name := FProfile.FontName;
  SampleEdit.Font.Size := FProfile.FontSize;

  NoAntialiasingChk.Checked := FProfile.FontNoAntialiasing;
  if FProfile.FontNoAntialiasing then
    SampleEdit.Font.Quality := fqNonAntialiased
  else
    SampleEdit.Font.Quality := fqDefault;

  FontLbl.Caption := FontLbl.Font.Name + ' ' + IntToStr(FontLbl.Font.Size) + ' pt';
  //Options
  AutoIndentChk.Checked := eoAutoIndent in FProfile.Options;
  TabIndentChk.Checked := eoTabIndent in FProfile.Options;
  AutoSizeMaxWidthChk.Checked := eoAutoSizeMaxScrollWidth in FProfile.Options;
  DragAndDropEditingChk.Checked := eoDragDropEditing in FProfile.Options;
  SmartTabsChk.Checked := eoSmartTabs in FProfile.Options;
  AltSetsColumnModeChk.Checked := eoAltSetsColumnMode in FProfile.Options;
  HalfPageScrollChk.Checked := eoHalfPageScroll in FProfile.Options;
  ScrollByOneLessChk.Checked := eoScrollByOneLess in FProfile.Options;
  ScrollPastEOFChk.Checked := eoScrollPastEof in FProfile.Options;
  ScrollPastEOLChk.Checked := eoScrollPastEol in FProfile.Options;
  ShowScrollHintChk.Checked := eoShowScrollHint in FProfile.Options;
  TabsToSpacesChk.Checked := eoTabsToSpaces in FProfile.Options;
  TrimTrailingSpacesChk.Checked := eoTrimTrailingSpaces in FProfile.Options;
  KeepCaretXChk.Checked := eoKeepCaretX in FProfile.Options;
  SmartTabDeleteChk.Checked := eoSmartTabDelete in FProfile.Options;
  RightMouseMovesChk.Checked := eoRightMouseMovesCursor in FProfile.Options;
  EnhanceHomeKeyChk.Checked := eoEnhanceHomeKey in FProfile.Options;
  EnhanceEndKeyChk.Checked := eoEnhanceEndKey in FProfile.ExtOptions;
  GroupUndoChk.Checked := eoGroupUndo in FProfile.Options;
  DisableScrollArrowsChk.Checked := eoDisableScrollArrows in FProfile.Options;
  HideShowScrollbarsChk.Checked := eoHideShowScrollbars in FProfile.Options;
  ShowSpecialCharsChk.Checked := eoShowSpecialChars in FProfile.Options;
  BracketHighlightChk.Checked := eoBracketHighlight in FProfile.Options;
  //Caret
  InsertModeChk.Checked := FProfile.InsertMode;
  CodeFoldingChk.Checked := FProfile.CodeFolding;
  InsertCaretCbo.ItemIndex := Ord(FProfile.InsertCaret);
  OverwriteCaretCbo.ItemIndex := Ord(FProfile.OverwriteCaret);
end;

procedure TEditorOptionsForm.PutData;
var
  vOptions: TSynEditorOptions;
  vExtOptions: TSynEditorOptions2;

  procedure SetFlag(aOption: TSynEditorOption; aValue: boolean);
  begin
    if aValue then
      Include(vOptions, aOption)
    else
      Exclude(vOptions, aOption);
  end;

  procedure SetExtFlag(aOption: TSynEditorOption2; aValue: boolean);
  begin
    if aValue then
      Include(vExtOptions, aOption)
    else
      Exclude(vExtOptions, aOption);
  end;

begin
  FProfile.ForegroundColor := DefForegroundCbo.Selected;
  FProfile.BackgroundColor := DefBackgroundCbo.Selected;
  //Gutter
  FProfile.Gutter.Visible := GutterVisibleChk.Checked;
  FProfile.Gutter.AutoSize := GutterAutosizeChk.Checked;
  FProfile.Gutter.Forecolor := GutterForecolorCbo.Selected;
  FProfile.Gutter.Backcolor := GutterBackcolorCbo.Selected;
  FProfile.Gutter.SeparatorColor := SeparatorColorCbo.Selected;
  FProfile.Gutter.ShowSeparator := ShowSeparatorChk.Checked;
  FProfile.Gutter.ShowLineNumbers := GutterShowLineNumbersChk.Checked;

  FProfile.Gutter.Savedcolor := SavedColorCbo.Selected;
  FProfile.Gutter.Unsavedcolor := UnsavedColorCbo.Selected;
  FProfile.Gutter.ShowModifiedLines := ShowModifiedLinesChk.Checked;

  FProfile.Gutter.LeadingZeros := GutterShowLeaderZerosChk.Checked;
  FProfile.Gutter.ZeroStart := GutterStartAtZeroChk.Checked;
  //Right Edge
  FProfile.RightEdge := StrToIntDef(RightEdgeEdit.Text, 80);
  FProfile.RightEdgeColor := RightEdgeColorCbo.Selected;
  //Line Spacing
  FProfile.ExtraLineSpacing := StrToIntDef(LineSpacingEdit.Text, 0);
  FProfile.TabWidth := StrToIntDef(TabWidthEdit.Text, 8);
  //Font
  FProfile.FontName := SampleEdit.Font.Name;
  FProfile.FontSize := SampleEdit.Font.Size;
  FProfile.FontNoAntialiasing := SampleEdit.Font.Quality = fqNonAntialiased;
  //Options
  vOptions := FProfile.Options; //Keep old values for unsupported options
  vExtOptions := FProfile.ExtOptions;
  SetFlag(eoAutoIndent, AutoIndentChk.Checked);
  SetFlag(eoTabIndent, TabIndentChk.Checked);
  SetFlag(eoAutoSizeMaxScrollWidth, AutoSizeMaxWidthChk.Checked);
  SetFlag(eoDragDropEditing, DragAndDropEditingChk.Checked);
  SetFlag(eoSmartTabs, SmartTabsChk.Checked);
  SetFlag(eoAltSetsColumnMode, AltSetsColumnModeChk.Checked);
  SetFlag(eoHalfPageScroll, HalfPageScrollChk.Checked);
  SetFlag(eoScrollByOneLess, ScrollByOneLessChk.Checked);
  SetFlag(eoScrollPastEof, ScrollPastEOFChk.Checked);
  SetFlag(eoScrollPastEol, ScrollPastEOLChk.Checked);
  SetFlag(eoShowScrollHint, ShowScrollHintChk.Checked);
  SetFlag(eoTabsToSpaces, TabsToSpacesChk.Checked);
  SetFlag(eoTrimTrailingSpaces, TrimTrailingSpacesChk.Checked);
  SetFlag(eoKeepCaretX, KeepCaretXChk.Checked);
  SetFlag(eoSmartTabDelete, SmartTabDeleteChk.Checked);
  SetFlag(eoRightMouseMovesCursor, RightMouseMovesChk.Checked);
  SetFlag(eoEnhanceHomeKey, EnhanceHomeKeyChk.Checked);
  SetExtFlag(eoEnhanceEndKey, EnhanceEndKeyChk.Checked);
  SetFlag(eoGroupUndo, GroupUndoChk.Checked);
  SetFlag(eoDisableScrollArrows, DisableScrollArrowsChk.Checked);
  SetFlag(eoHideShowScrollbars, HideShowScrollbarsChk.Checked);
  SetFlag(eoShowSpecialChars, ShowSpecialCharsChk.Checked);
  SetFlag(eoBracketHighlight, BracketHighlightChk.Checked);
  FProfile.Options := vOptions;
  FProfile.ExtOptions := vExtOptions;
  //Caret
  FProfile.InsertMode := InsertModeChk.Checked;
  FProfile.CodeFolding := CodeFoldingChk.Checked;
  FProfile.InsertCaret := TSynEditCaretType(InsertCaretCbo.ItemIndex);
  FProfile.OverwriteCaret := TSynEditCaretType(OverwriteCaretCbo.ItemIndex);
end;

procedure TEditorOptionsForm.FormCreate(Sender: TObject);
begin
  InChanging := False;
end;

procedure TEditorOptionsForm.AttributeCboSelect(Sender: TObject);
begin
  RetrieveElement;
end;

procedure TEditorOptionsForm.BackgroundCboSelect(Sender: TObject);
begin
  if not InChanging then
    BackgroundChk.Checked := True;
  ApplyElement;
end;

procedure TEditorOptionsForm.NoAntialiasingChkChange(Sender: TObject);
begin
  if NoAntialiasingChk.Checked then
  SampleEdit.Font.Quality := fqNonAntialiased
  else
    SampleEdit.Font.Quality := fqDefault;
end;

procedure TEditorOptionsForm.DefaultBackgroundCboSelect(Sender: TObject);
begin
  ApplyElement;
end;

procedure TEditorOptionsForm.DefaultForegroundCboSelect(Sender: TObject);
begin
  ApplyElement;
end;

procedure TEditorOptionsForm.ForegroundCboSelect(Sender: TObject);
begin
  if not InChanging then
    ForegroundChk.Checked := True;
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
    FontLbl.Caption := FontLbl.Font.Name + ' ' + IntToStr(FontLbl.Font.Size) + ' pt';
    ApplyElement;
  end;
end;

procedure TEditorOptionsForm.FormShow(Sender: TObject);
begin
  PageControl.TabIndex := 0;
end;

procedure TEditorOptionsForm.CategoryCboSelect(Sender: TObject);
begin
  ApplyCategory;
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
  FProfile.Reset;
  InChanging := True;
  try
    GetData;
  finally
    InChanging := False;
  end;
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
  Attributes := nil;
  if not SampleEdit.GetHighlighterAttriAtRowCol(Point(p.x, p.y), s, Attributes) then
    Attributes := nil;
  if Attributes = nil then
    Attributes := SampleEdit.Highlighter.WhitespaceAttribute;
  if Attributes <> nil then
  begin
    aFileCategory := TFileCategory(CategoryCbo.Items.Objects[CategoryCbo.ItemIndex]);
    M := aFileCategory.Mapper.Find(Attributes.StoredName);
    if M <> nil then
      G := FProfile.Attributes.Find(M.ToName)
    else
      G := nil;

    if G <> nil then
      AttributeCbo.ItemIndex := AttributeCbo.Items.IndexOf(G.Name)
    else
      AttributeCbo.ItemIndex := -1;
  end
  else
    AttributeCbo.ItemIndex := -1;
  RetrieveElement;
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
  aFontStyle: TFontStyles;
  aFileCategory: TFileCategory;
  aGlobalAttribute: TGlobalAttribute;
begin
  if not InChanging and (AttributeCbo.ItemIndex >= 0) then
  begin
    InChanging := True;
    try
      aFileCategory := TFileCategory(CategoryCbo.Items.Objects[CategoryCbo.ItemIndex]);
      aGlobalAttribute := (AttributeCbo.Items.Objects[AttributeCbo.ItemIndex] as TGlobalAttribute);

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
      if ItalicChk.Checked then
        aFontStyle := aFontStyle + [fsItalic];
      if UnderlineChk.Checked then
        aFontStyle := aFontStyle + [fsUnderline];

      aGlobalAttribute.Style := aFontStyle;

      aFileCategory.Apply(SampleEdit.Highlighter, FProfile.Attributes);
    finally
      InChanging := False;
    end;
  end;
end;

procedure TEditorOptionsForm.GroupCboClick(Sender: TObject);
begin

end;

procedure TEditorOptionsForm.ApplyCategory;
var
  i: integer;
  aFileCategory: TFileCategory;
begin
  aFileCategory := TFileCategory(CategoryCbo.Items.Objects[CategoryCbo.ItemIndex]);
  if (SampleEdit.Highlighter = nil) or (SampleEdit.Highlighter.ClassType <> aFileCategory.Highlighter.ClassType) then
  begin
    SampleEdit.Highlighter.Free;
    SampleEdit.Highlighter := aFileCategory.CreateHighlighter;
    SampleEdit.Text := SampleEdit.Highlighter.SampleSource;
  end;
  SampleEdit.Color := SampleEdit.Highlighter.WhitespaceAttribute.Background;
  SampleEdit.Font.Color := SampleEdit.Highlighter.WhitespaceAttribute.Foreground;
  RetrieveElement;
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

procedure TEditorOptionsForm.ForegroundCboChange(Sender: TObject);
begin
end;

procedure TEditorOptionsForm.BackgroundCboChange(Sender: TObject);
begin
end;

end.
