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
    EdgeVisibleChk: TCheckBox;
    NoAntialiasingChk: TCheckBox;
    Bevel1: TBevel;
    BracketHighlightChk: TCheckBox;
    BoldChk: TCheckBox;
    FontBtn: TButton;
    FontLbl: TLabel;
    CodeFoldingChk: TCheckBox;
    Label15: TLabel;
    Label7: TLabel;
    OpenDialog: TOpenDialog;
    ResetBtn: TButton;
    RevertBtn: TButton;
    SaveBtn: TButton;
    LoadBtn: TButton;
    SavedColorCbo: TColorBox;
    SaveDialog: TSaveDialog;
    UnsavedColorCbo: TColorBox;
    ShowSeparatorChk: TCheckBox;
    GutterForecolorCbo: TColorBox;
    ItalicChk: TCheckBox;
    Label5: TLabel;
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
    Label12: TLabel;
    CategoryCbo: TComboBox;
    UnderlineChk: TCheckBox;
    WordWrapChk: TCheckBox;
    procedure BackgroundCboChange(Sender: TObject);
    procedure BackgroundCboClick(Sender: TObject);
    procedure ForegroundCboChange(Sender: TObject);
    procedure ForegroundCboCloseUp(Sender: TObject);
    procedure ForegroundCboEditingDone(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
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
    procedure RevertBtnClick(Sender: TObject);
    procedure SampleEditMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure GroupCboClick(Sender: TObject);
    procedure BoldChkClick(Sender: TObject);
    procedure BackgroundChkClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
  private
    FProfile: TEditorProfile;
    FAttributes: TGlobalAttributes;
    InChanging: boolean;
    procedure ApplyElement;
    procedure RetrieveElement;
    procedure ApplyCategory;
    procedure GetData;
    procedure PutData;
  public
    function Execute(Profile: TEditorProfile; Select: string): boolean;
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
    FProfile := Profile;
    FAttributes := TGlobalAttributes.Create(nil);
    try
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

      for i := 0 to FAttributes.Count - 1 do
      begin
        AttributeCbo.Items.AddObject(FAttributes.Items[i].Title, FAttributes.Items[i]);
      end;
      AttributeCbo.ItemIndex := 0;

      //Get Data
      GetData;
      //Show the form
      Result := ShowModal = mrOk;
      //PutData
      if Result then
        PutData;
    finally
      FAttributes.Free;
    end;
  end
  else
    Result := False;
end;

procedure TEditorOptionsForm.GetData;
begin
  FAttributes.Assign(FProfile.Attributes);

  //Gutter
  GutterVisibleChk.Checked := FProfile.Gutter.Visible;
  GutterAutosizeChk.Checked := FProfile.Gutter.AutoSize;
  GutterForecolorCbo.Selected := FProfile.Gutter.Forecolor;
  GutterBackcolorCbo.Selected := FProfile.Gutter.Backcolor;
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

  FontLbl.Caption := SampleEdit.Font.Name + ' ' + IntToStr(SampleEdit.Font.Size) + ' pt';
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
  EdgeVisibleChk.Checked := not (eoHideRightMargin in FProfile.Options);
  //Caret
  InsertModeChk.Checked := FProfile.InsertMode;
  CodeFoldingChk.Checked := FProfile.CodeFolding;
  InsertCaretCbo.ItemIndex := Ord(FProfile.InsertCaret);
  OverwriteCaretCbo.ItemIndex := Ord(FProfile.OverwriteCaret);

  ApplyCategory;
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
  //Gutter
  FProfile.Gutter.Visible := GutterVisibleChk.Checked;
  FProfile.Gutter.AutoSize := GutterAutosizeChk.Checked;
  FProfile.Gutter.Forecolor := GutterForecolorCbo.Selected;
  FProfile.Gutter.Backcolor := GutterBackcolorCbo.Selected;
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
  SetFlag(eoHideRightMargin, not EdgeVisibleChk.Checked);
  FProfile.Options := vOptions;
  FProfile.ExtOptions := vExtOptions;
  //Caret
  FProfile.InsertMode := InsertModeChk.Checked;
  FProfile.CodeFolding := CodeFoldingChk.Checked;
  FProfile.InsertCaret := TSynEditCaretType(InsertCaretCbo.ItemIndex);
  FProfile.OverwriteCaret := TSynEditCaretType(OverwriteCaretCbo.ItemIndex);

  FProfile.Attributes.Assign(FAttributes);
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
end;

procedure TEditorOptionsForm.NoAntialiasingChkChange(Sender: TObject);
begin
  if NoAntialiasingChk.Checked then
  SampleEdit.Font.Quality := fqNonAntialiased
  else
    SampleEdit.Font.Quality := fqDefault;
end;

procedure TEditorOptionsForm.ForegroundCboEditingDone(Sender: TObject);
begin
end;

procedure TEditorOptionsForm.LoadBtnClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    XMLReadObjectFile(FAttributes, OpenDialog.FileName);
    ApplyCategory;
  end;
end;

procedure TEditorOptionsForm.ForegroundCboCloseUp(Sender: TObject);
begin

end;

procedure TEditorOptionsForm.ForegroundCboChange(Sender: TObject);
begin
  if not InChanging then
  begin
    ForegroundChk.Checked := True;
    ApplyElement;
  end;
end;

procedure TEditorOptionsForm.BackgroundCboClick(Sender: TObject);
begin
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

procedure TEditorOptionsForm.ForegroundCboSelect(Sender: TObject);
begin
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
  InChanging := True;
  try
    FAttributes.Reset;
  finally
    InChanging := False;
  end;
  ApplyCategory;
end;

procedure TEditorOptionsForm.RevertBtnClick(Sender: TObject);
begin
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
    s := Attributes.StoredName;
    M := aFileCategory.Mapper.Find(s);
    if M <> nil then
      G := FAttributes.Find(M.ToName)
    else
      G := FAttributes.Find(s);

    if G = nil then
      G := FAttributes.Whitespace;

    AttributeCbo.ItemIndex := G.Index;
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

      aFileCategory.Apply(SampleEdit.Highlighter, FAttributes);

      SampleEdit.Font.Color := FAttributes.Whitespace.Foreground;
      SampleEdit.Color := FAttributes.Whitespace.Background;
      SampleEdit.SelectedColor.Foreground := FAttributes.Selected.Foreground;
      SampleEdit.SelectedColor.Background := FAttributes.Selected.Background;
      SampleEdit.BracketMatchColor.Foreground := FAttributes.Selected.Foreground;
      SampleEdit.BracketMatchColor.Background := FAttributes.Selected.Background;

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
  aFileCategory.Apply(SampleEdit.Highlighter, FAttributes);
  SampleEdit.Font.Color := FAttributes.Whitespace.Foreground;
  SampleEdit.Color := FAttributes.Whitespace.Background;
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

procedure TEditorOptionsForm.SaveBtnClick(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    XMLWriteObjectFile(FAttributes, SaveDialog.FileName);
  end;
end;

end.
