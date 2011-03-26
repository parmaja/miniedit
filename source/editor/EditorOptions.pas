unit EditorOptions;
{$mode delphi}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  Windows, Messages, Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls,
  CommCtrl, Registry, ExtCtrls, Buttons, ImgList, Menus, ColorBox, SynEdit,
  SynEditHighlighter, SynEditMiscClasses, SynEditKeyCmds, Classes, SysUtils,
  EditorProfiles;

type
  TSynEditorOptionsUserCommand = procedure(AUserCommand: Integer;
    var ADescription: string) of object;

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
    GutterFontChk: TCheckBox;
    GutterFontBtn: TButton;
    ScrollHintFollowsChk: TCheckBox;
    GroupUndoChk: TCheckBox;
    SmartTabDeleteChk: TCheckBox;
    GutterFontDisplayPnl: TPanel;
    GutterFontLbl: TLabel;
    EnhanceHomeKeyChk: TCheckBox;
    HideShowScrollbarsChk: TCheckBox;
    DisableScrollArrowsChk: TCheckBox;
    ShowSpecialCharsChk: TCheckBox;
    ColorTab: TTabSheet;
    SampleEdit: TSynEdit;
    Label11: TLabel;
    EditorStyleGrp: TGroupBox;
    BoldChk: TCheckBox;
    ItalicChk: TCheckBox;
    UnderlineChk: TCheckBox;
    BackgroundCbo: TColorBox;
    ForegroundCbo: TColorBox;
    EditorFontGrp: TGroupBox;
    FontBtn: TButton;
    Panel3: TPanel;
    FontLbl: TLabel;
    ElementCbo: TComboBox;
    GutterColorCbo: TColorBox;
    RightEdgeColorCbo: TColorBox;
    BackgroundChk: TCheckBox;
    ForegroundChk: TCheckBox;
    RightMouseMovesChk: TCheckBox;
    InsertModeChk: TCheckBox;
    TabIndentChk: TCheckBox;
    ResetBtn: TButton;
    Label12: TLabel;
    GroupCbo: TComboBox;
    WordWrapChk: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FontBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure KeyListEditing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure OkBtnClick(Sender: TObject);
    procedure GutterFontBtnClick(Sender: TObject);
    procedure GutterFontChkClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure ResetBtnClick(Sender: TObject);
    procedure SampleEditMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ElementCboClick(Sender: TObject);
    procedure GroupCboClick(Sender: TObject);
    procedure BoldChkClick(Sender: TObject);
    procedure BackgroundChkClick(Sender: TObject);
    procedure ForegroundCboChange(Sender: TObject);
    procedure BackgroundCboChange(Sender: TObject);
  private
    FProfile: TEditorProfile;
    InChanging: Boolean;
    procedure ApplyGroup;
    procedure RetrieveElement;
    procedure ApplyElement;
    procedure GetData;
    procedure PutData;
  public
    function Execute(Profile: TEditorProfile; Highlighters: TList): Boolean;
  end;

implementation

{$R *.lfm}

uses
  SynEditTypes;

{ TEditorOptionsForm }

function TEditorOptionsForm.Execute(Profile: TEditorProfile; Highlighters: TList): Boolean;
var
  i: Integer;
  aHighlighter: TSynCustomHighlighter;
begin
  if (Profile <> nil) then
  begin
    FProfile := Profile;
    for i := 0 to Highlighters.Count - 1 do
    begin
      aHighlighter := TSynCustomHighlighterClass(Highlighters[i]).Create(Self);
      GroupCbo.Items.AddObject(aHighlighter.GetLanguageName, aHighlighter);
      Profile.Highlighters.AssignTo(aHighlighter);
    end;
    GroupCbo.ItemIndex := 0;
    ApplyGroup;
    //Get Data
    GetData;
    //Show the form
    Result := Showmodal = mrOk;
    //PutData
    if Result then
    begin
      PutData;
      Profile.Highlighters.Clear;
      for i := 0 to GroupCbo.Items.Count - 1 do
        Profile.Highlighters.Assign(TSynCustomHighlighter(GroupCbo.Items.Objects[i]));
    end;
  end
  else
    Result := False;
end;


procedure TEditorOptionsForm.GetData;
begin
  //Gutter
  GutterVisibleChk.Checked := FProfile.Gutter.Visible;
  GutterAutosizeChk.Checked := FProfile.Gutter.AutoSize;
  GutterColorCbo.Selected := FProfile.Gutter.Color;
  {GutterShowLineNumbersChk.Checked := FProfile.Gutter.ShowLineNumbers;
  GutterShowLeaderZerosChk.Checked := FProfile.Gutter.LeadingZeros;
  GutterStartAtZeroChk.Checked := FProfile.Gutter.ZeroStart;
  GutterFontChk.Checked := FProfile.Gutter.UseFontStyle;
  GutterFontLbl.Font.Assign(FProfile.Gutter.Font);
  GutterFontLbl.Caption := GutterFontLbl.Font.Name + ' ' + IntToStr(GutterFontLbl.Font.Size) + 'pt';
  }
  //Right Edge
  RightEdgeEdit.Text := IntToStr(FProfile.RightEdge);
  RightEdgeColorCbo.Selected := FProfile.RightEdgeColor;
  //Line Spacing
  LineSpacingEdit.Text := IntToStr(FProfile.ExtraLineSpacing);
  TabWidthEdit.Text := IntToStr(FProfile.TabWidth);
  //Font
  FontLbl.Font.Assign(FProfile.Font);
  FontLbl.Caption := FontLbl.Font.Name + ' ' + IntToStr(FontLbl.Font.Size) + 'pt';
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
  //Caret
  InsertModeChk.Checked := FProfile.InsertMode;
  InsertCaretCbo.ItemIndex := ord(FProfile.InsertCaret);
  OverwriteCaretCbo.ItemIndex := ord(FProfile.OverwriteCaret);
end;

procedure TEditorOptionsForm.PutData;
var
  vOptions: TSynEditorOptions;
  vExtOptions: TSynEditorOptions2;

  procedure SetFlag(aOption: TSynEditorOption; aValue: Boolean);
  begin
    if aValue then
      Include(vOptions, aOption)
    else
      Exclude(vOptions, aOption);
  end;

  procedure SetExtFlag(aOption: TSynEditorOption2; aValue: Boolean);
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
  FProfile.Gutter.Color := GutterColorCbo.Selected;
  {FProfile.Gutter.ShowLineNumbers := GutterShowLineNumbersChk.Checked;
  FProfile.Gutter.LeadingZeros := GutterShowLeaderZerosChk.Checked;
  FProfile.Gutter.UseFontStyle := GutterFontChk.Checked;
  FProfile.Gutter.Font.Assign(GutterFontLbl.Font);
  FProfile.Gutter.ZeroStart := GutterStartAtZeroChk.Checked;}
  //Right Edge
  FProfile.RightEdge := StrToIntDef(RightEdgeEdit.Text, 80);
  FProfile.RightEdgeColor := RightEdgeColorCbo.Selected;
  //Line Spacing
  FProfile.ExtraLineSpacing := StrToIntDef(LineSpacingEdit.Text, 0);
  FProfile.TabWidth := StrToIntDef(TabWidthEdit.Text, 8);
  //Font
  FProfile.Font.Assign(FontLbl.Font);
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
  FProfile.Options := vOptions;
  FProfile.ExtOptions := vExtOptions;
  //Caret
  FProfile.InsertMode := InsertModeChk.Checked;
  FProfile.InsertCaret := TSynEditCaretType(InsertCaretCbo.ItemIndex);
  FProfile.OverwriteCaret := TSynEditCaretType(OverwriteCaretCbo.ItemIndex);
end;

procedure TEditorOptionsForm.FormCreate(Sender: TObject);
begin
  InChanging := False;
end;

procedure TEditorOptionsForm.FontBtnClick(Sender: TObject);
begin
  FontDialog.Font.Assign(FontLbl.Font);
  if FontDialog.Execute then
  begin
    FontLbl.Font.Assign(FontDialog.Font);
    FontLbl.Caption := FontLbl.Font.Name;
    FontLbl.Caption := FontLbl.Font.Name + ' ' + IntToStr(FontLbl.Font.Size) + 'pt';
    ApplyElement;
  end;
end;

procedure TEditorOptionsForm.FormShow(Sender: TObject);
begin
  PageControl.TabIndex := 0;
end;

procedure TEditorOptionsForm.KeyListEditing(Sender: TObject;
  Item: TListItem; var AllowEdit: Boolean);
begin
  AllowEdit := False;
end;

procedure TEditorOptionsForm.OkBtnClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TEditorOptionsForm.GutterFontBtnClick(Sender: TObject);
begin
  FontDialog.Font.Assign(GutterFontLbl.Font);
  if FontDialog.Execute then
  begin
    GutterFontLbl.Font.Assign(FontDialog.Font);
    GutterFontLbl.Caption := GutterFontLbl.Font.Name + ' ' + IntToStr(GutterFontLbl.Font.Size) + 'pt';
  end;
end;

procedure TEditorOptionsForm.GutterFontChkClick(Sender: TObject);
begin
  GutterFontLbl.Enabled := GutterFontChk.Checked;
  GutterFontLbl.Enabled := GutterFontChk.Checked;
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

procedure TEditorOptionsForm.SampleEditMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Attributes: TSynHighlighterAttributes;
  p: TPoint;
  s: string;
begin
  p := SampleEdit.PixelsToRowColumn(Point(X, Y));
  if SampleEdit.GetHighlighterAttriAtRowCol(Point(p.x, p.y), s, Attributes) then
  begin
    if Attributes <> nil then
      ElementCbo.ItemIndex := ElementCbo.Items.IndexOf(Attributes.Name)
    else
      ElementCbo.ItemIndex := -1;
    RetrieveElement;
  end;
end;

procedure TEditorOptionsForm.ElementCboClick(Sender: TObject);
begin
  RetrieveElement;
end;

procedure TEditorOptionsForm.RetrieveElement;
var
  aColor: TColor;
begin
  if not InChanging and (SampleEdit.Highlighter <> nil) and (ElementCbo.ItemIndex>=0) then
  begin
    InChanging := True;
    try
      with SampleEdit.Highlighter do
      begin
        aColor := Attribute[ElementCbo.ItemIndex].Foreground;
        if aColor = clNone then
        begin
          ForegroundChk.Checked := False;
          ForegroundCbo.Selected := clBlack
        end
        else
        begin
          ForegroundChk.Checked := True;
          ForegroundCbo.Selected := aColor;
          ForegroundCbo.Refresh;//bug when custom and then custom colos
        end;

        aColor := Attribute[ElementCbo.ItemIndex].Background;
        if aColor = clNone then
        begin
          BackgroundChk.Checked := False;
          BackgroundCbo.Selected := clWindow
        end
        else
        begin
          BackgroundChk.Checked := True;
          BackgroundCbo.Selected := aColor;
          BackgroundCbo.Refresh;//bug when custom and then custom colos
        end;

        BoldChk.Checked := (fsBold in Attribute[ElementCbo.ItemIndex].Style);
        ItalicChk.Checked := (fsItalic in Attribute[ElementCbo.ItemIndex].Style);
        UnderlineChk.Checked := (fsUnderline in Attribute[ElementCbo.ItemIndex].Style);
      end;
    finally
      InChanging := False;
    end;
  end;
end;

procedure TEditorOptionsForm.ApplyElement;
var
  aFontStyle: TFontStyles;
begin
  if not InChanging and (SampleEdit.Highlighter <> nil) and (ElementCbo.ItemIndex>=0) then
  begin
    InChanging := True;
    try
      with SampleEdit.Highlighter do
      begin
        if ForegroundChk.Checked then
          Attribute[ElementCbo.ItemIndex].Foreground := ForegroundCbo.Selected
        else
          Attribute[ElementCbo.ItemIndex].Foreground := clNone;
          
        if BackgroundChk.Checked then
          Attribute[ElementCbo.ItemIndex].Background := BackgroundCbo.Selected
        else
          Attribute[ElementCbo.ItemIndex].Background := clNone;

        aFontStyle := [];
        if BoldChk.Checked then
          aFontStyle := aFontStyle + [fsBold];
        if ItalicChk.Checked then
          aFontStyle := aFontStyle + [fsItalic];
        if UnderlineChk.Checked then
          aFontStyle := aFontStyle + [fsUnderline];

        Attribute[ElementCbo.ItemIndex].Style := aFontStyle;
      end;
    finally
      InChanging := False;
    end;
  end;
end;

procedure TEditorOptionsForm.GroupCboClick(Sender: TObject);
begin
  ApplyGroup;
end;

procedure TEditorOptionsForm.ApplyGroup;
var
  i: Integer;
begin
  SampleEdit.Highlighter := TSynCustomHighlighter(GroupCbo.Items.Objects[GroupCbo.ItemIndex]);
  SampleEdit.Text := SampleEdit.Highlighter.SampleSource;
  ElementCbo.Clear;
  for i := 0 to SampleEdit.Highlighter.AttrCount - 1 do
    if SampleEdit.Highlighter.Attribute[i].Name <> '' then
      ElementCbo.Items.Add(SampleEdit.Highlighter.Attribute[i].Name);
  ElementCbo.ItemIndex := 0;
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
  if not InChanging then
    ForegroundChk.Checked := True;
  ApplyElement;
end;

procedure TEditorOptionsForm.BackgroundCboChange(Sender: TObject);
begin
  if not InChanging then
    BackgroundChk.Checked := True;
  ApplyElement;
end;

end.

