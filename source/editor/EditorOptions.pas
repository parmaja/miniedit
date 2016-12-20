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
  SynEditHighlighter, SynEditMiscClasses, SynEditKeyCmds, Classes, SysUtils, typinfo,
  EditorProfiles, SynGutterBase, SynEditMarks, mnStreams, Types;

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
    IndentModeCbo: TComboBox;
    Label10: TLabel;
    Label3: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LineSpacingEdit: TEdit;
    NoAntialiasingChk: TCheckBox;
    Bevel1: TBevel;
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
    SmartTabDeleteChk: TCheckBox;
    SmartTabsChk: TCheckBox;
    TabIndentChk: TCheckBox;
    TabWidthEdit: TEdit;
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
    WordWrapChk: TCheckBox;
    procedure BackgroundCboChange(Sender: TObject);
    procedure ForegroundCboChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure NoAntialiasingChkChange(Sender: TObject);
    procedure DefaultBackgroundCboSelect(Sender: TObject);
    procedure DefaultForegroundCboSelect(Sender: TObject);
    procedure AttributeCboSelect(Sender: TObject);
    procedure FontBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CategoryCboSelect(Sender: TObject);
    procedure KeyListEditing(Sender: TObject; Item: TListItem; var AllowEdit: boolean);
    procedure OkBtnClick(Sender: TObject);
    procedure ResetBtnClick(Sender: TObject);
    procedure RevertBtnClick(Sender: TObject);

    procedure SampleEditGutterClick(Sender: TObject; X, Y, Line: integer; mark: TSynEditMark);
    procedure SampleEditMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure BoldChkClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
  private
    FProfile: TEditorProfile;
    InChanging: boolean;
    procedure ChangeEdit;
    procedure ApplyAttribute;
    procedure RetrieveAttribute;

    procedure Apply;
    procedure Retrieve;
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

procedure TEditorOptionsForm.AttributeCboSelect(Sender: TObject);
begin
  RetrieveAttribute;
end;

procedure TEditorOptionsForm.NoAntialiasingChkChange(Sender: TObject);
begin
  if not InChanging then
  begin
    FProfile.Attributes.FontNoAntialiasing := NoAntialiasingChk.Checked;
    ChangeEdit;
  end;
end;

procedure TEditorOptionsForm.LoadBtnClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    FProfile.Attributes.Reset;
    XMLReadObjectFile(FProfile.Attributes, OpenDialog.FileName);
    Retrieve;
  end;
end;

procedure TEditorOptionsForm.ForegroundCboChange(Sender: TObject);
begin
  if not InChanging then
  begin
    if ForegroundChk.Enabled then
      ForegroundChk.Checked := True;
    ApplyAttribute;
  end;
end;

procedure TEditorOptionsForm.FormCreate(Sender: TObject);
begin
  EnumIndentMode(IndentModeCbo.Items);
end;

procedure TEditorOptionsForm.BackgroundCboChange(Sender: TObject);
begin
  if not InChanging then
  begin
    if BackgroundChk.Enabled then
      BackgroundChk.Checked := True;
    ApplyAttribute;
  end;
end;

procedure TEditorOptionsForm.DefaultBackgroundCboSelect(Sender: TObject);
begin
  ApplyAttribute;
end;

procedure TEditorOptionsForm.DefaultForegroundCboSelect(Sender: TObject);
begin
  ApplyAttribute;
end;

procedure TEditorOptionsForm.FontBtnClick(Sender: TObject);
begin
  FontDialog.Font.Name := FProfile.Attributes.FontName;
  FontDialog.Font.Size := FProfile.Attributes.FontSize;
  FontDialog.Options := FontDialog.Options - [fdNoStyleSel];
  if FontDialog.Execute then
  begin
    FProfile.Attributes.FontName := FontDialog.Font.Name;
    FProfile.Attributes.FontSize := FontDialog.Font.Size;
    ChangeEdit;
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

procedure TEditorOptionsForm.KeyListEditing(Sender: TObject; Item: TListItem; var AllowEdit: boolean);
begin
  AllowEdit := False;
end;

procedure TEditorOptionsForm.OkBtnClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TEditorOptionsForm.ResetBtnClick(Sender: TObject);
begin
  InChanging := True;
  try
    FProfile.Attributes.Revert;
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
  G: TGlobalAttribute;
begin
  G := FProfile.Attributes.Find(attGutter);
  if G = nil then
    G := FProfile.Attributes.Default;
  AttributeCbo.ItemIndex := G.Index;
  RetrieveAttribute;
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
        G := FProfile.Attributes.Find(M.AttType)
      else
        G := nil;

      if G = nil then
        G := FProfile.Attributes.Default;

      AttributeCbo.ItemIndex := G.Index;
    end
    else
      AttributeCbo.ItemIndex := -1;
    RetrieveAttribute;
  end;
end;

procedure TEditorOptionsForm.RetrieveAttribute;
var
  aGlobalAttribute: TGlobalAttribute;
begin
  if not InChanging and (AttributeCbo.ItemIndex >= 0) then
  begin
    aGlobalAttribute := (AttributeCbo.Items.Objects[AttributeCbo.ItemIndex] as TGlobalAttribute);
    InChanging := True;
    try
      ForegroundCbo.Selected := aGlobalAttribute.ForeColor;
      ForegroundCbo.Refresh;//bug when custom and then custom colors
      BackgroundCbo.Selected := aGlobalAttribute.BackColor;
      BackgroundCbo.Refresh;//bug when custom and then custom colors

      //ForegroundCbo.CustomColor := ;

      if aGlobalAttribute.AttType = attDefault then //Default is required
      begin
        ForegroundChk.Enabled := False;
        BackgroundChk.Enabled := False;

        ForegroundChk.Checked := True;
        BackgroundChk.Checked := True;
      end
      else
      begin
        ForegroundChk.Enabled := True;
        BackgroundChk.Enabled := True;

        ForegroundChk.Checked := not (gaoDefaultForeground in aGlobalAttribute.Options);
        BackgroundChk.Checked :=  not (gaoDefaultBackground in aGlobalAttribute.Options);
      end;

    finally
      InChanging := False;
    end;
  end;
end;

procedure TEditorOptionsForm.ApplyAttribute;
var
  aGlobalAttribute: TGlobalAttribute;
begin
  if not InChanging and (AttributeCbo.ItemIndex >= 0) then
  begin
    aGlobalAttribute := (AttributeCbo.Items.Objects[AttributeCbo.ItemIndex] as TGlobalAttribute);

    //Copy some from TGutterOptions.AssignTo(Dest: TPersistent);

    aGlobalAttribute.ForeColor := ForegroundCbo.Selected;

    if ForegroundChk.Checked then
      aGlobalAttribute.Options := aGlobalAttribute.Options - [gaoDefaultForeground]
    else
      aGlobalAttribute.Options := aGlobalAttribute.Options + [gaoDefaultForeground];

    aGlobalAttribute.BackColor := BackgroundCbo.Selected;

    if BackgroundChk.Checked then
      aGlobalAttribute.Options := aGlobalAttribute.Options - [gaoDefaultBackground]
    else
      aGlobalAttribute.Options := aGlobalAttribute.Options + [gaoDefaultBackground];

    ChangeEdit;
  end;
end;

procedure TEditorOptionsForm.Retrieve;
begin
  InChanging := True;
  try
    CodeFoldingChk.Checked := FProfile.Attributes.CodeFolding;

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
    BracketHighlightChk.Checked := eoBracketHighlight in FProfile.EditorOptions;
    //Can be override by project options
    TabWidthEdit.Text := IntToStr(FProfile.TabWidth);
    IndentModeCbo.ItemIndex := Ord(FProfile.IndentMode);
  finally
    InChanging := False;
  end;
  RetrieveAttribute;
  ChangeEdit;
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
  SetFlag(eoBracketHighlight, BracketHighlightChk.Checked);

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
  FProfile.IndentMode := TIndentMode(IndentModeCbo.ItemIndex);

  //Font
  FProfile.Attributes.FontNoAntialiasing := NoAntialiasingChk.Checked;
  FProfile.Attributes.Assign(FProfile.Attributes);
  ApplyAttribute;
end;

procedure TEditorOptionsForm.BoldChkClick(Sender: TObject);
begin
  ApplyAttribute;
end;

procedure TEditorOptionsForm.SaveBtnClick(Sender: TObject);
{$ifdef debug}
var
  i: Integer;
  s: string;
  aName: string;
  Stream: TFileStream;
  function GetStyle(op: TGlobalAttributeOptions): string;
    procedure Add(ss: string);
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + ss;
    end;
  begin
    Result := '';
{    if gaoBold in op then
      Add('gaoBold');}
  end;
  {$endif}
begin
  if SaveDialog.Execute then
  begin
    Apply;
    XMLWriteObjectFile(FProfile.Attributes, SaveDialog.FileName);
    {$ifdef debug}
    Stream := TFileStream.Create(SaveDialog.FileName+'-code.pas', fmCreate);
    try
      for i := 0 to FProfile.Attributes.Count -1 do
      begin
        aName := GetEnumName(typeinfo(TAttributeType), ord(FProfile.Attributes[i].AttType));
        //v := Integer(FProfile.Attributes[i].Style);
        s := '  Add(F'+Copy(aName, 4, MaxInt) + ', ' +
          aName + ', '''+FProfile.Attributes[i].Title+''', ' +
          ColorToString(FProfile.Attributes[i].ForeColor)+', '+ColorToString(FProfile.Attributes[i].BackColor)+', '+
          //'['+SetToString(TypeInfo(TFontStyles), v)+']'+

          '['+GetStyle(FProfile.Attributes[i].Options)+']'+
          ');'+#13#10;
         Stream.WriteBuffer(Pointer(s)^, length(s));
      end;
    finally
      Stream.Free;
    end;
    {$endif}
  end;
end;

procedure TEditorOptionsForm.ChangeEdit;
var
  i: integer;
  aFileCategory: TFileCategory;
  sp: TSynGutterSeparator;
begin
  FProfile.Attributes.Correct;
  aFileCategory := TFileCategory(CategoryCbo.Items.Objects[CategoryCbo.ItemIndex]);

  if (SampleEdit.Highlighter = nil) or (SampleEdit.Highlighter.ClassType <> aFileCategory.Highlighter.ClassType) then
  begin
    SampleEdit.Highlighter.Free;
    SampleEdit.Highlighter := aFileCategory.CreateHighlighter;
    SampleEdit.Text := SampleEdit.Highlighter.SampleSource;
  end;

  aFileCategory.Apply(SampleEdit.Highlighter, FProfile.Attributes);

  if SampleEdit.Highlighter <> nil then //remove Divider
    for i := 0 to SampleEdit.Highlighter.DividerDrawConfigCount - 1 do
      SampleEdit.Highlighter.DividerDrawConfig[i].MaxDrawDepth := 0;

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

  FontLbl.Caption := FProfile.Attributes.FontName + ' ' + IntToStr(FProfile.Attributes.FontSize) + ' pt';
  SampleEdit.Font.Name := FProfile.Attributes.FontName;
  SampleEdit.Font.Size := FProfile.Attributes.FontSize;
  SampleEdit.Font.Color := FProfile.Attributes.Default.Foreground;
  SampleEdit.Color := FProfile.Attributes.Default.Background;
  SampleEdit.SelectedColor.Foreground := FProfile.Attributes.Selected.Foreground;
  SampleEdit.SelectedColor.Background := FProfile.Attributes.Selected.Background;
  SampleEdit.BracketMatchColor.Foreground := FProfile.Attributes.Selected.Foreground;
  SampleEdit.BracketMatchColor.Background := FProfile.Attributes.Selected.Background;

  SampleEdit.MarkupManager.MarkupByClass[TSynEditMarkupWordGroup].MarkupInfo.FrameColor := FProfile.Attributes.Selected.Background;

  if FProfile.Attributes.FontNoAntialiasing then
    SampleEdit.Font.Quality := fqNonAntialiased
  else
    SampleEdit.Font.Quality := fqDefault;

  FProfile.AssignTo(SampleEdit);
end;

end.
