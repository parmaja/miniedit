unit EditorOptions;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey 
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
    AutoUpdateFileChk: TCheckBox;
    CodeFoldingChk: TCheckBox;
    GroupUndoChk: TCheckBox;
    GutterAutosizeChk: TCheckBox;
    GutterGrp: TGroupBox;
    GutterShowLeaderZerosChk: TCheckBox;
    HalfPageScrollChk: TCheckBox;
    IndentModeCbo: TComboBox;
    Label10: TLabel;
    Label3: TLabel;
    Label9: TLabel;
    RevertBtn: TButton;
    OkBtn: TButton;
    CancelBtn: TButton;
    ScrollByOneLessChk: TCheckBox;
    ScrollHintFollowsChk: TCheckBox;
    ShowModifiedLinesChk: TCheckBox;
    ShowScrollHintChk: TCheckBox;
    ShowSeparatorChk: TCheckBox;
    AutoIndentOnPasteChk: TCheckBox;
    TabWidthEdit: TEdit;
    WordWrapChk: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure CategoryCboSelect(Sender: TObject);
    procedure KeyListEditing(Sender: TObject; Item: TListItem; var AllowEdit: boolean);
    procedure OkBtnClick(Sender: TObject);
    procedure ResetBtnClick(Sender: TObject);
    procedure RevertBtnClick(Sender: TObject);

  private
    FProfile: TEditorProfile;
    InChanging: boolean;
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
begin
  if (Profile <> nil) then
  begin
    FProfile := TEditorProfile.Create;
    try
      FProfile.Assign(Profile);

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
begin
  inherited;
end;

procedure TEditorOptionsForm.FormCreate(Sender: TObject);
begin
  EnumIndentMode(IndentModeCbo.Items);
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

procedure TEditorOptionsForm.Retrieve;
begin
  InChanging := True;
  try
    CodeFoldingChk.Checked := FProfile.Attributes.CodeFolding;

    //Gutter
    GutterAutosizeChk.Checked := FProfile.Attributes.GutterAutoSize;
    ShowSeparatorChk.Checked := FProfile.Attributes.GutterShowSeparator;

    ShowModifiedLinesChk.Checked := FProfile.Attributes.GutterShowModifiedLines;

    GutterShowLeaderZerosChk.Checked := FProfile.Attributes.GutterLeadingZeros;

    //Options
    AutoIndentChk.Checked := eoAutoIndent in FProfile.EditorOptions;
    AutoIndentOnPasteChk.Checked := eoAutoIndentOnPaste in FProfile.EditorOptions;

	HalfPageScrollChk.Checked := eoHalfPageScroll in FProfile.EditorOptions;
    ScrollByOneLessChk.Checked := eoScrollByOneLess in FProfile.EditorOptions;
    ShowScrollHintChk.Checked := eoShowScrollHint in FProfile.EditorOptions;
    GroupUndoChk.Checked := eoGroupUndo in FProfile.EditorOptions;

    //Can be override by project options
    TabWidthEdit.Text := IntToStr(FProfile.TabWidth);
    IndentModeCbo.ItemIndex := Ord(FProfile.IndentMode);
    AutoUpdateFileChk.Checked := FProfile.AutoUpdateFile;
  finally
    InChanging := False;
  end;
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
  SetFlag(eoAutoIndentOnPaste, AutoIndentOnPasteChk.Checked);
  SetFlag(eoHalfPageScroll, HalfPageScrollChk.Checked);
  SetFlag(eoScrollByOneLess, ScrollByOneLessChk.Checked);
  SetFlag(eoShowScrollHint, ShowScrollHintChk.Checked);
  SetFlag(eoGroupUndo, GroupUndoChk.Checked);

  FProfile.EditorOptions := aOptions;
  FProfile.ExtEditorOptions := aExtOptions;

  //Gutter
  FProfile.Attributes.GutterAutoSize := GutterAutosizeChk.Checked;
  FProfile.Attributes.GutterShowSeparator := ShowSeparatorChk.Checked;
  FProfile.Attributes.GutterShowModifiedLines := ShowModifiedLinesChk.Checked;
  FProfile.Attributes.GutterLeadingZeros := GutterShowLeaderZerosChk.Checked;
  FProfile.Attributes.CodeFolding := CodeFoldingChk.Checked;

  FProfile.TabWidth := StrToIntDef(TabWidthEdit.Text, 4);
  FProfile.IndentMode := TIndentMode(IndentModeCbo.ItemIndex);
  FProfile.AutoUpdateFile := AutoUpdateFileChk.Checked;
end;

end.
