unit mneTendencyOptions;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey 
 *}

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, SynEdit,
  EditorEngine, EditorProfiles, mneClasses, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Menus;

type

  { TTendencyForm }

  TTendencyForm = class(TForm)
    Button3: TButton;
    Label1: TLabel;
    Label3: TLabel;
    IndentModeCbo: TComboBox;
    GroupBox1: TGroupBox;
    Label9: TLabel;
    OkBtn: TButton;
    CancelBtn: TButton;
    OverrideOptionsChk: TCheckBox;
    MacrosChk: TCheckBox;
    PageControl: TPageControl;
    GeneralSheet: TTabSheet;
    MainPathEdit: TEdit;
    TabWidthEdit: TEdit;
    procedure Button3Click(Sender: TObject);
    procedure PageControlChanging(Sender: TObject; var AllowChange: Boolean);
  private
    FTendency: TEditorTendency;
    FFrames: array of TFrame;
  protected
    procedure AddFrame(AFrame: TFrame);
  public
    procedure SelectPathFolder;
    procedure ApplyFrames;
    procedure Apply;
    procedure RetrieveFrames;
    procedure Retrieve;
  end;

function ShowTendencyForm(vTendency: TEditorTendency): Boolean;

implementation

uses
    EditorClasses, mneResources;

{$R *.lfm}

function ShowTendencyForm(vTendency: TEditorTendency): Boolean;
begin
  with TTendencyForm.Create(Application) do
  begin
    FTendency := vTendency;
    RetrieveFrames;
    Retrieve;
    PageControl.ActivePage := GeneralSheet;
    //ActiveControl := NameEdit;
    Result := ShowModal = mrOk;
    if Result then
    begin
      Apply;
      ApplyFrames;
      Engine.Options.Apply;
      Engine.Update([ecsOptions]);
      Engine.SaveOptions;
    end;
    Free;
  end;
end;

procedure TTendencyForm.Apply;
begin
  FTendency.OverrideEditorOptions := OverrideOptionsChk.Checked;
  FTendency.TabWidth := StrToIntDef(TabWidthEdit.Text, 4);
  FTendency.IndentMode := TIndentMode(IndentModeCbo.ItemIndex);
  FTendency.RunOptions.MainPath := MainPathEdit.Text;
  FTendency.EnableMacros := MacrosChk.Checked;
  FTendency.UpdatePath;
end;

procedure TTendencyForm.RetrieveFrames;
var
  TabSheet: TTabSheet;
  i: Integer;
begin
  Caption := Caption + ' [' + FTendency.Name + ']';
  FTendency.CreateOptionsFrame(Self, @AddFrame);

  for i := 0 to Length(FFrames) - 1 do
  begin
    TabSheet := PageControl.AddTabSheet;
    TabSheet.Caption := FFrames[i].Caption;

    FFrames[i].Parent := TabSheet;
    FFrames[i].Align := alClient;
    FFrames[i].Visible := True;
    if Supports(FFrames[i], IEditorOptions) then
      (FFrames[i] as IEditorOptions).Retrieve;
  end;
end;

procedure TTendencyForm.Retrieve;
begin
  //Add any new overrided options to cSynOverridedOptions in EditorProfiles unit
  EnumIndentMode(IndentModeCbo.Items);
  OverrideOptionsChk.Checked := FTendency.OverrideEditorOptions;
  TabWidthEdit.Text := IntToStr(FTendency.TabWidth);
  IndentModeCbo.ItemIndex := Ord(FTendency.IndentMode);
  MainPathEdit.Text := FTendency.RunOptions.MainPath;
  MacrosChk.Checked := FTendency.EnableMacros;
end;

procedure TTendencyForm.PageControlChanging(Sender: TObject; var AllowChange: Boolean);
begin
  if not (csLoading in ComponentState) then //When createing the form when do not need to trigger
    if (PageControl.ActivePage = GeneralSheet) then
      Apply;
end;

procedure TTendencyForm.Button3Click(Sender: TObject);
begin
  SelectPathFolder;
end;

procedure TTendencyForm.AddFrame(AFrame: TFrame);
begin
  SetLength(FFrames, Length(FFrames) + 1);
  FFrames[Length(FFrames)- 1] := AFrame;
end;

procedure TTendencyForm.SelectPathFolder;
var
  aFolder: string;
begin
  aFolder := MainPathEdit.Text;
  if (aFolder = '') and (Engine.Files.Current <> nil) then
    aFolder := ExtractFilePath(Engine.Files.Current.FileName);
  if SelectFolder('Select root directory for your project', '', aFolder) then
  begin
    MainPathEdit.Text := aFolder;
  end;
end;

procedure TTendencyForm.ApplyFrames;
var
  i: Integer;
begin
  for i :=0 to Length(FFrames) - 1 do
    if Supports(FFrames[i], IEditorOptions) then
      (FFrames[i] as IEditorOptions).Apply;
end;

end.
