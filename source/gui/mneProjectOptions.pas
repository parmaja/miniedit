unit mneProjectOptions;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, SynEdit,
  EditorEngine, mneClasses, Dialogs, StdCtrls, ExtCtrls, ComCtrls, Menus;

type

  { TProjectForm }

  TProjectForm = class(TForm)
    Button3: TButton;
    Label9: TLabel;
    OverrideOptionsChk: TCheckBox;
    DescriptionEdit: TEdit;
    SpecialExtEdit: TEdit;
    Label6: TLabel;
    TabSheet1: TTabSheet;
    TabSpaceEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label8: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    NameEdit: TEdit;
    TabsToSpacesChk: TCheckBox;
    TabWidthEdit: TEdit;
    TitleEdit: TEdit;
    OkBtn: TButton;
    CancelBtn: TButton;
    PageControl: TPageControl;
    PathPopupMenu: TPopupMenu;
    RootDirEdit: TEdit;
    SaveDesktopChk: TCheckBox;
    SCMCbo: TComboBox;
    GeneralSheet: TTabSheet;
    procedure Label3Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PageControlChanging(Sender: TObject; var AllowChange: Boolean);
  private
    FProject: TEditorProject;
    FFrames: array of TFrame;
  protected
  public
    procedure AddFrame(AFrame: TFrame);
    procedure SelectPathFolder;
    procedure Apply(GeneralOnly: Boolean = False);
    procedure Retrieve;
  end;

function ShowProjectForm(vProject: TEditorProject): Boolean;

implementation

uses mneResources;

{$R *.lfm}

function ShowProjectForm(vProject: TEditorProject): Boolean;
begin
  with TProjectForm.Create(Application) do
  begin
    FProject := vProject;
    Retrieve;
    ActiveControl := NameEdit;
    Result := ShowModal = mrOk;
  end;
end;

procedure TProjectForm.OkBtnClick(Sender: TObject);
begin
  Apply;
end;

procedure TProjectForm.Label3Click(Sender: TObject);
begin

end;

procedure TProjectForm.MenuItem1Click(Sender: TObject);
begin
  RootDirEdit.Text := Engine.BrowseFolder;
end;

procedure TProjectForm.MenuItem3Click(Sender: TObject);
begin
  SelectPathFolder;
end;

procedure TProjectForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TProjectForm.Button2Click(Sender: TObject);
begin

end;

procedure TProjectForm.Apply(GeneralOnly: Boolean);
var
  i: Integer;
  procedure SetFlag(aOption: TSynEditorOption; aValue: boolean);
  begin
    if aValue then
      FProject.EditorOptions := FProject.EditorOptions + [aOption]
    else
      FProject.EditorOptions := FProject.EditorOptions - [aOption];
  end;
begin
  FProject.Title := TitleEdit.Text;
  FProject.Name := NameEdit.Text;
  FProject.Description := DescriptionEdit.Text;
  FProject.RootDir := RootDirEdit.Text;
  FProject.SaveDesktop := SaveDesktopChk.Checked;
  FProject.SetSCMClass(TEditorSCM(SCMCbo.Items.Objects[SCMCbo.ItemIndex]));
  if not GeneralOnly then
  begin
    for i :=0 to Length(FFrames) - 1 do
      if Supports(FFrames[i], IEditorOptions) then
        (FFrames[i] as IEditorOptions).Apply;
  end;

  FProject.OverrideEditorOptions := OverrideOptionsChk.Checked;
  FProject.TabWidth := StrToIntDef(TabWidthEdit.Text, 4);
  SetFlag(eoTabsToSpaces, TabsToSpacesChk.Checked);
end;

procedure TProjectForm.Retrieve;
var
  TabSheet: TTabSheet;
  i: Integer;
begin
  Caption := Caption + ' [' + FProject.Tendency.Name + ']';
  FProject.Options.CreateOptionsFrame(Self, FProject, @AddFrame);

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

  TitleEdit.Text := FProject.Title;
  NameEdit.Text := FProject.Name;
  DescriptionEdit.Text := FProject.Description;
  RootDirEdit.Text := FProject.RootDir;
  SaveDesktopChk.Checked := FProject.SaveDesktop;
  if FProject.SCM <> nil then
    SCMCbo.ItemIndex := Engine.SourceManagements.IndexOf(FProject.SCM.Name)
  else
    SCMCbo.ItemIndex := 0;

  //Add any new overrided options to cSynOverridedOptions in EditorProfiles unit
  OverrideOptionsChk.Checked := FProject.OverrideEditorOptions;
  TabWidthEdit.Text := IntToStr(FProject.TabWidth);
  TabsToSpacesChk.Checked := eoTabsToSpaces in FProject.EditorOptions;
end;

procedure TProjectForm.Button3Click(Sender: TObject);
begin
  SelectPathFolder;
end;

procedure TProjectForm.SelectPathFolder;
var
  aFolder: string;
begin
  aFolder := RootDirEdit.Text;
  if (aFolder = '') and (Engine.Files.Current <> nil) then
    aFolder := ExtractFilePath(Engine.Files.Current.Name);
  if SelectFolder('Select root directory for your project', '', aFolder) then
  begin
    RootDirEdit.Text := aFolder;
  end;
end;

procedure TProjectForm.FormCreate(Sender: TObject);
var
  i:Integer;
begin
  SCMCbo.Items.BeginUpdate;
  try
    SCMCbo.Items.Add('None');
    for i := 0 to Engine.SourceManagements.Count -1 do
    begin
      SCMCbo.Items.AddObject(Engine.SourceManagements[i].Title, Engine.SourceManagements[i]);
    end;
    SCMCbo.ItemIndex := 0;
  finally
    SCMCbo.Items.EndUpdate;
  end;
end;

procedure TProjectForm.PageControlChanging(Sender: TObject; var AllowChange: Boolean);
begin
  if PageControl.ActivePage = GeneralSheet then
    Apply(True);
end;

procedure TProjectForm.AddFrame(AFrame: TFrame);
begin
  SetLength(FFrames, Length(FFrames) + 1);
  FFrames[Length(FFrames)- 1] := AFrame;
end;

end.
