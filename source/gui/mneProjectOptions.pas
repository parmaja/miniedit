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
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  EditorEngine, mneClasses, Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type

  { TProjectForm }

  TProjectForm = class(TForm)
    Button3: TButton;
    DescriptionEdit: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label8: TLabel;
    NameEdit: TEdit;
    OkBtn: TButton;
    CancelBtn: TButton;
    OpenDialog: TOpenDialog;
    PageControl: TPageControl;
    RootDirEdit: TEdit;
    SaveDesktopChk: TCheckBox;
    SCMCbo: TComboBox;
    GeneralSheet: TTabSheet;
    procedure Label3Click(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure PageControlChanging(Sender: TObject; var AllowChange: Boolean);
  private
    FProject: TEditorProject;
    FFrame: TFrame;
  protected
  public
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

procedure TProjectForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TProjectForm.Apply(GeneralOnly: Boolean);
begin
  FProject.Name := NameEdit.Text;
  FProject.Description := DescriptionEdit.Text;
  FProject.RootDir := RootDirEdit.Text;
  FProject.SaveDesktop := SaveDesktopChk.Checked;
  FProject.SetSCMClass(TEditorSCM(SCMCbo.Items.Objects[SCMCbo.ItemIndex]));
  if not GeneralOnly and Supports(FFrame, IEditorOptions) then
    (FFrame as IEditorOptions).Apply;
end;

procedure TProjectForm.Retrieve;
var
  TabSheet: TTabSheet;
begin
  FFrame := FProject.Options.CreateOptionsFrame(Self, FProject);
  if FFrame <> nil then
  begin
    TabSheet := PageControl.AddTabSheet;
    TabSheet.Caption := FProject.Tendency.Title;

    FFrame.Parent := TabSheet;
    FFrame.Align := alClient;
    FFrame.Visible := True;
    if Supports(FFrame, IEditorOptions) then
      (FFrame as IEditorOptions).Retrieve;
  end;

  NameEdit.Text := FProject.Name;
  DescriptionEdit.Text := FProject.Description;
  RootDirEdit.Text := FProject.RootDir;
  SaveDesktopChk.Checked := FProject.SaveDesktop;
  if FProject.SCM <> nil then
    SCMCbo.ItemIndex := Engine.SourceManagements.IndexOf(FProject.SCM.Name)
  else
    SCMCbo.ItemIndex := 0;
end;

procedure TProjectForm.Button2Click(Sender: TObject);
begin
  OpenDialog.Filter := 'EXE files|*.exe|All files|*.*';
//  OpenDialog.FileName := ProgramEdit.Text;
  OpenDialog.InitialDir := ExtractFilePath(OpenDialog.FileName);
  if OpenDialog.Execute then
  begin
//    ProgramEdit.Text := OpenDialog.FileName;
  end;
end;

procedure TProjectForm.Button3Click(Sender: TObject);
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

procedure TProjectForm.PageControlChange(Sender: TObject);
begin

end;

procedure TProjectForm.PageControlChanging(Sender: TObject; var AllowChange: Boolean);
begin
  if PageControl.ActivePage = GeneralSheet then
    Apply(True);
end;

end.

