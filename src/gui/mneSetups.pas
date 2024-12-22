unit mneSetups;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey 
 *}

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  mneClasses, Dialogs, StdCtrls, FileUtil, IniFiles;

type

  { TEditorSetupForm }

  TEditorSetupForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Label3: TLabel;
    NeedToRestartLbl: TLabel;
    OkBtn: TButton;
    CancelBtn: TButton;
    WorkspaceEdit: TComboBox;
    procedure Button1Click(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.lfm}

uses
  EditorEngine, mnMsgBox;

procedure TEditorSetupForm.Button1Click(Sender: TObject);
var
  aFolder: string;
begin
  aFolder := WorkspaceEdit.Text;
  if SelectFolder('Select root directory for you project', '', aFolder) then
  begin
    WorkspaceEdit.Text := aFolder;
  end;
end;

procedure TEditorSetupForm.OkBtnClick(Sender: TObject);
var
  aIniFile: TIniFile;
begin
  if WorkspaceEdit.Text = '' then
  begin
    MsgBox.Show('You must enter valid path for workspace directory');
    WorkspaceEdit.SetFocus;
    Abort;
  end
  else
  begin
    aIniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'setting.ini');
    try
      aIniFile.WriteString('options', 'Workspace', ExcludeTrailingPathDelimiter(WorkspaceEdit.Text));
    finally
      aIniFile.Free;
    end;
  end;
  ModalResult := mrOk;
end;

procedure TEditorSetupForm.FormCreate(Sender: TObject);
var
  aIniFile: TIniFile;
begin
  WorkspaceEdit.Items.Add('.' + DirectorySeparator + 'config');
  WorkspaceEdit.Items.Add('.');
  WorkspaceEdit.Items.Add(GetAppConfigDir(False));
  WorkspaceEdit.Items.Add(GetAppConfigDir(True));
  WorkspaceEdit.Items.Add(GetUserDir);
  WorkspaceEdit.Items.Add('.' + DirectorySeparator + 'setting');
  WorkspaceEdit.Items.Add('.' + DirectorySeparator + '.miniedit');
  {$ifdef windows}
  WorkspaceEdit.Items.Add('C:\workspace\miniedit');
  if DirectoryExists('D:\') then
    WorkspaceEdit.Items.Add('D:\workspace\miniedit');
  WorkspaceEdit.Items.Add('\workspace\miniedit');
  {$else}
  WorkspaceEdit.Items.Add(GetUserDir);
  WorkspaceEdit.Items.Add('/usr/workspace/miniedit');
  {$endif}
  WorkspaceEdit.Items.Add('?home' + DirectorySeparator + 'miniedit');
  WorkspaceEdit.Items.Add(Application.Location);
  WorkspaceEdit.ItemIndex := 0;
  aIniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'setting.ini');
  try
    WorkspaceEdit.Text := aIniFile.ReadString('options', 'Workspace', '');
  finally
    aIniFile.Free;
  end;
end;

end.
