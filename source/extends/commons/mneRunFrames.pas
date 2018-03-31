unit mneRunFrames;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, EditorEngine, SelectFiles, EditorDebugger;

type

  { TRunFrameOptions }

  TRunFrameOptions = class(TFrame, IEditorOptions)
    Button3: TButton;
    CommandEdit: TComboBox;
    CompilerLabel: TLabel;
    OpenDialog: TOpenDialog;
    PauseChk: TCheckBox;
    ConsoleChk: TCheckBox;
    procedure Button3Click(Sender: TObject);
  private
  protected
  public
    Options : TRunProjectOptions;
    procedure Apply;
    procedure Retrieve;
  end;

implementation

{$R *.lfm}

{ TRunFrameOptions }

procedure TRunFrameOptions.Button3Click(Sender: TObject);
begin
  OpenDialog.Filter := 'EXE files|*.exe|All files|*.*';
  OpenDialog.FileName := CommandEdit.Text;
  OpenDialog.InitialDir := ExtractFilePath(OpenDialog.FileName);
  if OpenDialog.Execute then
  begin
    CommandEdit.Text := OpenDialog.FileName;
  end;
end;

procedure TRunFrameOptions.Apply;
begin
  Options.Pause := PauseChk.Checked;
  Options.Console := ConsoleChk.Checked;
  Options.Command := CommandEdit.Text;
end;

procedure TRunFrameOptions.Retrieve;
begin
  PauseChk.Checked := Options.Pause;
  ConsoleChk.Checked := Options.Console;
  CommandEdit.Text := Options.Command;
end;

end.
