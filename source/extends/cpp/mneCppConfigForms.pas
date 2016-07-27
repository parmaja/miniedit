unit mneCppConfigForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditorEngine, mneDClasses;

type

  { TDConfigForm }

  TCppConfigForm = class(TFrame, IEditorOptions)
    Button3: TButton;
    Label1: TLabel;
    CompilerEdit: TEdit;
    OpenDialog: TOpenDialog;
    procedure Button3Click(Sender: TObject);
  private
  protected
  public
    FTendency: TEditorTendency;
    procedure Apply;
    procedure Retrieve;
  end;

implementation

{$R *.lfm}

{ TCppConfigForm }

procedure TCppConfigForm.Button3Click(Sender: TObject);
begin
  OpenDialog.Filter := 'EXE files|*.exe|All files|*.*';
  OpenDialog.FileName := CompilerEdit.Text;
  OpenDialog.InitialDir := ExtractFilePath(OpenDialog.FileName);
  if OpenDialog.Execute then
  begin
    CompilerEdit.Text := OpenDialog.FileName;
  end;
end;

procedure TCppConfigForm.Apply;
begin
  (FTendency as TDTendency).Command := CompilerEdit.Text;
end;

procedure TCppConfigForm.Retrieve;
begin
  CompilerEdit.Text := (FTendency as TDTendency).Command;
end;

end.

