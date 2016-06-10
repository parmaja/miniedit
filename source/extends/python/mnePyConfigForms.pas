unit mnePyConfigForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditorEngine;

type

  { TPyConfigForm }

  TPyConfigForm = class(TFrame, IEditorOptions)
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

uses
  mnePyClasses;

{$R *.lfm}

{ TPyConfigForm }

procedure TPyConfigForm.Button3Click(Sender: TObject);
begin
  OpenDialog.Filter := 'EXE files|*.exe|All files|*.*';
  OpenDialog.FileName := CompilerEdit.Text;
  OpenDialog.InitialDir := ExtractFilePath(OpenDialog.FileName);
  if OpenDialog.Execute then
  begin
    CompilerEdit.Text := OpenDialog.FileName;
  end;
end;

procedure TPyConfigForm.Apply;
begin
  (FTendency as TPyTendency).Command := CompilerEdit.Text;
end;

procedure TPyConfigForm.Retrieve;
begin
  CompilerEdit.Text := (FTendency as TPyTendency).Command;
end;

end.

