unit mneDConfigForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, mneDClasses;

type

  { TDConfigForm }

  TDConfigForm = class(TForm)
    Button3: TButton;
    CancelBtn: TButton;
    Label1: TLabel;
    OkBtn: TButton;
    CompilerEdit: TEdit;
    OpenDialog: TOpenDialog;
    procedure Button3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
  protected
  public
    FTendency: TDTendency;
    procedure Apply;
    procedure Retrieve;
  end;

implementation

uses
  EditorEngine;

{$R *.lfm}

{ TDConfigForm }

procedure TDConfigForm.Button3Click(Sender: TObject);
begin
  OpenDialog.Filter := 'EXE files|*.exe|All files|*.*';
  OpenDialog.FileName := CompilerEdit.Text;
  OpenDialog.InitialDir := ExtractFilePath(OpenDialog.FileName);
  if OpenDialog.Execute then
  begin
    CompilerEdit.Text := OpenDialog.FileName;
  end;
end;

procedure TDConfigForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult = mrOk then
  begin
  end;
end;

procedure TDConfigForm.Apply;
begin
  FTendency.Command := CompilerEdit.Text;
end;

procedure TDConfigForm.Retrieve;
begin
  CompilerEdit.Text := FTendency.Command;
end;

end.

