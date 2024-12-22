unit mneDProjectFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, EditorEngine, SelectFiles, mneDClasses;

type

  { TDProjectFrame }

  TDProjectFrame = class(TFrame, IEditorOptions)
    DCompilerCbo: TComboBox;
    Label1: TLabel;
    procedure ExpandPathsChk1Change(Sender: TObject);
  private
    Options: TDProjectOptions;
  protected
  public
    Project: TEditorProject;
    procedure Apply;
    procedure Retrieve;
  end;

implementation

{$R *.lfm}

{ TDProjectFrame }

procedure TDProjectFrame.ExpandPathsChk1Change(Sender: TObject);
begin
end;

procedure TDProjectFrame.Apply;
begin
  Options.CompilerType := TDCompilerType(DCompilerCbo.ItemIndex);
end;

procedure TDProjectFrame.Retrieve;
begin
  DCompilerCbo.Clear;
  DCompilerCbo.Items.Add('DMD');
  DCompilerCbo.Items.Add('GDC');
  DCompilerCbo.Items.Add('LDC');

  Options := (Project.Options as TDProjectOptions);
  DCompilerCbo.ItemIndex := ord(Options.CompilerType);
end;

end.

