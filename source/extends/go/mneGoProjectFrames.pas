unit mneGoProjectFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, EditorEngine, SelectFiles, mneGoClasses;

type

  { TGoProjectFrame }

  TGoProjectFrame = class(TFrame, IEditorOptions)
    GoCompilerCbo: TComboBox;
    ExpandPathsChk1: TCheckBox;
    ExpandPathsChk2: TCheckBox;
    procedure ExpandPathsChk1Change(Sender: TObject);
  private
    Options: TGoProjectOptions;
  protected
  public
    Project: TEditorProject;
    procedure Apply;
    procedure Retrieve;
  end;

implementation

{$R *.lfm}

{ TGoProjectFrame }

procedure TGoProjectFrame.ExpandPathsChk1Change(Sender: TObject);
begin
end;

procedure TGoProjectFrame.Apply;
begin
  Options.CompilerType := GoCompilerCbo.ItemIndex;
end;

procedure TGoProjectFrame.Retrieve;
begin
  GoCompilerCbo.Clear;
  GoCompilerCbo.Items.Add('GoMGo');
  GoCompilerCbo.Items.Add('GGoC');
  GoCompilerCbo.Items.Add('LGoC');
  Options := (Project.Options as TGoProjectOptions);
  GoCompilerCbo.ItemIndex := Options.CompilerType;
end;

end.
