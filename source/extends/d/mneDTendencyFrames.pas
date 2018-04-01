unit mneDTendencyFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, EditorEngine, SelectFiles;

type

  { TDTendencyFrame }

  TDTendencyFrame = class(TFrame, IEditorOptions)
    DCompilerCbo: TComboBox;
    ExpandPathsChk1: TCheckBox;
    ExpandPathsChk2: TCheckBox;
    Label1: TLabel;
    UseMainCfgChk: TCheckBox;
  private
  protected
  public
    Tendency: TEditorTendency;
    procedure Apply;
    procedure Retrieve;
  end;

implementation

uses
  mneDClasses;

{$R *.lfm}

{ TDTendencyFrame }

procedure TDTendencyFrame.Apply;
begin
  (Tendency as TDTendency).CompilerType := DCompilerCbo.ItemIndex;
  (Tendency as TDTendency).UseCfg := UseMainCfgChk.Checked;
end;

procedure TDTendencyFrame.Retrieve;
begin
  DCompilerCbo.Clear;
  DCompilerCbo.Items.Add('DMD');
  DCompilerCbo.Items.Add('GDC');
  DCompilerCbo.Items.Add('LDC');
  DCompilerCbo.ItemIndex := (Tendency as TDTendency).CompilerType;
  UseMainCfgChk.Checked := (Tendency as TDTendency).UseCfg;
end;

end.

