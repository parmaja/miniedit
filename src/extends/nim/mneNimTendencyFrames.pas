unit mneNimTendencyFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, EditorEngine, SelectFiles;

type

  { TNimTendencyFrame }

  TNimTendencyFrame = class(TFrame, IEditorOptions)
    RunWithCompileCommandChk: TCheckBox;
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
  mneNimClasses;

{$R *.lfm}

{ TNimTendencyFrame }

procedure TNimTendencyFrame.Apply;
begin
  (Tendency as TNimTendency).RunWithCompileCommand := RunWithCompileCommandChk.Checked;
  (Tendency as TNimTendency).UseCfg := UseMainCfgChk.Checked;
end;

procedure TNimTendencyFrame.Retrieve;
begin
  UseMainCfgChk.Checked := (Tendency as TNimTendency).UseCfg;
  RunWithCompileCommandChk.Checked :=  (Tendency as TNimTendency).RunWithCompileCommand;
end;

end.

