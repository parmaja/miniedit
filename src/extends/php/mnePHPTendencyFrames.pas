unit mnePHPTendencyFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, EditorEngine, SelectFiles;

type

  { TPHPTendencyFrame }

  TPHPTendencyFrame = class(TFrame, IEditorOptions)
    DebugPortEdit: TComboBox;
    CompilerLabel: TLabel;
  private
  protected
  public
    Tendency: TEditorTendency;
    procedure Apply;
    procedure Retrieve;
  end;

implementation

uses
  mnePHPClasses;

{$R *.lfm}

{ TPHPTendencyFrame }

procedure TPHPTendencyFrame.Apply;
begin
  (Tendency as TPHPTendency).DebugPort := DebugPortEdit.Text;
end;

procedure TPHPTendencyFrame.Retrieve;
begin
  DebugPortEdit.Text := (Tendency as TPHPTendency).DebugPort;
end;

end.

