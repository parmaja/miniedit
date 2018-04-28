unit mneGoTendencyFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, EditorEngine, SelectFiles;

type

  { TGoTendencyFrame }

  TGoTendencyFrame = class(TFrame, IEditorOptions)
    GoCompilerCbo: TComboBox;
    ExpandPathsChk1: TCheckBox;
    ExpandPathsChk2: TCheckBox;
  private
  protected
  public
    Tendency: TEditorTendency;
    procedure Apply;
    procedure Retrieve;
  end;

implementation

uses
  mneGoClasses;

{$R *.lfm}

{ TGoTendencyFrame }

procedure TGoTendencyFrame.Apply;
begin
end;

procedure TGoTendencyFrame.Retrieve;
begin
end;

end.
