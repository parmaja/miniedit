unit mneDProjectFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, EditorEngine, SelectFiles, mneDClasses;

type

  { TDProjectFrame }

  TDProjectFrame = class(TFrame, IEditorOptions)
    ExpandPathsChk1: TCheckBox;
    ExpandPathsChk2: TCheckBox;
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

procedure TDProjectFrame.Apply;
begin
end;

procedure TDProjectFrame.Retrieve;
begin
  Options := (Project.Options as TDProjectOptions);
end;

end.

