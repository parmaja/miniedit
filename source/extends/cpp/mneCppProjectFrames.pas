unit mneCppProjectFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, EditorEngine, SelectFiles, DebugClasses, mneDClasses;

type

  { TDProjectFrame }

  TCppProjectFrame = class(TFrame, IEditorOptions)
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

{ TCppProjectFrame }

procedure TCppProjectFrame.Apply;
begin
end;

procedure TCppProjectFrame.Retrieve;
begin
  Options := (Project.Options as TDProjectOptions);
end;

end.

