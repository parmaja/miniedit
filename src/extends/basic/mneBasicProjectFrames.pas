unit mneBasicProjectFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, EditorEngine, SelectFiles, mneBasicClasses;

type

  { TBasicProjectFrame }

  TBasicProjectFrame = class(TFrame, IEditorOptions, IEditorProjectFrame)
  private
  protected
    function GetProject: TEditorProject;
  public
    Project: TEditorProject;
    procedure Apply;
    procedure Retrieve;
  end;

implementation

{$R *.lfm}

{ TBasicProjectFrame }

function TBasicProjectFrame.GetProject: TEditorProject;
begin
  Result := Project;
end;

procedure TBasicProjectFrame.Apply;
begin
end;

procedure TBasicProjectFrame.Retrieve;
begin
end;

end.

