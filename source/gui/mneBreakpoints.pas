unit mneBreakpoints;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, EditorEngine, EditorRun;

type
  TBreakpointsForm = class(TForm)
    BreakpointList: TListView;
    CloseBtn: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure BreakpointListDblClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
  private
    FTendency: TEditorTendency;
    procedure Reload;
    procedure DoGoto;
  public
  end;

procedure ShowBreakpointsForm();

implementation

uses
  EditorClasses, mneResources;

{$R *.lfm}

procedure ShowBreakpointsForm;
begin
  with TBreakpointsForm.Create(Application) do
  begin
    Show;
  end;
end;

procedure TBreakpointsForm.FormCreate(Sender: TObject);
begin
  FTendency := Engine.Tendency;
  DebugManager.Enter;
  try
    Reload;
  finally
    DebugManager.Leave;
  end;
end;

procedure TBreakpointsForm.Reload;
var
  i: Integer;
  aItem: TListItem;
begin
  BreakpointList.Clear;
  for i := 0 to FTendency.Debugger.Breakpoints.Count - 1 do
  begin
    aItem := BreakpointList.Items.Add;
    aItem.ImageIndex := 40;
    aItem.Data := Pointer(PtrInt(FTendency.Debugger.Breakpoints[i].Handle));
    aItem.Caption := FTendency.Debugger.Breakpoints[i].FileName;
    aItem.SubItems.Add(IntToStr(FTendency.Debugger.Breakpoints[i].Line));
  end;
end;

procedure TBreakpointsForm.Button2Click(Sender: TObject);
begin
  if BreakpointList.Selected <> nil then
  begin
    DebugManager.Enter;
    try
      FTendency.Debugger.Breakpoints.Remove(IntPtr(BreakpointList.Selected.Data));
      Reload;
      Engine.Update([ecsDebug]);
    finally
      DebugManager.Leave;
    end;
  end;
end;

procedure TBreakpointsForm.Button1Click(Sender: TObject);
begin
  DebugManager.Enter;
  try
    FTendency.Debugger.Breakpoints.Clear;
    Reload;
    Engine.Update([ecsDebug]);
  finally
    DebugManager.Leave;
  end;
end;

procedure TBreakpointsForm.Button3Click(Sender: TObject);
begin
  DoGoto;
end;

procedure TBreakpointsForm.DoGoto;
begin
  if BreakpointList.Selected <> nil then
  begin
    Engine.Files.ShowFile(BreakpointList.Selected.Caption, StrToIntDef(BreakpointList.Selected.SubItems[0], 0));
  end;
end;

procedure TBreakpointsForm.BreakpointListDblClick(Sender: TObject);
begin
  DoGoto;
end;

procedure TBreakpointsForm.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

end.

