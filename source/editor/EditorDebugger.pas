unit EditorDebugger;
{$mode delphi}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
interface

uses
  SysUtils, Forms, StrUtils, Variants, Classes, Controls, Graphics, Contnrs, SynEdit;

type
  TEditBreakpoint = record
    Handle: Integer;
    FileName: string;
    Line: Integer;
  end;

  TEditWatch = record
    Name: string;
    VarType: string;
    Value: Variant;
  end;

  { TEditorDebugger }

  TEditorDebugger = class(TObject)
  private
    FExecuteLine: integer;
    FExecuteEdit: TCustomSynEdit;
    function GetActive: Boolean;
    function GetBreakpoints(Index: Integer): TEditBreakpoint;
    function GetWatches(Index: Integer): TEditWatch;
    procedure SetActive(const AValue: Boolean);
  protected
  public
    procedure Start; virtual;
    procedure Stop; virtual;
    procedure Reset; virtual;
    procedure StepInto; virtual;
    procedure StepOver; virtual;
    procedure StepOut; virtual;
    procedure Run; virtual;
    procedure Resume; virtual;
    procedure Lock; virtual;
    procedure Unlock; virtual;
    function IsRuning: Boolean; virtual;

    procedure RunToCursor(FileName:string; LineNo: Integer); virtual;
    procedure ToggleBreakpoint(FileName:string; LineNo: Integer); virtual;
    function IsBreakpoint(FileName:string; LineNo: Integer): Boolean; virtual;
    procedure AddBreakpoint(FileName:string; LineNo: Integer); virtual;
    procedure RemoveBreakpoint(FileName:string; Line: Integer); virtual; overload;
    procedure RemoveBreakpoint(Handle: Integer); virtual; overload;

    property ExecuteLine: integer read FExecuteLine;
    property ExecuteEdit: TCustomSynEdit read FExecuteEdit;
    function BreakpointsCount: Integer; virtual;
    procedure BreakpointsClear; virtual;
    property Breakpoints[Index: Integer]: TEditBreakpoint read GetBreakpoints;

    procedure AddWatch(vName:string); virtual;
    procedure RemoveWatch(vName:string); virtual;
    function WatchesCount: Integer; virtual;
    function WatchesClear: Integer; virtual;
    property Watches[Index: Integer]: TEditWatch read GetWatches;
    function GetWatchValue(vName:string; var vValue: string): Boolean; virtual;

    function GetSessionName: string; virtual;
    property Active: Boolean read GetActive write SetActive;
  end;

implementation

{ TEditorDebugger }

function TEditorDebugger.GetBreakpoints(Index: Integer): TEditBreakpoint;
begin
end;

function TEditorDebugger.GetWatches(Index: Integer): TEditWatch;
begin
  raise Exception.Create('Out of index');
end;

function TEditorDebugger.GetActive: Boolean;
begin
  Result := False;
end;

procedure TEditorDebugger.SetActive(const AValue: Boolean);
begin

end;

procedure TEditorDebugger.Start;
begin

end;

procedure TEditorDebugger.Stop;
begin

end;

procedure TEditorDebugger.Reset;
begin

end;

procedure TEditorDebugger.StepInto;
begin

end;

procedure TEditorDebugger.StepOver;
begin

end;

procedure TEditorDebugger.StepOut;
begin
end;

procedure TEditorDebugger.Run;
begin

end;

procedure TEditorDebugger.Resume;
begin

end;

procedure TEditorDebugger.Lock;
begin

end;

procedure TEditorDebugger.Unlock;
begin

end;

function TEditorDebugger.IsRuning: Boolean;
begin
  Result := False;
end;

procedure TEditorDebugger.RunToCursor(FileName: string; LineNo: Integer);
begin

end;

procedure TEditorDebugger.ToggleBreakpoint(FileName: string; LineNo: Integer);
begin

end;

function TEditorDebugger.IsBreakpoint(FileName: string; LineNo: Integer): Boolean;
begin
  Result := False;
end;

procedure TEditorDebugger.AddBreakpoint(FileName: string; LineNo: Integer);
begin

end;

procedure TEditorDebugger.RemoveBreakpoint(FileName: string; Line: Integer);
begin

end;

procedure TEditorDebugger.RemoveBreakpoint(Handle: Integer);
begin
end;

function TEditorDebugger.BreakpointsCount: Integer;
begin
  Result := 0;
end;

procedure TEditorDebugger.BreakpointsClear;
begin

end;

procedure TEditorDebugger.AddWatch(vName: string);
begin

end;

procedure TEditorDebugger.RemoveWatch(vName: string);
begin

end;

function TEditorDebugger.WatchesCount: Integer;
begin
  Result := 0;
end;

function TEditorDebugger.WatchesClear: Integer;
begin

end;

function TEditorDebugger.GetWatchValue(vName: string; var vValue: string): Boolean;
begin
  Result := False;
end;

function TEditorDebugger.GetSessionName: string;
begin
  Result := ''
end;

end.

