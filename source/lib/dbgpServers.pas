unit dbgpServers;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license   GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 * @ref       http://xdebug.org/docs-dbgp.php#id1
 *}
{

}

{$ifdef WINDOWS}
{.$DEFINE SAVELOG}
{$endif}

interface

uses
  SysUtils, StrUtils, Classes, Contnrs, Dialogs, Variants,
  mnSockets, mnStreams, mnConnections, mnServers, mnXMLUtils, Base64,
  mnXMLRttiProfile, mnXMLNodes, SyncObjs, IniFiles, EditorDebugger, mnClasses;

type
  TdbgpServer = class;
  TdbgpConnection = class;
  TdbgpConnectionClass = class of TdbgpConnection;

  TDebugCommandRespond = class(TmnXMLNodes)
  public
    Source: string;
  end;

  { TdbgpAction }

  TdbgpAction = class(TDebugCommand)
  private
    FConnection: TdbgpConnection;
  protected
    FTransactionID: integer;
    property Connection: TdbgpConnection read FConnection;
    procedure CheckError(Respond: TDebugCommandRespond);
    procedure Execute(Respond: TDebugCommandRespond); virtual; abstract;
  public
  end;

  TdbgpActionClass = class of TdbgpAction;

  TdbgpSpool = class(specialize GItems<TdbgpAction>)
  private
  public
  end;

  { TdbgpConnectionSpool }

  TdbgpConnectionSpool = class(TdbgpSpool)
  private
    FConnection: TdbgpConnection;
  public
    procedure Added(Action: TdbgpAction); override;
  end;

  TdbgpInit = class(TdbgpAction)
  protected
    procedure Created; override;
  public
    function GetCommand: string; override;
    procedure Execute(Respond: TDebugCommandRespond); override;
  end;

  { TdbgpFeatureSet }

  TdbgpFeatureSet = class(TdbgpAction)
  protected
    FName: string;
    FValue: string;
    procedure Execute(Respond: TDebugCommandRespond); override;
  public
    constructor CreateBy(vName, vValue: string);
    function GetCommand: string; override;
  end;

  { TdbgpCommandSet }

  TdbgpCommandSet = class(TdbgpAction)
  protected
    FName: string;
    FValue: string;
    procedure Execute(Respond: TDebugCommandRespond); override;
  public
    constructor CreateBy(vName, vValue: string);
    function GetCommand: string; override;
  end;


  { TdbgpGetCurrent }

  TdbgpGetCurrent = class(TdbgpAction)
  private
    FCurKey: string;
    FCurFile: string;
    FCurLine: integer;
    FCallStack: TCallStackItems;
    procedure ShowFile;
  public
    procedure Created; override;
    destructor Destroy; override;
    function GetCommand: string; override;
    procedure Execute(Respond: TDebugCommandRespond); override;
  end;

  TdbgpStepOver = class(TdbgpAction)
  public
    function GetCommand: string; override;
    procedure Execute(Respond: TDebugCommandRespond); override;
  end;

  TdbgpStepInto = class(TdbgpAction)
  public
    function GetCommand: string; override;
    procedure Execute(Respond: TDebugCommandRespond); override;
  end;

  TdbgpStepOut = class(TdbgpAction)
  public
    function GetCommand: string; override;
    procedure Execute(Respond: TDebugCommandRespond); override;
  end;

  TdbgpRun = class(TdbgpAction)
  public
    function GetCommand: string; override;
    procedure Execute(Respond: TDebugCommandRespond); override;
  end;

  { TdbgpDetach }

  TdbgpDetach = class(TdbgpAction)
  public
    function GetCommand: string; override;
    procedure Execute(Respond: TDebugCommandRespond); override;
    destructor Destroy; override;
  end;

  TdbgpStop = class(TdbgpAction)
  public
    function GetCommand: string; override;
    procedure Execute(Respond: TDebugCommandRespond); override;
  end;

  TdbgpCustomGet = class(TdbgpAction)
  public
    Info: TDebugWatchInfo;
  end;
  // Watches

  TdbgpCustomGetWatch = class(TdbgpCustomGet)
  protected
  public
    function GetCommand: string; override;
    procedure Execute(Respond: TDebugCommandRespond); override;
  end;

  TdbgpGetWatch = class(TdbgpCustomGetWatch)
  protected
  public
    Index: integer;
    procedure Execute(Respond: TDebugCommandRespond); override;
  end;

  { TdbgpEval }

  TdbgpEval = class(TdbgpCustomGet)
  protected
  public
    function GetCommand: string; override;
    function GetData: string; override;
    procedure Execute(Respond: TDebugCommandRespond); override;
  end;

  { TdbgpGetWatchInstance }

  TdbgpGetWatchInstance = class(TdbgpCustomGetWatch)
  protected
  public
  end;

  TdbgpGetWatches = class(TdbgpCustomGetWatch)
  protected
  public
    Current: integer;
    function Stay: boolean; override;
    function Enabled: boolean; override;
    procedure Execute(Respond: TDebugCommandRespond); override;
  end;

  // Breakpoints

  TdbgpSetBreakpoint = class(TdbgpAction)
  protected
  public
    BreakpointID: cardinal;
    FileName: string;
    FileLine: integer;
    function GetCommand: string; override;
    procedure Execute(Respond: TDebugCommandRespond); override;
  end;

  TdbgpSetBreakpoints = class(TdbgpSetBreakpoint)
  protected
  public
    Current: integer;
    function Enabled: boolean; override;
    function Stay: boolean; override;
  end;

  { TdbgpRemoveBreakpoint }

  TdbgpRemoveBreakpoint = class(TdbgpAction)
  protected
    procedure Execute(Respond: TDebugCommandRespond); override;
  public
    BreakpointID: integer;
    function GetCommand: string; override;
  end;

//* Watches

  TdbgpWatch = class(TObject)
  private
    FHandle: integer;
  public
    Info: TDebugWatchInfo;
    property Handle: integer read FHandle write FHandle;
  published
  end;

  TdbgpWatches = class(specialize GItems<TdbgpWatch>)
  private
    FServer: TdbgpServer;
    CurrentHandle: integer;
    function GetValues(Name: string): variant;
    procedure SetValues(Name: string; const Value: variant);
  protected
    property Server: TdbgpServer read FServer;
  public
    function Find(Name: string): TdbgpWatch;
    function Add(Name: string; Value: variant): integer; overload;
    procedure AddWatch(Name: string);
    procedure RemoveWatch(Name: string);
    procedure Clean;
    property Values[Name: string]: variant read GetValues write SetValues;
  end;

//* Breakpoints

  TdbgpBreakpoint = class(TObject)
  private
    FID: integer;
    FLine: integer;
    FHandle: integer;
    FFileName: string;
  public
    property Handle: integer read FHandle write FHandle;
    property ID: integer read FID write FID;
  published
    property FileName: string read FFileName write FFileName;
    property Line: integer read FLine write FLine;
  end;

  TdbgpBreakpoints = class(specialize GItems<TdbgpBreakpoint>)
  private
    CurrentHandle: integer;
    FServer: TdbgpServer;
  protected
    property Server: TdbgpServer read FServer;
  public
    function Remove(Breakpoint: TdbgpBreakpoint): integer; overload;
    procedure Remove(Handle: integer); overload;
    function Add(FileName: string; Line: integer): integer; overload;
    function Find(Name: string; Line: integer): TdbgpBreakpoint;
    procedure Toggle(FileName: string; Line: integer);
  end;

  TdbgpOnServerEvent = procedure(Sender: TObject; Socket: TdbgpConnection) of object;

  { TdbgpConnection }

  TdbgpConnection = class(TmnServerConnection)
  private
    FLocalSpool: TdbgpConnectionSpool;
    FKey: string;
    function GetServer: TdbgpServer;
  public
    FTransactionID: integer;
  protected
    function NewTransactionID: integer;
{$IFDEF SAVELOG}
    procedure SaveLog(s: string);
{$ENDIF}
    function ReadRespond: TDebugCommandRespond;
    function PopAction: TdbgpAction;
    function SendCommand(Command: string; Data: string): integer;
    procedure Prepare; override;
    procedure DoExecute;
    procedure Process; override;
  public
    constructor Create(vConnector: TmnConnector; Socket: TmnCustomSocket); override;
    destructor Destroy; override;
    procedure Stop; override;
    property Key: string read FKey;
    property Server: TdbgpServer read GetServer;
  published
  end;

  TmnDBGListener = class(TmnListener)
  private
  protected
    function CreateConnection(vSocket: TmnCustomSocket): TmnServerConnection; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TdbgpServer }

  TdbgpServer = class(TmnServer)
  private
    FBreakOnFirstLine: Boolean;
    FSpool: TdbgpSpool;
    FStackDepth: Integer;
    FWatches: TdbgpWatches;
    FBreakpoints: TdbgpBreakpoints;
    FRunCount: Integer;
    FKey: string;
    function GetIsRuning: Boolean;
  protected
    function CreateListener: TmnListener; override;
    procedure DoChanged(vListener: TmnListener); override;
    procedure DoStart; override;
    procedure DoStop; override;
    property Spool: TdbgpSpool read FSpool;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Resume;
    procedure AddAction(Action: TdbgpAction); overload;
    procedure AddAction(ActionClass: TdbgpActionClass); overload;
    procedure RemoveAction(Action: TdbgpAction);
    procedure ExtractAction(Action: TdbgpAction);
    procedure Clear;
    property RunCount: Integer read FRunCount;
    property IsRuning: Boolean read GetIsRuning;
    property Watches: TdbgpWatches read FWatches;
    property StackDepth: Integer read FStackDepth write FStackDepth;
    property Breakpoints: TdbgpBreakpoints read FBreakpoints;
    property BreakOnFirstLine: Boolean read FBreakOnFirstLine write FBreakOnFirstLine default False;
    property Key: string read FKey;
  published
  end;

  TdbgpDebug = class;

  { TdbgpDebugBreakPoints }

  TdbgpDebugBreakPoints = class(TEditorBreakPoints)
  protected
    FDebug: TdbgpDebug;
    function GetCount: integer; override;
    function GetItems(Index: integer): TDebugBreakpointInfo; override;
  public
    procedure Clear; override;
    procedure Toggle(FileName: string; LineNo: integer); override;
    function IsExists(FileName: string; LineNo: integer): boolean; override;
    procedure Add(FileName: string; LineNo: integer); override;
    procedure Remove(FileName: string; Line: integer); override; overload;
    procedure Remove(Handle: integer); override; overload;
  end;

  { TdbgpDebugWatches }

  TdbgpDebugWatches = class(TEditorWatches)
  protected
    FDebug: TdbgpDebug;
    function GetCount: integer; override;
    function GetItems(Index: integer): TDebugWatchInfo; override;
  public
    procedure Clear; override;
    procedure Add(vName: string); override;
    procedure Remove(vName: string); override;
    function GetValue(vName: string; out vValue: Variant; out vType: string; EvalIt: Boolean): boolean; override;
  end;

  { TdbgpDebug }

  TdbgpDebug = class(TEditorDebugger)
  private
    FServer: TdbgpServer;
  protected
    function CreateBreakPoints: TEditorBreakPoints; override;
    function CreateWatches: TEditorWatches; override;

    procedure Reset;
    procedure Resume;
    procedure StepOver;
    procedure StepInto;
    procedure StepOut;
    procedure Run;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start; override;
    procedure Stop; override;
    procedure Action(AAction: TDebugAction); override;
    function GetState: TDebugStates; override;
    function GetKey: string; override;
  end;

implementation

uses
  EditorEngine;

{ TdbgpAction }

procedure TdbgpAction.CheckError(Respond: TDebugCommandRespond);
begin
  if (Respond.Root <> nil) then
    if StrToIntDef(Respond.GetAttribute('response', 'transaction_id'), -1) <> FTransactionID then
      raise EDebugException.Create('transaction_id is not same with command.'#13 + Respond.Source);
end;

{ TdbgpCommandSet }

procedure TdbgpCommandSet.Execute(Respond: TDebugCommandRespond);
begin
end;

constructor TdbgpCommandSet.CreateBy(vName, vValue: string);
begin
  Create;
  FName := vName;
  FValue:= vValue;
end;

function TdbgpCommandSet.GetCommand: string;
begin
  Result := FName + ' ' + FValue;
end;

{ TdbgpEval }

function TdbgpEval.GetCommand: string;
begin
  Result := 'eval';
end;

function TdbgpEval.GetData: string;
begin
  Result := 'echo ' + Info.Name;
end;

procedure TdbgpEval.Execute(Respond: TDebugCommandRespond);
begin
end;

{ TdbgpFeatureSet }

procedure TdbgpFeatureSet.Execute(Respond: TDebugCommandRespond);
begin
end;

constructor TdbgpFeatureSet.CreateBy(vName, vValue: string);
begin
  Create;
  FName := vName;
  FValue:= vValue;
end;

function TdbgpFeatureSet.GetCommand: string;
begin
  // 'feature_set -n show_hidden -v 1';
  Result := 'feature_set -n ' + FName + ' -v '+ FValue;
end;

constructor TdbgpServer.Create;
begin
  inherited;
  FSpool := TdbgpSpool.Create(True);
  Port := '9000';
  FStackDepth := 10;
  FWatches := TdbgpWatches.Create;
  FWatches.FServer := Self;
  FBreakpoints := TdbgpBreakpoints.Create;
  FBreakpoints.FServer := Self;
  FBreakpoints.FServer := Self;
  FBreakOnFirstLine := False;
end;

destructor TdbgpServer.Destroy;
begin
  FreeAndNil(FWatches);
  FreeAndNil(FBreakpoints);
  FreeAndNil(FSpool);
  inherited;
end;

function TdbgpServer.GetIsRuning: Boolean;
begin
  Result := RunCount > 0;
end;

constructor TdbgpConnection.Create(vConnector: TmnConnector; Socket: TmnCustomSocket);
begin
  inherited;
  FLocalSpool := TdbgpConnectionSpool.Create;
  FLocalSpool.FConnection := Self;
  //KeepAlive := True;
  Stream.Timeout := 5000;
end;

destructor TdbgpConnection.Destroy;
begin
  FreeAndNil(FLocalSpool);
  inherited;
end;

{ TdbgpConnection }

function TdbgpConnection.NewTransactionID: integer;
begin
  Inc(FTransactionID);
  Result := FTransactionID;
end;

procedure TdbgpGetCurrent.ShowFile; //this function must Synchronize
begin
  Engine.DebugLink.SetExecutedLine(FCurKey, FCurFile, FCurLine, FCallStack);
end;

procedure TdbgpConnection.DoExecute;
var
  aAction: TdbgpAction;
  aRespond: TDebugCommandRespond;
  aCommand: string;
  aKeep: Boolean;
begin
  aAction := PopAction;
  if aAction <> nil then
  begin
    if not aAction.Enabled then
      FLocalSpool.Remove(aAction)
    else
      try
        aCommand := aAction.GetCommand;
        if (dafSend in aAction.Flags) and (aCommand <> '') then
          aAction.FTransactionID := SendCommand(aCommand, aAction.GetData);
        if aAction.Accept and Connected then
        begin
          aRespond := ReadRespond;
          try
            if (aRespond <> nil) and (aRespond.Root <> nil) then
            begin
              if (aRespond.GetAttribute('response', 'status') = 'stopping') then
                Disconnect
              else if (aRespond.GetAttribute('response', 'status') = 'stoped') then
              begin
                //Disconnect;
              end
              else
              begin
                try
                  if (aRespond <> nil) and Connected and (aRespond.Root <> nil) then
                    aAction.Execute(aRespond);
                finally
                end;
              end;
            end;
          finally
            FreeAndNil(aRespond);
          end;
        end;
      finally
        if not aAction.Stay then
        begin
          aKeep := (aAction.Event <> nil) or aAction.KeepAlive;
          if aAction.Event <> nil then
            aAction.Event.SetEvent;
          if aKeep then
            FLocalSpool.Extract(aAction)
          else
            FLocalSpool.Remove(aAction);
        end;
      end;
  end;
end;

{ TdbgpSocketServer }

function TdbgpServer.CreateListener: TmnListener;
begin
  Result := TmnDBGListener.Create;
end;

function TdbgpConnection.ReadRespond: TDebugCommandRespond;
var
  Reader: TmnXMLNodeReader;
  s: string;
  aMatched: boolean;
begin
  Result := nil;
  Stream.ReadUntil(#0, true, s, aMatched);
  if Connected and aMatched and (S <> '') then
  begin
    Result := TDebugCommandRespond.Create;
    Stream.ReadUntil(#0, true, s, aMatched);
    s := Trim(s);
    {$IFDEF SAVELOG}
    SaveLog(s);
    {$ENDIF}
    Result.Source := s;
    Reader := TmnXMLNodeReader.Create;
    try
      Reader.Start;
      Reader.Nodes := Result;
      Reader.Parse(s);
    finally
      Reader.Free;
    end;
  end;
end;

{$IFDEF SAVELOG}
procedure TdbgpConnection.SaveLog(s: string);
var
  aStrings: TStringList;
  aStream: TFileStream;
  i: integer;
const
  sFile = 'c:\xdebug_server.log';
begin
  aStrings := TStringList.Create;
  aStrings.Text := s;
  if FileExists(sFile) then
  begin
    aStream := TFileStream.Create(sFile, fmOpenWrite);
    aStream.Seek(0, soFromEnd);
  end
  else
    aStream := TFileStream.Create(sFile, fmCreate);

  try
    for i := 0 to aStrings.Count - 1 do
    begin
      s := aStrings[i] + #13;
      aStream.Write(s[1], Length(s));
    end;
  finally
    aStream.Free;
    aStrings.Free;
  end;
end;

{$ENDIF}

function TdbgpConnection.SendCommand(Command: string; Data: string): integer;
var
  s: string;
begin
  Result := NewTransactionID;
  s := Command + ' -i ' + IntToStr(Result);
  if Data <> '' then
    s := s + ' -- ' + Data;
  Stream.WriteLine(s, #0);
{$IFDEF SAVELOG}
  SaveLog(s);
{$ENDIF}
end;

function TdbgpConnection.GetServer: TdbgpServer;
begin
  Result := (Listener.Server as TdbgpServer);
end;

function TdbgpConnection.PopAction: TdbgpAction;
var
  aAction: TdbgpAction;
  i: integer;
begin
  if FLocalSpool.Count = 0 then
  begin
    InterLockedIncrement(Server.FRunCount);
    DebugManager.Event.WaitFor(INFINITE); //wait the ide to make resume
    InterLockedDecrement(Server.FRunCount);

    DebugManager.Lock.Enter;
    try
      i := 0;
      while i < Server.Spool.Count do
      begin
        aAction := Server.Spool.Extract(Server.Spool[i]) as TdbgpAction;
        //        if aAction.Key = Key then
        FLocalSpool.Add(aAction);
        //        else
        //        inc(i);
      end;
    finally
      DebugManager.Lock.Leave;
    end;
  end;
  Result := nil;
  while not Terminated and ((FLocalSpool.Count > 0) and (Result = nil)) do
  begin
    Result := FLocalSpool[0];
    Result.Prepare;
  end;
end;

procedure TdbgpConnection.Prepare;
begin
  inherited;
  FLocalSpool.Add(TdbgpInit.Create);
  FLocalSpool.Add(TdbgpFeatureSet.CreateBy('show_hidden', '1'));
  FLocalSpool.Add(TdbgpFeatureSet.CreateBy('max_depth', IntToStr(Server.StackDepth)));
  FLocalSpool.Add(TdbgpFeatureSet.CreateBy('max_children', '100'));

  FLocalSpool.Add(TdbgpSetBreakpoints.Create);
  FLocalSpool.Add(TdbgpCommandSet.CreateBy('breakpoint_set', '-t exception -X Error -s enabled'));
  FLocalSpool.Add(TdbgpCommandSet.CreateBy('breakpoint_set', '-t exception -X Warning -s enabled'));
  { or
    breakpoint_set -t exception -X Error
    breakpoint_set -t exception -X Warning
    breakpoint_set -t exception -X Notice
  }

  if Server.BreakOnFirstLine then
  begin
    FLocalSpool.Add(TdbgpStepInto.Create);
    FLocalSpool.Add(TdbgpGetCurrent.Create);
  end
  else
  begin
    FLocalSpool.Add(TdbgpRun.Create);
    FLocalSpool.Add(TdbgpGetWatches.Create);
    FLocalSpool.Add(TdbgpGetCurrent.Create);
  end;
end;

procedure TdbgpConnection.Process;
begin
  inherited;
  //Allow one connection to Execute
  //Listener.Enter;
  try
    DoExecute;
  finally
    //Listener.Leave;
  end;
end;

procedure TdbgpConnection.Stop;
begin
  inherited;
  DebugManager.Event.SetEvent;
end;

{ TmnDBGListener }

function TmnDBGListener.CreateConnection(vSocket: TmnCustomSocket): TmnServerConnection;
begin
  Result := TdbgpConnection.Create(Self, vSocket);
end;

constructor TmnDBGListener.Create;
begin
  inherited;
  FOptions := FOptions + [soReuseAddr];
end;

destructor TmnDBGListener.Destroy;
begin
  inherited;
end;

procedure TdbgpServer.DoStart;
begin
  Spool.Clear;
  inherited;
end;

procedure TdbgpServer.DoStop;
begin
  inherited;
  if FSpool <> nil then //DoStop class when Server free
    FSpool.Clear;
end;

procedure TdbgpServer.DoChanged(vListener: TmnListener);
begin
  inherited;
  if vListener.Count = 0 then //TODO: i am not sure in Linux
    Engine.DebugLink.SetExecutedLine('', '', 0);
end;

{ TdbgpStepOver }

function TdbgpStepOver.GetCommand: string;
begin
  Result := 'step_over';
end;

procedure TdbgpStepOver.Execute(Respond: TDebugCommandRespond);
begin
end;

{ TdbgpStepInto }

function TdbgpStepInto.GetCommand: string;
begin
  Result := 'step_into';
end;

procedure TdbgpStepInto.Execute(Respond: TDebugCommandRespond);
begin
end;

procedure TdbgpServer.Resume;
begin
  DebugManager.Event.SetEvent;
end;

{ TdbgpInit }

procedure TdbgpInit.Created;
begin
  inherited;
  Flags := Flags - [dafSend];
end;

function TdbgpInit.GetCommand: string;
begin
  Result := 'init';
end;

procedure TdbgpInit.Execute(Respond: TDebugCommandRespond);
begin
  DebugManager.Lock.Enter;
  try
    Connection.Server.Watches.Clean;
    Connection.FKey := Respond.Root.Attributes['idekey'];
  finally
    DebugManager.Lock.Leave;
  end;
end;

{ TdbgpGetCurrent }

function TdbgpGetCurrent.GetCommand: string;
{var
  aDepth: Integer;}
begin
  //aDepth := Connection.Server.StackDepth;
  Result := 'stack_get';
{  if aDepth > 0 then
    Result := Result + ' -d ' + IntToStr(aDepth);}
end;

procedure TdbgpGetCurrent.Execute(Respond: TDebugCommandRespond);
var
  i: Integer;
begin
(*
  <response xmlns="urn:debugger_protocol_v1" xmlns:xdebug="http://xdebug.org/DebugManager/xdebug" command="stack_get" transaction_id="8">
  <stack where="App-&gt;__construct" level="0" type="file" filename="file:///W:/web/sites/abrash.com/websale/fw/core/ui/app.php" lineno="200"></stack>
  <stack where="{main}" level="1" type="file" filename="file:///W:/web/sites/abrash.com/websale/index.php" lineno="8"></stack>
  </response>
*)
  if Respond.Root.Items.Count > 0 then
  begin
    FCallStack := TCallStackItems.Create;
    try
      for i := 0 to Respond.Root.Items.Count -1 do
      begin
        if SameText(Respond.Root.Items[i].Name, 'stack') then
          FCallStack.Add(URIToFileName(Respond.Root.Items[i].Attributes.Values['filename']), StrToIntDef(Respond.Root.Items[i].Attributes.Values['lineno'], 0));
      end;
    finally
    end;

    FCurFile := URIToFileName(Respond.GetAttribute('stack', 'filename'));
    if FCurFile <> '' then
    begin
      FCurKey := Connection.Key;
      FCurLine := StrToIntDef(Respond.GetAttribute('stack', 'lineno'), 0);
      try
        //Dont do any lock here
        Connection.Synchronize(@ShowFile);
      finally
      end;
    end;
  end;
end;

procedure TdbgpGetCurrent.Created;
begin
  inherited;
  Flags := Flags + [dafCheckError];
end;

destructor TdbgpGetCurrent.Destroy;
begin
  FreeAndNil(FCallStack);
  inherited Destroy;
end;

{ TdbgpRun }

function TdbgpRun.GetCommand: string;
begin
  Result := 'run';
end;

procedure TdbgpRun.Execute(Respond: TDebugCommandRespond);
begin
end;

{ TdbgpDetach }

function TdbgpDetach.GetCommand: string;
begin
  Result := 'detach';
end;

procedure TdbgpDetach.Execute(Respond: TDebugCommandRespond);
begin
  Connection.Disconnect;
end;

destructor TdbgpDetach.Destroy;
begin
  inherited Destroy;
end;

{ TdbgpStop }

function TdbgpStop.GetCommand: string;
begin
  Result := 'stop';
end;

procedure TdbgpStop.Execute(Respond: TDebugCommandRespond);
begin
  Connection.Disconnect;
end;

{ TdbgpStepOut }

function TdbgpStepOut.GetCommand: string;
begin
  Result := 'step_out';
end;

procedure TdbgpStepOut.Execute(Respond: TDebugCommandRespond);
begin
end;

{ TdbgpWatches }

function TdbgpWatches.Add(Name: string; Value: variant): integer;
var
  aWatch: TdbgpWatch;
begin
  Inc(CurrentHandle);
  aWatch := TdbgpWatch.Create;
  aWatch.Handle := CurrentHandle;
  aWatch.Info.Name := Name;
  aWatch.Info.VarType := '';
  aWatch.Info.Value := Value;
  Result := Add(aWatch);
end;

procedure TdbgpWatches.AddWatch(Name: string);
begin
  DebugManager.Lock.Enter;
  try
    Add(Name, '');
  finally
    DebugManager.Lock.Leave;
  end;
  if Server.IsRuning then
  begin
    DebugManager.Lock.Enter;
    try
      Server.Spool.Add(TdbgpGetWatches.Create);
      Server.Spool.Add(TdbgpGetCurrent.Create);
    finally
      DebugManager.Lock.Leave;
    end;
    Server.Resume;
  end;
end;

procedure TdbgpWatches.Clean;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    VarClear(Items[i].Info.Value);
  end;
end;

function TdbgpWatches.Find(Name: string): TdbgpWatch;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Items[i].Info.Name = Name then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TdbgpWatches.GetValues(Name: string): variant;
var
  aWatch: TdbgpWatch;
begin
  aWatch := Find(Name);
  if aWatch <> nil then
    Result := aWatch.Info.Value
  else
    VarClear(Result);
end;

procedure TdbgpWatches.RemoveWatch(Name: string);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Items[i].Info.Name = Name then
    begin
      Delete(i);
      break;
    end;
  end;
  if Server.IsRuning then
  begin
    DebugManager.Lock.Enter;
    try
      Server.Spool.Add(TdbgpGetWatches.Create);
      Server.Spool.Add(TdbgpGetCurrent.Create);
    finally
      DebugManager.Lock.Leave;
    end;
    Server.Resume;
  end;
end;

procedure TdbgpWatches.SetValues(Name: string; const Value: variant);
begin
end;

{ TdbgpGetWatch }

procedure TdbgpGetWatch.Execute(Respond: TDebugCommandRespond);
begin
  inherited;
  DebugManager.Lock.Enter;
  try
    Connection.Server.Watches[Index].Info.Value := Info.Value;
    Connection.Server.Watches[Index].Info.VarType := Info.VarType;
  finally
    DebugManager.Lock.Leave;
  end;
end;

{ TdbgpBreakpoints }

function TdbgpBreakpoints.Add(FileName: string; Line: integer): integer;
var
  aBreakpoint: TdbgpBreakpoint;
begin
  Inc(CurrentHandle);
  aBreakpoint := TdbgpBreakpoint.Create;
  aBreakpoint.Handle := CurrentHandle;
  aBreakpoint.FileName := FileName;
  aBreakpoint.Line := Line;
  Result := Add(aBreakpoint);
end;

function TdbgpBreakpoints.Find(Name: string; Line: integer): TdbgpBreakpoint;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if (Items[i].line = Line) and SameText(Items[i].FileName, Name) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TdbgpBreakpoints.Remove(Breakpoint: TdbgpBreakpoint): integer;
begin
  Result := inherited Remove(Breakpoint);
end;

procedure TdbgpBreakpoints.Remove(Handle: integer);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Items[i].Handle = Handle then
    begin
      Delete(i);
      break;
    end;
  end;
end;

procedure TdbgpBreakpoints.Toggle(FileName: string; Line: integer);
var
  aBreakpoint: TdbgpBreakpoint;
  aSetBreakpoint: TdbgpSetBreakpoint;
  aRemoveBreakpoint: TdbgpRemoveBreakpoint;
begin
  aBreakpoint := Find(FileName, Line);
  if aBreakpoint <> nil then
  begin
    Remove(aBreakpoint);
    if Server.IsRuning then
    begin
      if aBreakpoint.ID <> 0 then
      begin
        aRemoveBreakpoint := TdbgpRemoveBreakpoint.Create;
        aRemoveBreakpoint.BreakpointID := aBreakpoint.ID;
        Server.Spool.Add(aRemoveBreakpoint);
      end;
    end;
  end
  else
  begin
    Add(FileName, Line);
    if Server.IsRuning then
    begin
      aSetBreakpoint := TdbgpSetBreakpoint.Create;
      aSetBreakpoint.FileName := FileName;
      aSetBreakpoint.FileLine := Line;
      Server.Spool.Add(aSetBreakpoint);
    end;
  end;
end;

{ TdbgpGetWatches }

function TdbgpGetWatches.Stay: boolean;
begin
  DebugManager.Lock.Enter;
  try
    Inc(Current);
    Result := Current < Connection.Server.Watches.Count;
  finally
    DebugManager.Lock.Leave;
  end;
end;

procedure TdbgpGetWatches.Execute(Respond: TDebugCommandRespond);
begin
  inherited;
  DebugManager.Lock.Enter;
  try
    Connection.Server.Watches[Current].Info.Value := Info.Value;
    Connection.Server.Watches[Current].Info.VarType := Info.VarType;
  finally
    DebugManager.Lock.Leave;
  end;
end;

function TdbgpGetWatches.Enabled: boolean;
begin
  DebugManager.Lock.Enter;
  try
    Result := Current < Connection.Server.Watches.Count;
    if Result then
      Info.Name := Connection.Server.Watches[Current].Info.Name;
  finally
    DebugManager.Lock.Leave;
  end;
end;

{ TdbgpSetBreakpoints }

function TdbgpSetBreakpoints.Enabled: boolean;
begin
  DebugManager.Lock.Enter;
  try
    Result := Current < Connection.Server.Breakpoints.Count;
    if Result then
    begin
      FileName := Connection.Server.Breakpoints[Current].FileName;
      FileLine := Connection.Server.Breakpoints[Current].Line;
    end;
  finally
    DebugManager.Lock.Leave;
  end;
end;

function TdbgpSetBreakpoints.Stay: boolean;
begin
  DebugManager.Lock.Enter;
  try
    Connection.Server.Breakpoints[Current].ID := BreakpointID;
    Inc(Current);
    Result := Current < Connection.Server.Breakpoints.Count;
  finally
    DebugManager.Lock.Leave;
  end;
end;

{ TdbgpSetBreakpoint }

function TdbgpSetBreakpoint.GetCommand: string;
begin
  Result := 'breakpoint_set -t line -n ' + IntToStr(FileLine) + ' -f ' + FileNameToURI(FileName) + '';
end;

procedure TdbgpSetBreakpoint.Execute(Respond: TDebugCommandRespond);
begin
  CheckError(Respond);
  BreakpointID := StrToInt(Respond.Root.Attributes['id']);
end;

{ TdbgpRemoveBreakpoint }

procedure TdbgpRemoveBreakpoint.Execute(Respond: TDebugCommandRespond);
begin
end;

function TdbgpRemoveBreakpoint.GetCommand: string;
begin
  Result := 'breakpoint_remove -d ' + IntToStr(BreakpointID);
end;

{ TdbgpConnectionSpool }

procedure TdbgpConnectionSpool.Added(Action: TdbgpAction);
begin
  inherited Added(Action);
  Action.FConnection := FConnection;
end;

{$IFDEF SAVELOG}
procedure SaveLog(s: string);
var
  aStrings: TStringList;
  aStream: TFileStream;
  i: integer;
const
  sFile = 'c:\lock_server.log';
begin
  aStrings := TStringList.Create;
  aStrings.Text := s;
  if FileExists(sFile) then
  begin
    aStream := TFileStream.Create(sFile, fmOpenWrite);
    aStream.Seek(0, soFromEnd);
  end
  else
    aStream := TFileStream.Create(sFile, fmCreate);

  try
    for i := 0 to aStrings.Count - 1 do
    begin
      s := aStrings[i] + #13;
      aStream.Write(s[1], Length(s));
    end;
  finally
    aStream.Free;
    aStrings.Free;
  end;
end;
{$ENDIF}

procedure TdbgpServer.AddAction(Action: TdbgpAction);
begin
  DebugManager.Lock.Enter;
  try
    Spool.Add(Action);
  finally
    DebugManager.Lock.Leave;
  end;
end;

procedure TdbgpServer.AddAction(ActionClass: TdbgpActionClass);
begin
  AddAction(ActionClass.Create);
end;

procedure TdbgpServer.RemoveAction(Action: TdbgpAction);
begin
  DebugManager.Lock.Enter;
  try
    Spool.Remove(Action);
  finally
    DebugManager.Lock.Leave;
  end;
end;

procedure TdbgpServer.ExtractAction(Action: TdbgpAction);
begin
  DebugManager.Lock.Enter;
  try
    Spool.Extract(Action);
  finally
    DebugManager.Lock.Leave;
  end;
end;

procedure TdbgpServer.Clear;
begin
  DebugManager.Lock.Enter;
  try
    Spool.Clear;
  finally
    DebugManager.Lock.Leave;
  end;
end;

{ TdbgpCustomGetWatch }

function TdbgpCustomGetWatch.GetCommand: string;
begin
  Result := 'property_value -n "' + Info.Name + '" -m 1024';
  //Result := 'property_get -n "' + Name + '" -m 1024';
end;

procedure TdbgpCustomGetWatch.Execute(Respond: TDebugCommandRespond);
const
  //sCmd = 'property';
  sCmd = 'response';
var
  S: string;
begin
  if Respond[sCmd] <> nil then
  begin
    S := Respond[sCmd].Value;
    if (S <> '') and (Respond[sCmd].Attributes['encoding'] = 'base64') then //bug DecodeStringBase64 when S = ''
      Info.Value := DecodeStringBase64(S)
    else
      Info.Value := S;

    Info.VarType := Respond[sCmd].Attributes['type'];
  end
  else
  begin
    Info.VarType := '[ERROR]';
    Info.Value := '';
  end;
end;

{ TdbgpDebugWatches }

function TdbgpDebugWatches.GetCount: integer;
begin
  with FDebug.FServer do
    Result := Watches.Count;
end;

function TdbgpDebugWatches.GetItems(Index: integer): TDebugWatchInfo;
var
  aWt: TdbgpWatch;
begin
  with FDebug.FServer do
    aWt := Watches[Index];
  Result:= aWt.Info;
end;

procedure TdbgpDebugWatches.Clear;
begin
  with FDebug.FServer do
    Watches.Clear;
end;

procedure TdbgpDebugWatches.Add(vName: string);
begin
  with FDebug.FServer do
    Watches.AddWatch(vName);
end;

procedure TdbgpDebugWatches.Remove(vName: string);
begin
  with FDebug.FServer do
    Watches.RemoveWatch(vName);
end;

function TdbgpDebugWatches.GetValue(vName: string; out vValue: Variant; out vType: string; EvalIt: Boolean): boolean;
var
  aAction: TdbgpCustomGet;
begin
  Result := False;
  if dbsRunning in FDebug.GetState then   //there is a connection from XDebug
  begin
    if EvalIt then
      aAction := TdbgpEval.Create
    else
      aAction := TdbgpGetWatchInstance.Create;
    aAction.CreateEvent;
    aAction.Info.Name := vName;
    with FDebug.FServer do
    begin
      AddAction(aAction);

      Resume;

      aAction.Event.WaitFor(30000);
      vValue := aAction.Info.Value;
      vType := aAction.Info.VarType;

      ExtractAction(aAction);
      aAction.Free;

      Result := True;
    end;
  end;
end;

{ TdbgpDebugBreakPoints }

function TdbgpDebugBreakPoints.GetCount: integer;
begin
  with FDebug.FServer do
    Result := Breakpoints.Count;
end;

function TdbgpDebugBreakPoints.GetItems(Index: integer): TDebugBreakpointInfo;
var
  aBP: TdbgpBreakpoint;
begin
  with FDebug.FServer do
    aBP := Breakpoints[Index];
  Result.FileName := aBP.FileName;
  Result.Handle := aBP.Handle;
  Result.Line := aBP.Line;
end;

procedure TdbgpDebugBreakPoints.Clear;
begin
  with FDebug.FServer do
    Breakpoints.Clear;
end;

procedure TdbgpDebugBreakPoints.Toggle(FileName: string; LineNo: integer);
begin
  with FDebug.FServer do
    Breakpoints.Toggle(FileName, LineNo);
end;

function TdbgpDebugBreakPoints.IsExists(FileName: string; LineNo: integer): boolean;
begin
  with FDebug.FServer do
    Result := Breakpoints.Find(FileName, LineNo) <> nil;
end;

procedure TdbgpDebugBreakPoints.Add(FileName: string; LineNo: integer);
begin
  with FDebug.FServer do
    Breakpoints.Add(FileName, LineNo);
end;

procedure TdbgpDebugBreakPoints.Remove(FileName: string; Line: integer);
var
  aBP: TdbgpBreakpoint;
begin
  with FDebug.FServer do
    aBP := Breakpoints.Find(FileName, Line);
  if aBP <> nil then
    with FDebug.FServer do
      Breakpoints.Remove(aBP);
end;

procedure TdbgpDebugBreakPoints.Remove(Handle: integer);
begin
  with FDebug.FServer do
    Breakpoints.Remove(Handle);
end;

{ TdbgpDebug }

function TdbgpDebug.CreateBreakPoints: TEditorBreakPoints;
begin
  Result := TdbgpDebugBreakPoints.Create;
  (Result as TdbgpDebugBreakPoints).FDebug := Self;
end;

function TdbgpDebug.CreateWatches: TEditorWatches;
begin
  Result := TdbgpDebugWatches.Create;
  (Result as TdbgpDebugWatches).FDebug := Self;
end;

constructor TdbgpDebug.Create;
begin
  inherited Create;
  FServer := TdbgpServer.Create;
  //FServer.FDebug := Self;
end;

destructor TdbgpDebug.Destroy;
begin
  FreeAndNil(FServer);
  inherited;
end;

procedure TdbgpDebug.Action(AAction: TDebugAction);
begin
    case AAction of
    dbaStartServer: Start;
    dbaStopServer: Stop;
    dbaReset: Reset;
    dbaResume: Resume;
    dbaStepInto: StepInto;
    dbaStepOver: StepOver;
    dbaStepOut: StepOut;
    dbaRun: Run;
  end;
end;

function TdbgpDebug.GetState: TDebugStates;
begin
  Result := [];
  if FServer.Active then
    Result := Result + [dbsActive];
  if FServer.IsRuning then
    Result := Result + [dbsRunning];
end;

procedure TdbgpDebug.Start;
begin
  inherited;
  FServer.Start;
end;

procedure TdbgpDebug.Stop;
var
  aAction: TdbgpDetach;
begin
  inherited;
  if FServer.IsRuning then
  begin
    FServer.Clear;
    aAction := TdbgpDetach.Create;
    aAction.CreateEvent;
    FServer.AddAction(aAction);
    FServer.Resume;
    aAction.Event.WaitFor(30000);
    FServer.ExtractAction(aAction);
    aAction.Free;
  end;
  FServer.Stop;
end;

procedure TdbgpDebug.Reset;
begin
  FServer.Clear; //no need to any exists actions
  FServer.AddAction(TdbgpStop.Create);
  FServer.AddAction(TdbgpGetCurrent.Create);
  FServer.Resume;
end;

procedure TdbgpDebug.Resume;
begin
  FServer.AddAction(TdbgpDetach.Create);
  FServer.AddAction(TdbgpGetCurrent.Create);
  FServer.Resume;
end;

procedure TdbgpDebug.StepInto;
begin
  FServer.AddAction(TdbgpStepInto.Create);
  FServer.AddAction(TdbgpGetWatches.Create);
  FServer.AddAction(TdbgpGetCurrent.Create);
  FServer.Resume;
end;

procedure TdbgpDebug.StepOver;
begin
  FServer.AddAction(TdbgpStepOver.Create);
  FServer.AddAction(TdbgpGetWatches.Create);
  FServer.AddAction(TdbgpGetCurrent.Create);
  FServer.Resume;
end;

procedure TdbgpDebug.StepOut;
begin
  FServer.AddAction(TdbgpStepOut.Create);
  FServer.AddAction(TdbgpGetWatches.Create);
  FServer.AddAction(TdbgpGetCurrent.Create);
  FServer.Resume;
end;

procedure TdbgpDebug.Run;
begin
  FServer.AddAction(TdbgpRun.Create);
  FServer.AddAction(TdbgpGetWatches.Create);
  FServer.AddAction(TdbgpGetCurrent.Create);
  FServer.Resume;
end;

function TdbgpDebug.GetKey: string;
begin
  Result := FServer.Key;
end;

end.

