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
  SyncObjs, IniFiles, Base64,
  EditorRun,
  mnClasses, mnSockets, mnStreams, mnConnections, mnServers,
  mnXMLUtils, mnXMLRttiProfile, mnXMLNodes;

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
    procedure DoExecute(Respond: TDebugCommandRespond); virtual; abstract;
    procedure Execute(Respond: TDebugCommandRespond);
  public
  end;

  TdbgpActionClass = class of TdbgpAction;

  { TdbgpSpool }

  TdbgpSpool = class(specialize TmnObjectList<TdbgpAction>)
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

  { TdbgpInit }

  TdbgpInit = class(TdbgpAction)
  protected
    procedure Created; override;
  public
    function GetCommand: string; override;
    procedure DoExecute(Respond: TDebugCommandRespond); override;
  end;

  { TdbgpFeatureSet }

  TdbgpFeatureSet = class(TdbgpAction)
  protected
    FName: string;
    FValue: string;
    procedure DoExecute(Respond: TDebugCommandRespond); override;
  public
    constructor CreateBy(vName, vValue: string);
    function GetCommand: string; override;
  end;

  { TdbgpCommandSet }

  TdbgpCommandSet = class(TdbgpAction)
  protected
    FName: string;
    FValue: string;
    procedure DoExecute(Respond: TDebugCommandRespond); override;
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
    procedure DoExecute(Respond: TDebugCommandRespond); override;
  end;

  { TdbgpStepOver }

  TdbgpStepOver = class(TdbgpAction)
  public
    function GetCommand: string; override;
    procedure DoExecute(Respond: TDebugCommandRespond); override;
  end;

  { TdbgpStepInto }

  TdbgpStepInto = class(TdbgpAction)
  public
    function GetCommand: string; override;
    procedure DoExecute(Respond: TDebugCommandRespond); override;
  end;

  { TdbgpStepOut }

  TdbgpStepOut = class(TdbgpAction)
  public
    function GetCommand: string; override;
    procedure DoExecute(Respond: TDebugCommandRespond); override;
  end;

  { TdbgpRun }

  TdbgpRun = class(TdbgpAction)
  public
    function GetCommand: string; override;
    procedure DoExecute(Respond: TDebugCommandRespond); override;
  end;

  { TdbgpDetach }

  TdbgpDetach = class(TdbgpAction)
  public
    function GetCommand: string; override;
    procedure DoExecute(Respond: TDebugCommandRespond); override;
    destructor Destroy; override;
  end;

  { TdbgpStop }

  TdbgpStop = class(TdbgpAction)
  public
    function GetCommand: string; override;
    procedure DoExecute(Respond: TDebugCommandRespond); override;
  end;

  { TdbgpCustomGet }

  TdbgpCustomGet = class(TdbgpAction)
  public
    Info: TDebugWatchInfo;
  end;
  // Watches

  { TdbgpCustomGetWatch }

  TdbgpCustomGetWatch = class(TdbgpCustomGet)
  protected
  public
    function GetCommand: string; override;
    procedure DoExecute(Respond: TDebugCommandRespond); override;
  end;

  { TdbgpGetWatch }

  TdbgpGetWatch = class(TdbgpCustomGetWatch)
  protected
  public
    Index: integer;
    procedure DoExecute(Respond: TDebugCommandRespond); override;
  end;

  { TdbgpEval }

  TdbgpEval = class(TdbgpCustomGet)
  protected
  public
    function GetCommand: string; override;
    function GetData: string; override;
    procedure DoExecute(Respond: TDebugCommandRespond); override;
  end;

  { TdbgpGetWatchInstance }

  TdbgpGetWatchInstance = class(TdbgpCustomGetWatch)
  protected
  public
  end;

  { TdbgpGetWatches }

  TdbgpGetWatches = class(TdbgpCustomGetWatch)
  protected
  public
    Current: integer;
    function Stay: boolean; override;
    function Enabled: boolean; override;
    procedure DoExecute(Respond: TDebugCommandRespond); override;
  end;

  // Breakpoints

  { TdbgpModifyBreakpoint }

  TdbgpSetBreakpoint = class(TdbgpAction)
  protected
  public
    FileName: string;
    FileLine: integer;
    BreakpointID: cardinal;
    function GetCommand: string; override;
    procedure DoExecute(Respond: TDebugCommandRespond); override;
  end;

  { TdbgpRemoveBreakpoint }

  TdbgpRemoveBreakpoint = class(TdbgpAction)
  protected
    procedure DoExecute(Respond: TDebugCommandRespond); override;
  public
    BreakpointID: integer;
    function GetCommand: string; override;
  end;

  { TdbgpSetBreakpoints }

  TdbgpSetBreakpoints = class(TdbgpAction)
  protected
    Delete: Boolean;
    FileName: string;
    FileLine: integer;
    BreakpointID: cardinal;
    procedure CopyInfo;
  public
    Current: Integer;
    function GetCommand: string; override;
    function Enabled: Boolean; override;
    procedure DoExecute(Respond: TDebugCommandRespond); override;
    function Stay: Boolean; override;
  end;

//* Watches

  { TdbgpWatch }

  TdbgpWatch = class(TObject)
  private
    FHandle: integer;
  public
    Info: TDebugWatchInfo;
    property Handle: integer read FHandle write FHandle;
  published
  end;

  { TdbgpWatches }

  TdbgpWatches = class(specialize TmnObjectList<TdbgpWatch>)
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

  { TdbgpBreakpoint }

  TdbgpBreakpoint = class(TObject)
  private
    FDeleted: Boolean;
    FID: integer;
    FLine: integer;
    FHandle: Integer;
    FFileName: string;
  public
    property Handle: Integer read FHandle write FHandle;
    property ID: integer read FID write FID;
    property Deleted: Boolean read FDeleted write FDeleted;
  published
    property FileName: string read FFileName write FFileName;
    property Line: integer read FLine write FLine;
  end;

  { TdbgpBreakpoints }

  TdbgpBreakpoints = class(specialize TmnObjectList<TdbgpBreakpoint>)
  private
    CurrentHandle: Integer;
    FServer: TdbgpServer;
  protected
    property Server: TdbgpServer read FServer;
  public
    function Add(FileName: string; Line: integer): integer; overload;
    procedure Remove(Handle: Integer); overload;
    procedure ForceRemove(Handle: Integer); overload;
    function Find(Name: string; Line: integer; WithDeleted: Boolean = False): TdbgpBreakpoint; overload;
    procedure Toggle(FileName: string; Line: integer);
    procedure Clean;
  end;

  TdbgpOnServerEvent = procedure(Sender: TObject; Socket: TdbgpConnection) of object;

  { TdbgpConnection }

  TdbgpConnection = class(TmnServerConnection)
  private
    FSpool: TdbgpConnectionSpool;
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
    constructor Create(vOwner: TmnConnections; vStream: TmnConnectionStream);
    destructor Destroy; override;
    procedure Stop; override;
    property Key: string read FKey;
    property Server: TdbgpServer read GetServer;
  published
  end;

  { TmnDBGListener }

  TmnDBGListener = class(TmnListener)
  private
  protected
    function DoCreateConnection(vStream: TmnConnectionStream): TmnConnection; override;
  public
  end;

  { TdbgpServer }

  TdbgpServer = class(TmnServer)
  private
    FBreakOnFirstLine: Boolean;
    FQueue: TdbgpSpool;
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
    procedure DoBeforeOpen; override;
    procedure DoStop; override;
    property Queue: TdbgpSpool read FQueue;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Resume;
    procedure AddAction(vAction: TdbgpAction); overload;
    procedure AddAction(vActionClass: TdbgpActionClass); overload;
    procedure RemoveAction(vAction: TdbgpAction);
    procedure ExtractAction(vAction: TdbgpAction);
    procedure Clear;
    property IsRuning: Boolean read GetIsRuning;
    property Watches: TdbgpWatches read FWatches;
    property StackDepth: Integer read FStackDepth write FStackDepth;
    property Breakpoints: TdbgpBreakpoints read FBreakpoints;
    property BreakOnFirstLine: Boolean read FBreakOnFirstLine write FBreakOnFirstLine default False;
    property Key: string read FKey;
    property RunCount: Integer read FRunCount; //count of waiting action
  published
  end;

  TdbgpDebugger = class;

  { TdbgpDebuggerBreakPoints }

  TdbgpDebuggerBreakPoints = class(TEditorBreakPoints)
  protected
    FDebug: TdbgpDebugger;
    FBreakpoints: TdbgpBreakpoints; //Cache of undeleted breakpoints
    function GetCount: integer; override;
    function GetItems(Index: integer): TDebugBreakpointInfo; override;
    procedure UpdateBreakpoints;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Toggle(FileName: string; LineNo: integer); override;
    function IsExists(FileName: string; LineNo: integer): boolean; override;
    procedure Add(FileName: string; LineNo: integer); override;
    procedure Remove(Handle: integer); override; overload;
    procedure Remove(FileName: string; Line: integer); override; overload;
  end;

  { TdbgpDebuggerWatches }

  TdbgpDebuggerWatches = class(TEditorWatches)
  protected
    FDebug: TdbgpDebugger;
    function GetCount: integer; override;
    function GetItems(Index: integer): TDebugWatchInfo; override;
  public
    procedure Clear; override;
    procedure Add(vName: string); override;
    procedure Remove(vName: string); override;
    function GetValue(vName: string; out vValue: Variant; out vType: string; EvalIt: Boolean): boolean; override;
  end;

  { TdbgpDebugger }

  TdbgpDebugger = class(TEditorDebugger)
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

procedure TdbgpAction.Execute(Respond: TDebugCommandRespond);
begin
  DoExecute(Respond);
end;

{ TdbgpCommandSet }

procedure TdbgpCommandSet.DoExecute(Respond: TDebugCommandRespond);
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

procedure TdbgpEval.DoExecute(Respond: TDebugCommandRespond);
begin
end;

{ TdbgpFeatureSet }

procedure TdbgpFeatureSet.DoExecute(Respond: TDebugCommandRespond);
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
  FQueue := TdbgpSpool.Create(True);
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
  FreeAndNil(FQueue);
  inherited;
end;

function TdbgpServer.GetIsRuning: Boolean;
begin
  Result := RunCount > 0;
end;

constructor TdbgpConnection.Create(vOwner: TmnConnections; vStream: TmnConnectionStream);
begin
  inherited;
  FSpool := TdbgpConnectionSpool.Create;
  FSpool.FConnection := Self;
  //KeepAlive := True;
  Stream.ReadTimeout := 5000;
end;

destructor TdbgpConnection.Destroy;
begin
  FreeAndNil(FSpool);
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
      FSpool.Remove(aAction)
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
            FSpool.Extract(aAction)
          else
            FSpool.Remove(aAction);
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
  Stream.WriteString(s+#0);
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
  if FSpool.Count = 0 then
  begin
    InterLockedIncrement(Server.FRunCount);
    DebugManager.Event.WaitFor(INFINITE); //wait the ide to make resume
    InterLockedDecrement(Server.FRunCount);

    DebugManager.Enter;
    try
      i := 0;
      while i < Server.Queue.Count do
      begin
        aAction := Server.Queue.Extract(Server.Queue[i]);
        //        if aAction.Key = Key then
        FSpool.Add(aAction);
        //        else
        //        inc(i);
      end;
    finally
      DebugManager.Leave;
    end;
  end;
  Result := nil;
  while not Terminated and ((FSpool.Count > 0) and (Result = nil)) do
  begin
    Result := FSpool[0];
    Result.Prepare;
  end;
end;

procedure TdbgpConnection.Prepare;
begin
  inherited;
  Server.Breakpoints.Clean;

  FSpool.Add(TdbgpInit.Create);
  FSpool.Add(TdbgpFeatureSet.CreateBy('show_hidden', '1'));
  FSpool.Add(TdbgpFeatureSet.CreateBy('max_depth', IntToStr(Server.StackDepth)));
  FSpool.Add(TdbgpFeatureSet.CreateBy('max_children', '100'));

  FSpool.Add(TdbgpSetBreakpoints.Create);
  FSpool.Add(TdbgpCommandSet.CreateBy('breakpoint_set', '-t exception -X Error -s enabled'));
  FSpool.Add(TdbgpCommandSet.CreateBy('breakpoint_set', '-t exception -X Warning -s enabled'));
  { or
    breakpoint_set -t exception -X Error
    breakpoint_set -t exception -X Warning
    breakpoint_set -t exception -X Notice
  }

  if Server.BreakOnFirstLine then
  begin
    FSpool.Add(TdbgpStepInto.Create);
    FSpool.Add(TdbgpGetCurrent.Create);
  end
  else
  begin
    FSpool.Add(TdbgpRun.Create);
    FSpool.Add(TdbgpGetWatches.Create);
    FSpool.Add(TdbgpGetCurrent.Create);
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

function TmnDBGListener.DoCreateConnection(vStream: TmnConnectionStream): TmnConnection;
begin
  Result := TdbgpConnection.Create(Self, vStream);
end;

procedure TdbgpServer.DoStart;
begin
  inherited;
end;

procedure TdbgpServer.DoBeforeOpen;
begin
  Queue.Clear;
  inherited;
end;

procedure TdbgpServer.DoStop;
begin
  inherited;
  if FQueue <> nil then //DoStop class when Server free
    FQueue.Clear;
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

procedure TdbgpStepOver.DoExecute(Respond: TDebugCommandRespond);
begin
end;

{ TdbgpStepInto }

function TdbgpStepInto.GetCommand: string;
begin
  Result := 'step_into';
end;

procedure TdbgpStepInto.DoExecute(Respond: TDebugCommandRespond);
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

procedure TdbgpInit.DoExecute(Respond: TDebugCommandRespond);
begin
  DebugManager.Enter;
  try
    Connection.Server.Watches.Clean;
    Connection.FKey := Respond.Root.Attributes['idekey'];
  finally
    DebugManager.Leave;
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

procedure TdbgpGetCurrent.DoExecute(Respond: TDebugCommandRespond);
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

procedure TdbgpRun.DoExecute(Respond: TDebugCommandRespond);
begin
end;

{ TdbgpDetach }

function TdbgpDetach.GetCommand: string;
begin
  Result := 'detach';
end;

procedure TdbgpDetach.DoExecute(Respond: TDebugCommandRespond);
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

procedure TdbgpStop.DoExecute(Respond: TDebugCommandRespond);
begin
  Connection.Disconnect;
end;

{ TdbgpStepOut }

function TdbgpStepOut.GetCommand: string;
begin
  Result := 'step_out';
end;

procedure TdbgpStepOut.DoExecute(Respond: TDebugCommandRespond);
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
  DebugManager.Enter;
  try
    Add(Name, '');
  finally
    DebugManager.Leave;
  end;
  if Server.IsRuning then
  begin
    DebugManager.Enter;
    try
      Server.Queue.Add(TdbgpGetWatches.Create);
      Server.Queue.Add(TdbgpGetCurrent.Create);
    finally
      DebugManager.Leave;
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
    DebugManager.Enter;
    try
      Server.Queue.Add(TdbgpGetWatches.Create);
      Server.Queue.Add(TdbgpGetCurrent.Create);
    finally
      DebugManager.Leave;
    end;
    Server.Resume;
  end;
end;

procedure TdbgpWatches.SetValues(Name: string; const Value: variant);
begin
end;

{ TdbgpGetWatch }

procedure TdbgpGetWatch.DoExecute(Respond: TDebugCommandRespond);
begin
  inherited;
  DebugManager.Enter;
  try
    Connection.Server.Watches[Index].Info.Value := StringReplace(Info.Value, #13, '; ', [rfReplaceAll]);
    Connection.Server.Watches[Index].Info.VarType := Info.VarType;
  finally
    DebugManager.Leave;
  end;
end;

{ TdbgpBreakpoints }

function TdbgpBreakpoints.Add(FileName: string; Line: integer): integer;
var
  aBreakpoint: TdbgpBreakpoint;
begin
  Result := -1;
  Inc(CurrentHandle);
  aBreakpoint := Find(FileName, Line, True);
  if aBreakpoint = nil then
  begin
    aBreakpoint := TdbgpBreakpoint.Create;
    aBreakpoint.Handle := CurrentHandle;
    aBreakpoint.FileName := FileName;
    aBreakpoint.Line := Line;
    Result := Add(aBreakpoint);
  end
  else if aBreakpoint.Deleted then
    aBreakpoint.Deleted := False
  else
    Raise Exception.Create('Break point already exists');
end;

function TdbgpBreakpoints.Find(Name: string; Line: integer; WithDeleted: Boolean): TdbgpBreakpoint;
var
  aItem: TdbgpBreakpoint;
begin
  Result := nil;
  for aItem in Self do
  begin
    if (not aItem.Deleted or WithDeleted) and (aItem.line = Line) and SameText(aItem.FileName, Name) then
    begin
      Result := aItem;
      break;
    end;
  end;
end;

procedure TdbgpBreakpoints.Remove(Handle: Integer);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Items[i].Handle = Handle then
    begin
      if Items[i].ID > 0 then
        Items[i].Deleted := True
      else
        Delete(i);
      break;
    end;
  end;
end;

procedure TdbgpBreakpoints.ForceRemove(Handle: Integer);
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
  //aSetBreakpoint: TdbgpSetBreakpoint;
  //aRemoveBreakpoint: TdbgpRemoveBreakpoint;
begin
  aBreakpoint := Find(FileName, Line, True); //lookup it as normal
  if (aBreakpoint <> nil) and (not aBreakpoint.Deleted) then
  begin
    if aBreakpoint.ID > 0 then
      aBreakpoint.Deleted := True //Will be removed by action
    else
      Remove(ABreakpoint);
  end
  else
  begin
    if aBreakpoint <> nil then
      aBreakpoint.Deleted := False
    else
      Add(FileName, Line);
  end;
end;

procedure TdbgpBreakpoints.Clean;
var
  i: Integer;
begin
  i := 0;
  while i < Count do
  begin
    if Items[i].Deleted then
      Delete(i)
    else
    begin
      Items[i].ID := 0;
      Inc(i);
    end;
  end;
end;

{ TdbgpGetWatches }

function TdbgpGetWatches.Stay: boolean;
begin
  DebugManager.Enter;
  try
    Inc(Current);
    Result := Current < Connection.Server.Watches.Count;
  finally
    DebugManager.Leave;
  end;
end;

procedure TdbgpGetWatches.DoExecute(Respond: TDebugCommandRespond);
begin
  inherited;
  DebugManager.Enter;
  try
    Connection.Server.Watches[Current].Info.Value := StringReplace(Info.Value, #13, '; ', [rfReplaceAll]);
    Connection.Server.Watches[Current].Info.VarType := Info.VarType;
  finally
    DebugManager.Leave;
  end;
end;

function TdbgpGetWatches.Enabled: boolean;
begin
  DebugManager.Enter;
  try
    Result := Current < Connection.Server.Watches.Count;
    if Result then
      Info.Name := Connection.Server.Watches[Current].Info.Name;
  finally
    DebugManager.Leave;
  end;
end;

{ TdbgpSetBreakpoints }

procedure TdbgpSetBreakpoints.CopyInfo;
begin
  FileName := Connection.Server.Breakpoints[Current].FileName;
  FileLine := Connection.Server.Breakpoints[Current].Line;
  Delete := Connection.Server.Breakpoints[Current].Deleted;
  BreakpointID := Connection.Server.Breakpoints[Current].ID;
end;

function TdbgpSetBreakpoints.GetCommand: string;
begin
  if Delete then
    Result := 'breakpoint_remove -d ' + IntToStr(BreakpointID)
  else
    Result := 'breakpoint_set -t line -n ' + IntToStr(FileLine) + ' -f ' + FileNameToURI(FileName) + '';
end;

function TdbgpSetBreakpoints.Enabled: Boolean;
begin
  DebugManager.Enter;
  try
    Result := False;
    with Connection.Server do
    begin
      while Current < Breakpoints.Count do
      begin
        Result := (not Breakpoints[Current].Deleted) and (Breakpoints[Current].ID =0); //to add it
        Result := Result or (Breakpoints[Current].Deleted and (Breakpoints[Current].ID <> 0)); //to delete it

        if Result then
          break;
        Inc(Current);
      end;
    end;
    if Result then
      CopyInfo;
  finally
    DebugManager.Leave;
  end;
end;

function TdbgpSetBreakpoints.Stay: Boolean;
begin
  DebugManager.Enter;
  try
    Result := Current < Connection.Server.Breakpoints.Count;
  finally
    DebugManager.Leave;
  end;
end;

procedure TdbgpSetBreakpoints.DoExecute(Respond: TDebugCommandRespond);
begin
  if Delete then
  begin
    DebugManager.Enter;
    try
      Connection.Server.Breakpoints[Current].ID := 0;
      Connection.Server.Breakpoints.Delete(Current);
    finally
      DebugManager.Leave;
    end;
  end
  else
  begin
    CheckError(Respond);
    DebugManager.Enter;
    try
      Connection.Server.Breakpoints[Current].ID := StrToInt(Respond.Root.Attributes['id']);
    finally
      DebugManager.Leave;
    end;
    Inc(Current);
  end;
end;

{ TdbgpSetBreakpoint }

function TdbgpSetBreakpoint.GetCommand: string;
begin
  Result := 'breakpoint_set -t line -n ' + IntToStr(FileLine) + ' -f ' + FileNameToURI(FileName) + '';
end;

procedure TdbgpSetBreakpoint.DoExecute(Respond: TDebugCommandRespond);
begin
  CheckError(Respond);
  BreakpointID := StrToInt(Respond.Root.Attributes['id']);
end;

{ TdbgpRemoveBreakpoint }

procedure TdbgpRemoveBreakpoint.DoExecute(Respond: TDebugCommandRespond);
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

procedure TdbgpServer.AddAction(vAction: TdbgpAction);
begin
  DebugManager.Enter;
  try
    Queue.Add(vAction);
  finally
    DebugManager.Leave;
  end;
end;

procedure TdbgpServer.AddAction(vActionClass: TdbgpActionClass);
begin
  AddAction(vActionClass.Create);
end;

procedure TdbgpServer.RemoveAction(vAction: TdbgpAction);
begin
  DebugManager.Enter;
  try
    Queue.Remove(vAction);
  finally
    DebugManager.Leave;
  end;
end;

procedure TdbgpServer.ExtractAction(vAction: TdbgpAction);
begin
  DebugManager.Enter;
  try
    Queue.Extract(vAction);
  finally
    DebugManager.Leave;
  end;
end;

procedure TdbgpServer.Clear;
begin
  DebugManager.Enter;
  try
    Queue.Clear;
  finally
    DebugManager.Leave;
  end;
end;

{ TdbgpCustomGetWatch }

function TdbgpCustomGetWatch.GetCommand: string;
begin
  Result := 'property_value -n "' + Info.Name + '" -m 1024';
  //Result := 'property_get -n "' + Name + '" -m 1024';
end;

(*
<?xml version="1.0" encoding="iso-8859-1"?>'#10'
<response
    xmlns="urn:debugger_protocol_v1"
    xmlns:xdebug="https://xdebug.org/dbgp/xdebug" command="property_value" transaction_id="13" type="array" children="1" numchildren="2" page="0" pagesize="100">
    <property name="0" fullname="$a[0]" type="string" size="2" encoding="base64">
        <![CDATA[dDE=]]>
    </property>
    <property name="1" fullname="$a[1]" type="string" size="2" encoding="base64">
        <![CDATA[dDI=]]>
    </property>
</response>
*)
procedure TdbgpCustomGetWatch.DoExecute(Respond: TDebugCommandRespond);
const
  //sCmd = 'property';
  sCmd = 'response';
var
  S: string;
  v: string;
  i: integer;
begin
  if Respond[sCmd] <> nil then
  begin
    S := Respond[sCmd].Value;
    if S = '' then
    begin
      if StrToIntDef(Respond[sCmd].Attributes['numchildren'], 0) > 0 then
      begin
        for i := 0 to Respond.Root.Count -1 do
        begin
          if Respond.Root[i].Name = 'property' then
          begin
            v := Respond.Root[i].Value;
            if (Respond.Root[i].Attributes['encoding'] = 'base64') then
              v := DecodeStringBase64(v);
            if S <> '' then
              S := S + #13;
            if Respond.Root[i].Attributes['type'] = 'string' then
              v := '''' + v + '''';
            S := S + Respond.Root[i].Attributes['fullname'] + ': ' + Respond.Root[i].Attributes['type'] + '= '+ v;
          end;
        end;
      end;
    end
    else if (Respond[sCmd].Attributes['encoding'] = 'base64') then
      S := DecodeStringBase64(S);

    Info.VarType := Respond[sCmd].Attributes['type'];
    if Info.VarType = 'string' then
      Info.Value := '''' + S + ''''
    else
      Info.Value := S;
  end
  else
  begin
    Info.VarType := '[ERROR]';
    Info.Value := '';
  end;
end;

{ TdbgpDebuggerWatches }

function TdbgpDebuggerWatches.GetCount: integer;
begin
  with FDebug.FServer do
    Result := Watches.Count;
end;

function TdbgpDebuggerWatches.GetItems(Index: integer): TDebugWatchInfo;
var
  aWt: TdbgpWatch;
begin
  with FDebug.FServer do
    aWt := Watches[Index];
  Result:= aWt.Info;
end;

procedure TdbgpDebuggerWatches.Clear;
begin
  with FDebug.FServer do
    Watches.Clear;
end;

procedure TdbgpDebuggerWatches.Add(vName: string);
begin
  with FDebug.FServer do
    Watches.AddWatch(vName);
end;

procedure TdbgpDebuggerWatches.Remove(vName: string);
begin
  with FDebug.FServer do
    Watches.RemoveWatch(vName);
end;

function TdbgpDebuggerWatches.GetValue(vName: string; out vValue: Variant; out vType: string; EvalIt: Boolean): boolean;
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

{ TdbgpDebuggerBreakPoints }

function TdbgpDebuggerBreakPoints.GetCount: integer;
begin
  Result := FBreakpoints.Count;
end;

function TdbgpDebuggerBreakPoints.GetItems(Index: integer): TDebugBreakpointInfo;
var
  aBP: TdbgpBreakpoint;
begin
  aBP := FBreakpoints[Index];
  Result.FileName := aBP.FileName;
  Result.Handle := aBP.Handle;
  Result.Line := aBP.Line;
end;

procedure TdbgpDebuggerBreakPoints.UpdateBreakpoints;
var
  aItem: TdbgpBreakpoint;
begin
  FBreakpoints.Clear;
  for aItem in FDebug.FServer.Breakpoints do
    if not aItem.Deleted then
      FBreakpoints.Add(aItem);
end;

constructor TdbgpDebuggerBreakPoints.Create;
begin
  inherited Create;
  FBreakpoints := TdbgpBreakpoints.Create(False);
end;

destructor TdbgpDebuggerBreakPoints.Destroy;
begin
  FreeAndNil(FBreakpoints);
  inherited Destroy;
end;

procedure TdbgpDebuggerBreakPoints.Clear;
begin
  with FDebug.FServer do
    Breakpoints.Clear;
  UpdateBreakpoints;
end;

procedure TdbgpDebuggerBreakPoints.Toggle(FileName: string; LineNo: integer);
begin
  with FDebug.FServer do
    Breakpoints.Toggle(FileName, LineNo);
  UpdateBreakpoints;
end;

function TdbgpDebuggerBreakPoints.IsExists(FileName: string; LineNo: integer): boolean;
begin
  Result := FBreakpoints.Find(FileName, LineNo) <> nil;
end;

procedure TdbgpDebuggerBreakPoints.Add(FileName: string; LineNo: integer);
begin
  with FDebug.FServer do
    Breakpoints.Add(FileName, LineNo);
  UpdateBreakpoints;
end;

procedure TdbgpDebuggerBreakPoints.Remove(FileName: string; Line: integer);
var
  aBP: TdbgpBreakpoint;
begin
  with FDebug.FServer do
    aBP := Breakpoints.Find(FileName, Line);

  if aBP <> nil then
      Remove(aBP.Handle);
end;

procedure TdbgpDebuggerBreakPoints.Remove(Handle: integer);
begin
  with FDebug.FServer do
    Breakpoints.Remove(Handle);
  UpdateBreakpoints;
end;

{ TdbgpDebugger }

function TdbgpDebugger.CreateBreakPoints: TEditorBreakPoints;
begin
  Result := TdbgpDebuggerBreakPoints.Create;
  (Result as TdbgpDebuggerBreakPoints).FDebug := Self;
end;

function TdbgpDebugger.CreateWatches: TEditorWatches;
begin
  Result := TdbgpDebuggerWatches.Create;
  (Result as TdbgpDebuggerWatches).FDebug := Self;
end;

constructor TdbgpDebugger.Create;
begin
  inherited Create;
  FServer := TdbgpServer.Create;
  //FServer.FDebug := Self;
end;

destructor TdbgpDebugger.Destroy;
begin
  FreeAndNil(FServer);
  inherited;
end;

procedure TdbgpDebugger.Action(AAction: TDebugAction);
begin
    case AAction of
    dbaActivate: Start;
    dbaDeactivate: Stop;
    dbaReset: Reset;
    dbaResume: Resume;
    dbaStepInto: StepInto;
    dbaStepOver: StepOver;
    dbaStepOut: StepOut;
    dbaRun: Run;
  end;
end;

function TdbgpDebugger.GetState: TDebugStates;
begin
  Result := [];
  if FServer.Active then
    Result := Result + [dbsActive];
  if FServer.IsRuning then
    Result := Result + [dbsRunning, dbsDebugging];
end;

procedure TdbgpDebugger.Start;
begin
  inherited;
  FServer.Start;
end;

procedure TdbgpDebugger.Stop;
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

procedure TdbgpDebugger.Reset;
begin
  FServer.Clear; //no need to any exists actions
  FServer.AddAction(TdbgpStop.Create);
  FServer.AddAction(TdbgpGetCurrent.Create);
  FServer.Resume;
end;

procedure TdbgpDebugger.Resume;
begin
  FServer.AddAction(TdbgpDetach.Create);
  FServer.AddAction(TdbgpGetCurrent.Create);
  FServer.Resume;
end;

procedure TdbgpDebugger.StepInto;
begin
  FServer.AddAction(TdbgpSetBreakpoints.Create);
  FServer.AddAction(TdbgpStepInto.Create);
  FServer.AddAction(TdbgpGetWatches.Create);
  FServer.AddAction(TdbgpGetCurrent.Create);
  FServer.Resume;
end;

procedure TdbgpDebugger.StepOver;
begin
  FServer.AddAction(TdbgpSetBreakpoints.Create);
  FServer.AddAction(TdbgpStepOver.Create);
  FServer.AddAction(TdbgpGetWatches.Create);
  FServer.AddAction(TdbgpGetCurrent.Create);
  FServer.Resume;
end;

procedure TdbgpDebugger.StepOut;
begin
  FServer.AddAction(TdbgpSetBreakpoints.Create);
  FServer.AddAction(TdbgpStepOut.Create);
  FServer.AddAction(TdbgpGetWatches.Create);
  FServer.AddAction(TdbgpGetCurrent.Create);
  FServer.Resume;
end;

procedure TdbgpDebugger.Run;
begin
  FServer.AddAction(TdbgpSetBreakpoints.Create);
  FServer.AddAction(TdbgpRun.Create);
  FServer.AddAction(TdbgpGetWatches.Create);
  FServer.AddAction(TdbgpGetCurrent.Create);
  FServer.Resume;
end;

function TdbgpDebugger.GetKey: string;
begin
  Result := FServer.Key;
end;

end.

