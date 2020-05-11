unit LuaDBGServers;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license   MIT (http://www.gnu.org/licenses/mit.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
  *}
{
  https://bitbucket.org/sylvanaar2/lua-for-idea/src/f554ad3b78e9/src/main/resources/mobdebug/?at=idea16
}

{$ifdef WINDOWS}
{.$DEFINE SAVELOG}
{$endif}

interface

uses
  SysUtils, StrUtils, Classes, Contnrs, Dialogs, Variants,
  mnSockets, mnStreams, mnConnections, mnServers, mnXMLUtils, Base64,
  mnXMLRttiProfile, mnXMLNodes, SyncObjs, IniFiles, EditorRun, mnClasses;

type
  TLuaDBGServer = class;
  TLuaDBGConnection = class;
  TLuaDBGConnectionClass = class of TLuaDBGConnection;

  TDebugCommandRespond = class(TmnXMLNodes)
  public
    Source: string;
  end;

  { TLuaDBGAction }

  TLuaDBGAction = class(TDebugCommand)
  private
    FConnection: TLuaDBGConnection;
  protected
    FTransactionID: integer;
    property Connection: TLuaDBGConnection read FConnection;
    procedure CheckError(Respond: TDebugCommandRespond);
    procedure Execute(Respond: TDebugCommandRespond); virtual; abstract;
  public
  end;

  TLuaDBGActionClass = class of TLuaDBGAction;

  TLuaDBGSpool = class(specialize TmnObjectList<TLuaDBGAction>)
  private
  public
  end;

  { TLuaDBGConnectionSpool }

  TLuaDBGConnectionSpool = class(TLuaDBGSpool)
  private
    FConnection: TLuaDBGConnection;
  public
    procedure Added(Action: TLuaDBGAction); override;
  end;

  TLuaDBGInit = class(TLuaDBGAction)
  protected
    procedure Created; override;
  public
    function GetCommand: string; override;
    procedure Execute(Respond: TDebugCommandRespond); override;
  end;

  { TLuaDBGFeatureSet }

  TLuaDBGFeatureSet = class(TLuaDBGAction)
  protected
    FName: string;
    FValue: string;
    procedure Execute(Respond: TDebugCommandRespond); override;
  public
    constructor CreateBy(vName, vValue: string);
    function GetCommand: string; override;
  end;

  { TLuaDBGCommandSet }

  TLuaDBGCommandSet = class(TLuaDBGAction)
  protected
    FName: string;
    FValue: string;
    procedure Execute(Respond: TDebugCommandRespond); override;
  public
    constructor CreateBy(vName, vValue: string);
    function GetCommand: string; override;
  end;


  { TLuaDBGGetCurrent }

  TLuaDBGGetCurrent = class(TLuaDBGAction)
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

  TLuaDBGStep = class(TLuaDBGAction)
  public
    function GetCommand: string; override;
    procedure Execute(Respond: TDebugCommandRespond); override;
  end;

  TLuaDBGStepInto = class(TLuaDBGAction)
  public
    function GetCommand: string; override;
    procedure Execute(Respond: TDebugCommandRespond); override;
  end;

  TLuaDBGStepOut = class(TLuaDBGAction)
  public
    function GetCommand: string; override;
    procedure Execute(Respond: TDebugCommandRespond); override;
  end;

  TLuaDBGRun = class(TLuaDBGAction)
  public
    function GetCommand: string; override;
    procedure Execute(Respond: TDebugCommandRespond); override;
  end;

  { TLuaDBGDetach }

  TLuaDBGDetach = class(TLuaDBGAction)
  public
    function GetCommand: string; override;
    procedure Execute(Respond: TDebugCommandRespond); override;
    destructor Destroy; override;
  end;

  TLuaDBGStop = class(TLuaDBGAction)
  public
    function GetCommand: string; override;
    procedure Execute(Respond: TDebugCommandRespond); override;
  end;

  TLuaDBGCustomGet = class(TLuaDBGAction)
  public
    Info: TDebugWatchInfo;
  end;
  // Watches

  TLuaDBGCustomGetWatch = class(TLuaDBGCustomGet)
  protected
  public
    function GetCommand: string; override;
    procedure Execute(Respond: TDebugCommandRespond); override;
  end;

  TLuaDBGGetWatch = class(TLuaDBGCustomGetWatch)
  protected
  public
    Index: integer;
    procedure Execute(Respond: TDebugCommandRespond); override;
  end;

  { TLuaDBGEval }

  TLuaDBGEval = class(TLuaDBGCustomGet)
  protected
  public
    function GetCommand: string; override;
    function GetData: string; override;
    procedure Execute(Respond: TDebugCommandRespond); override;
  end;

  { TLuaDBGGetWatchInstance }

  TLuaDBGGetWatchInstance = class(TLuaDBGCustomGetWatch)
  protected
  public
  end;

  TLuaDBGGetWatches = class(TLuaDBGCustomGetWatch)
  protected
  public
    Current: integer;
    function Stay: boolean; override;
    function Enabled: boolean; override;
    procedure Execute(Respond: TDebugCommandRespond); override;
  end;

  // Breakpoints

  TLuaDBGSetBreakpoint = class(TLuaDBGAction)
  protected
  public
    BreakpointID: cardinal;
    FileName: string;
    FileLine: integer;
    function GetCommand: string; override;
    procedure Execute(Respond: TDebugCommandRespond); override;
  end;

  TLuaDBGSetBreakpoints = class(TLuaDBGSetBreakpoint)
  protected
  public
    Current: integer;
    function Enabled: boolean; override;
    function Stay: boolean; override;
  end;

  { TLuaDBGRemoveBreakpoint }

  TLuaDBGRemoveBreakpoint = class(TLuaDBGAction)
  protected
    procedure Execute(Respond: TDebugCommandRespond); override;
  public
    BreakpointID: integer;
    function GetCommand: string; override;
  end;

//* Watches

  TLuaDBGWatch = class(TObject)
  private
    FHandle: integer;
  public
    Info: TDebugWatchInfo;
    property Handle: integer read FHandle write FHandle;
  published
  end;

  TLuaDBGWatches = class(specialize TmnObjectList<TLuaDBGWatch>)
  private
    FServer: TLuaDBGServer;
    CurrentHandle: integer;
    function GetValues(Name: string): variant;
    procedure SetValues(Name: string; const Value: variant);
  protected
    property Server: TLuaDBGServer read FServer;
  public
    function Find(Name: string): TLuaDBGWatch;
    function Add(Name: string; Value: variant): integer; overload;
    procedure AddWatch(Name: string);
    procedure RemoveWatch(Name: string);
    procedure Clean;
    property Values[Name: string]: variant read GetValues write SetValues;
  end;

//* Breakpoints

  TLuaDBGBreakpoint = class(TObject)
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

  TLuaDBGBreakpoints = class(specialize TmnObjectList<TLuaDBGBreakpoint>)
  private
    CurrentHandle: integer;
    FServer: TLuaDBGServer;
  protected
    property Server: TLuaDBGServer read FServer;
  public
    function Remove(Breakpoint: TLuaDBGBreakpoint): integer; overload;
    procedure Remove(Handle: integer); overload;
    function Add(FileName: string; Line: integer): integer; overload;
    function Find(Name: string; Line: integer): TLuaDBGBreakpoint;
    procedure Toggle(FileName: string; Line: integer);
  end;

  TLuaDBGOnServerEvent = procedure(Sender: TObject; Socket: TLuaDBGConnection) of object;

  { TLuaDBGConnection }

  TLuaDBGConnection = class(TmnServerConnection)
  private
    FLocalSpool: TLuaDBGConnectionSpool;
    FKey: string;
    function GetServer: TLuaDBGServer;
  public
    FTransactionID: integer;
  protected
    function NewTransactionID: integer;
{$IFDEF SAVELOG}
    procedure SaveLog(s: string);
{$ENDIF}
    function ReadRespond: TDebugCommandRespond;
    function PopAction: TLuaDBGAction;
    function SendCommand(Command: string; Data: string): integer;
    procedure Prepare; override;
    procedure DoExecute;
    procedure Process; override;
  public
    constructor Create(vOwner: TmnConnections; vStream: TmnConnectionStream); override;
    destructor Destroy; override;
    procedure Stop; override;
    property Key: string read FKey;
    property Server: TLuaDBGServer read GetServer;
  published
  end;

  { TmnDBGListener }

  TmnDBGListener = class(TmnListener)
  private
  protected
    function DoCreateConnection(vStream: TmnConnectionStream): TmnConnection; override;
  public
    destructor Destroy; override;
  end;

  { TLuaDBGServer }

  TLuaDBGServer = class(TmnServer)
  private
    FBreakOnFirstLine: Boolean;
    FSpool: TLuaDBGSpool;
    FStackDepth: Integer;
    FWatches: TLuaDBGWatches;
    FBreakpoints: TLuaDBGBreakpoints;
    FRunCount: Integer;
    FKey: string;
    function GetIsRuning: Boolean;
  protected
    function CreateListener: TmnListener; override;
    procedure DoChanged(vListener: TmnListener); override;
    procedure DoStart; override;
    procedure DoStop; override;
    property Spool: TLuaDBGSpool read FSpool;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Resume;
    procedure AddAction(Action: TLuaDBGAction); overload;
    procedure AddAction(ActionClass: TLuaDBGActionClass); overload;
    procedure RemoveAction(Action: TLuaDBGAction);
    procedure ExtractAction(Action: TLuaDBGAction);
    procedure Clear;
    property RunCount: Integer read FRunCount;
    property IsRuning: Boolean read GetIsRuning;
    property Watches: TLuaDBGWatches read FWatches;
    property StackDepth: Integer read FStackDepth write FStackDepth;
    property Breakpoints: TLuaDBGBreakpoints read FBreakpoints;
    property BreakOnFirstLine: Boolean read FBreakOnFirstLine write FBreakOnFirstLine default False;
    property Key: string read FKey;
  published
  end;

  TLuaDBGDebug = class;

  { TLuaDBGDebugBreakPoints }

  TLuaDBGDebugBreakPoints = class(TEditorBreakPoints)
  protected
    FDebug: TLuaDBGDebug;
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

  { TLuaDBGDebugWatches }

  TLuaDBGDebugWatches = class(TEditorWatches)
  protected
    FDebug: TLuaDBGDebug;
    function GetCount: integer; override;
    function GetItems(Index: integer): TDebugWatchInfo; override;
  public
    procedure Clear; override;
    procedure Add(vName: string); override;
    procedure Remove(vName: string); override;
    function GetValue(vName: string; out vValue: Variant; out vType: string; EvalIt: Boolean): boolean; override;
  end;

  { TLuaDBGDebug }

  TLuaDBGDebug = class(TEditorDebugger)
  private
    FServer: TLuaDBGServer;
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

{ TLuaDBGAction }

procedure TLuaDBGAction.CheckError(Respond: TDebugCommandRespond);
begin
  if (Respond.Root <> nil) then
    if StrToIntDef(Respond.GetAttribute('response', 'transaction_id'), -1) <> FTransactionID then
      raise EDebugException.Create('transaction_id is not same with command.'#13 + Respond.Source);
end;

{ TLuaDBGCommandSet }

procedure TLuaDBGCommandSet.Execute(Respond: TDebugCommandRespond);
begin
end;

constructor TLuaDBGCommandSet.CreateBy(vName, vValue: string);
begin
  Create;
  FName := vName;
  FValue:= vValue;
end;

function TLuaDBGCommandSet.GetCommand: string;
begin
  Result := FName + ' ' + FValue;
end;

{ TLuaDBGEval }

function TLuaDBGEval.GetCommand: string;
begin
  Result := 'eval';
end;

function TLuaDBGEval.GetData: string;
begin
  Result := 'echo ' + Info.Name;
end;

procedure TLuaDBGEval.Execute(Respond: TDebugCommandRespond);
begin
end;

{ TLuaDBGFeatureSet }

procedure TLuaDBGFeatureSet.Execute(Respond: TDebugCommandRespond);
begin
end;

constructor TLuaDBGFeatureSet.CreateBy(vName, vValue: string);
begin
  Create;
  FName := vName;
  FValue:= vValue;
end;

function TLuaDBGFeatureSet.GetCommand: string;
begin
  // 'feature_set -n show_hidden -v 1';
  Result := 'feature_set -n ' + FName + ' -v '+ FValue;
end;

constructor TLuaDBGServer.Create;
begin
  inherited;
  FSpool := TLuaDBGSpool.Create(True);
  Port := '8172'; // MOBDEBUG_PORT
  FStackDepth := 10;
  FWatches := TLuaDBGWatches.Create;
  FWatches.FServer := Self;
  FBreakpoints := TLuaDBGBreakpoints.Create;
  FBreakpoints.FServer := Self;
  FBreakpoints.FServer := Self;
  FBreakOnFirstLine := False;
end;

destructor TLuaDBGServer.Destroy;
begin
  FreeAndNil(FWatches);
  FreeAndNil(FBreakpoints);
  FreeAndNil(FSpool);
  inherited;
end;

function TLuaDBGServer.GetIsRuning: Boolean;
begin
  Result := RunCount > 0;
end;

constructor TLuaDBGConnection.Create(vOwner: TmnConnections; vStream: TmnConnectionStream);
begin
  inherited;
  FLocalSpool := TLuaDBGConnectionSpool.Create;
  FLocalSpool.FConnection := Self;
  //KeepAlive := True;
  Stream.Timeout := 5000;
end;

destructor TLuaDBGConnection.Destroy;
begin
  FreeAndNil(FLocalSpool);
  inherited;
end;

{ TLuaDBGConnection }

function TLuaDBGConnection.NewTransactionID: integer;
begin
  Inc(FTransactionID);
  Result := FTransactionID;
end;

procedure TLuaDBGGetCurrent.ShowFile; //this function must Synchronize
begin
  Engine.DebugLink.SetExecutedLine(FCurKey, FCurFile, FCurLine, FCallStack);
end;

procedure TLuaDBGConnection.DoExecute;
var
  aAction: TLuaDBGAction;
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

{ TLuaDBGSocketServer }

function TLuaDBGServer.CreateListener: TmnListener;
begin
  Result := TmnDBGListener.Create([]);
end;

function TLuaDBGConnection.ReadRespond: TDebugCommandRespond;
var
  Reader: TmnXMLNodeReader;
  s, r: string;
  aMatched: boolean;
begin
  Result := nil;
  r := '';
  Stream.ReadUntil(#10, true, s, aMatched);
  if Connected and aMatched and (S <> '') then
  begin
    Result := TDebugCommandRespond.Create;
    repeat
      //Stream.Connected
      Stream.ReadUntil(#10, true, s, aMatched);
      s := Trim(s);
      r := r + s;
    until s = '';

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
procedure TLuaDBGConnection.SaveLog(s: string);
var
  aStrings: TStringList;
  aStream: TFileStream;
  i: integer;
const
  sFile = 'c:\mobdebug_server.log';
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

function TLuaDBGConnection.SendCommand(Command: string; Data: string): integer;
var
  s: string;
begin
  Result := NewTransactionID;
  s := Command ;//+ ' -i ' + IntToStr(Result);
  if Data <> '' then
    s := s + ' ' + Data;
  Stream.WriteLine(s);
{$IFDEF SAVELOG}
  SaveLog(s);
{$ENDIF}
end;

function TLuaDBGConnection.GetServer: TLuaDBGServer;
begin
  Result := (Listener.Server as TLuaDBGServer);
end;

function TLuaDBGConnection.PopAction: TLuaDBGAction;
var
  aAction: TLuaDBGAction;
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
        aAction := Server.Spool.Extract(Server.Spool[i]) as TLuaDBGAction;
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

procedure TLuaDBGConnection.Prepare;
begin
  inherited;
  //FLocalSpool.Add(TLuaDBGInit.Create);
  FLocalSpool.Add(TLuaDBGStep.Create);
  {FLocalSpool.Add(TLuaDBGFeatureSet.CreateBy('show_hidden', '1'));
  FLocalSpool.Add(TLuaDBGFeatureSet.CreateBy('max_depth', IntToStr(Server.StackDepth)));
  FLocalSpool.Add(TLuaDBGFeatureSet.CreateBy('max_children', '100'));}

  FLocalSpool.Add(TLuaDBGSetBreakpoints.Create);
  FLocalSpool.Add(TLuaDBGCommandSet.CreateBy('breakpoint_set', '-t exception -X Error -s enabled'));
  FLocalSpool.Add(TLuaDBGCommandSet.CreateBy('breakpoint_set', '-t exception -X Warning -s enabled'));
  { or
    breakpoint_set -t exception -X Error
    breakpoint_set -t exception -X Warning
    breakpoint_set -t exception -X Notice
  }

  if Server.BreakOnFirstLine then
  begin
    FLocalSpool.Add(TLuaDBGStepInto.Create);
    FLocalSpool.Add(TLuaDBGGetCurrent.Create);
  end
  else
  begin
    FLocalSpool.Add(TLuaDBGRun.Create);
    FLocalSpool.Add(TLuaDBGGetWatches.Create);
    FLocalSpool.Add(TLuaDBGGetCurrent.Create);
  end;
end;

procedure TLuaDBGConnection.Process;
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

procedure TLuaDBGConnection.Stop;
begin
  inherited;
  DebugManager.Event.SetEvent;
end;

{ TmnDBGListener }

function TmnDBGListener.DoCreateConnection(vStream: TmnConnectionStream): TmnConnection;
begin
  Result := TLuaDBGConnection.Create(Self, vStream);
end;

destructor TmnDBGListener.Destroy;
begin
  inherited;
end;

procedure TLuaDBGServer.DoStart;
begin
  Spool.Clear;
  inherited;
end;

procedure TLuaDBGServer.DoStop;
begin
  inherited;
  if FSpool <> nil then //DoStop class when Server free
    FSpool.Clear;
end;

procedure TLuaDBGServer.DoChanged(vListener: TmnListener);
begin
  inherited;
  if vListener.Count = 0 then //TODO: i am not sure in Linux
    Engine.DebugLink.SetExecutedLine('', '', 0);
end;

{ TLuaDBGStep }

function TLuaDBGStep.GetCommand: string;
begin
  Result := 'STEP';
end;

procedure TLuaDBGStep.Execute(Respond: TDebugCommandRespond);
begin
end;

{ TLuaDBGStepInto }

function TLuaDBGStepInto.GetCommand: string;
begin
  Result := 'step_into';
end;

procedure TLuaDBGStepInto.Execute(Respond: TDebugCommandRespond);
begin
end;

procedure TLuaDBGServer.Resume;
begin
  DebugManager.Event.SetEvent;
end;

{ TLuaDBGInit }

procedure TLuaDBGInit.Created;
begin
  inherited;
  Flags := Flags + [dafSend];
end;

function TLuaDBGInit.GetCommand: string;
begin
  Result := 'init';
end;

procedure TLuaDBGInit.Execute(Respond: TDebugCommandRespond);
begin
  DebugManager.Lock.Enter;
  try
    Connection.Server.Watches.Clean;
    Connection.FKey := Respond.Root.Attributes['idekey'];
  finally
    DebugManager.Lock.Leave;
  end;
end;

{ TLuaDBGGetCurrent }

function TLuaDBGGetCurrent.GetCommand: string;
{var
  aDepth: Integer;}
begin
  //aDepth := Connection.Server.StackDepth;
  Result := 'stack_get';
{  if aDepth > 0 then
    Result := Result + ' -d ' + IntToStr(aDepth);}
end;

procedure TLuaDBGGetCurrent.Execute(Respond: TDebugCommandRespond);
var
  i: Integer;
begin
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

procedure TLuaDBGGetCurrent.Created;
begin
  inherited;
  Flags := Flags + [dafCheckError];
end;

destructor TLuaDBGGetCurrent.Destroy;
begin
  FreeAndNil(FCallStack);
  inherited Destroy;
end;

{ TLuaDBGRun }

function TLuaDBGRun.GetCommand: string;
begin
  Result := 'run';
end;

procedure TLuaDBGRun.Execute(Respond: TDebugCommandRespond);
begin
end;

{ TLuaDBGDetach }

function TLuaDBGDetach.GetCommand: string;
begin
  Result := 'detach';
end;

procedure TLuaDBGDetach.Execute(Respond: TDebugCommandRespond);
begin
  Connection.Disconnect;
end;

destructor TLuaDBGDetach.Destroy;
begin
  inherited Destroy;
end;

{ TLuaDBGStop }

function TLuaDBGStop.GetCommand: string;
begin
  Result := 'stop';
end;

procedure TLuaDBGStop.Execute(Respond: TDebugCommandRespond);
begin
  Connection.Disconnect;
end;

{ TLuaDBGStepOut }

function TLuaDBGStepOut.GetCommand: string;
begin
  Result := 'step_out';
end;

procedure TLuaDBGStepOut.Execute(Respond: TDebugCommandRespond);
begin
end;

{ TLuaDBGWatches }

function TLuaDBGWatches.Add(Name: string; Value: variant): integer;
var
  aWatch: TLuaDBGWatch;
begin
  Inc(CurrentHandle);
  aWatch := TLuaDBGWatch.Create;
  aWatch.Handle := CurrentHandle;
  aWatch.Info.Name := Name;
  aWatch.Info.VarType := '';
  aWatch.Info.Value := Value;
  Result := Add(aWatch);
end;

procedure TLuaDBGWatches.AddWatch(Name: string);
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
      Server.Spool.Add(TLuaDBGGetWatches.Create);
      Server.Spool.Add(TLuaDBGGetCurrent.Create);
    finally
      DebugManager.Lock.Leave;
    end;
    Server.Resume;
  end;
end;

procedure TLuaDBGWatches.Clean;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    VarClear(Items[i].Info.Value);
  end;
end;

function TLuaDBGWatches.Find(Name: string): TLuaDBGWatch;
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

function TLuaDBGWatches.GetValues(Name: string): variant;
var
  aWatch: TLuaDBGWatch;
begin
  aWatch := Find(Name);
  if aWatch <> nil then
    Result := aWatch.Info.Value
  else
    VarClear(Result);
end;

procedure TLuaDBGWatches.RemoveWatch(Name: string);
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
      Server.Spool.Add(TLuaDBGGetWatches.Create);
      Server.Spool.Add(TLuaDBGGetCurrent.Create);
    finally
      DebugManager.Lock.Leave;
    end;
    Server.Resume;
  end;
end;

procedure TLuaDBGWatches.SetValues(Name: string; const Value: variant);
begin
end;

{ TLuaDBGGetWatch }

procedure TLuaDBGGetWatch.Execute(Respond: TDebugCommandRespond);
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

{ TLuaDBGBreakpoints }

function TLuaDBGBreakpoints.Add(FileName: string; Line: integer): integer;
var
  aBreakpoint: TLuaDBGBreakpoint;
begin
  Inc(CurrentHandle);
  aBreakpoint := TLuaDBGBreakpoint.Create;
  aBreakpoint.Handle := CurrentHandle;
  aBreakpoint.FileName := FileName;
  aBreakpoint.Line := Line;
  Result := Add(aBreakpoint);
end;

function TLuaDBGBreakpoints.Find(Name: string; Line: integer): TLuaDBGBreakpoint;
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

function TLuaDBGBreakpoints.Remove(Breakpoint: TLuaDBGBreakpoint): integer;
begin
  Result := inherited Remove(Breakpoint);
end;

procedure TLuaDBGBreakpoints.Remove(Handle: integer);
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

procedure TLuaDBGBreakpoints.Toggle(FileName: string; Line: integer);
var
  aBreakpoint: TLuaDBGBreakpoint;
  aSetBreakpoint: TLuaDBGSetBreakpoint;
  aRemoveBreakpoint: TLuaDBGRemoveBreakpoint;
begin
  aBreakpoint := Find(FileName, Line);
  if aBreakpoint <> nil then
  begin
    Remove(aBreakpoint);
    if Server.IsRuning then
    begin
      if aBreakpoint.ID <> 0 then
      begin
        aRemoveBreakpoint := TLuaDBGRemoveBreakpoint.Create;
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
      aSetBreakpoint := TLuaDBGSetBreakpoint.Create;
      aSetBreakpoint.FileName := FileName;
      aSetBreakpoint.FileLine := Line;
      Server.Spool.Add(aSetBreakpoint);
    end;
  end;
end;

{ TLuaDBGGetWatches }

function TLuaDBGGetWatches.Stay: boolean;
begin
  DebugManager.Lock.Enter;
  try
    Inc(Current);
    Result := Current < Connection.Server.Watches.Count;
  finally
    DebugManager.Lock.Leave;
  end;
end;

procedure TLuaDBGGetWatches.Execute(Respond: TDebugCommandRespond);
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

function TLuaDBGGetWatches.Enabled: boolean;
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

{ TLuaDBGSetBreakpoints }

function TLuaDBGSetBreakpoints.Enabled: boolean;
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

function TLuaDBGSetBreakpoints.Stay: boolean;
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

{ TLuaDBGSetBreakpoint }

function TLuaDBGSetBreakpoint.GetCommand: string;
begin
  Result := 'breakpoint_set -t line -n ' + IntToStr(FileLine) + ' -f ' + FileNameToURI(FileName) + '';
end;

procedure TLuaDBGSetBreakpoint.Execute(Respond: TDebugCommandRespond);
begin
  CheckError(Respond);
  BreakpointID := StrToInt(Respond.Root.Attributes['id']);
end;

{ TLuaDBGRemoveBreakpoint }

procedure TLuaDBGRemoveBreakpoint.Execute(Respond: TDebugCommandRespond);
begin
end;

function TLuaDBGRemoveBreakpoint.GetCommand: string;
begin
  Result := 'breakpoint_remove -d ' + IntToStr(BreakpointID);
end;

{ TLuaDBGConnectionSpool }

procedure TLuaDBGConnectionSpool.Added(Action: TLuaDBGAction);
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

procedure TLuaDBGServer.AddAction(Action: TLuaDBGAction);
begin
  DebugManager.Lock.Enter;
  try
    Spool.Add(Action);
  finally
    DebugManager.Lock.Leave;
  end;
end;

procedure TLuaDBGServer.AddAction(ActionClass: TLuaDBGActionClass);
begin
  AddAction(ActionClass.Create);
end;

procedure TLuaDBGServer.RemoveAction(Action: TLuaDBGAction);
begin
  DebugManager.Lock.Enter;
  try
    Spool.Remove(Action);
  finally
    DebugManager.Lock.Leave;
  end;
end;

procedure TLuaDBGServer.ExtractAction(Action: TLuaDBGAction);
begin
  DebugManager.Lock.Enter;
  try
    Spool.Extract(Action);
  finally
    DebugManager.Lock.Leave;
  end;
end;

procedure TLuaDBGServer.Clear;
begin
  DebugManager.Lock.Enter;
  try
    Spool.Clear;
  finally
    DebugManager.Lock.Leave;
  end;
end;

{ TLuaDBGCustomGetWatch }

function TLuaDBGCustomGetWatch.GetCommand: string;
begin
  Result := 'property_value -n "' + Info.Name + '" -m 1024';
  //Result := 'property_get -n "' + Name + '" -m 1024';
end;

procedure TLuaDBGCustomGetWatch.Execute(Respond: TDebugCommandRespond);
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

{ TLuaDBGDebugWatches }

function TLuaDBGDebugWatches.GetCount: integer;
begin
  with FDebug.FServer do
    Result := Watches.Count;
end;

function TLuaDBGDebugWatches.GetItems(Index: integer): TDebugWatchInfo;
var
  aWt: TLuaDBGWatch;
begin
  with FDebug.FServer do
    aWt := Watches[Index];
  Result:= aWt.Info;
end;

procedure TLuaDBGDebugWatches.Clear;
begin
  with FDebug.FServer do
    Watches.Clear;
end;

procedure TLuaDBGDebugWatches.Add(vName: string);
begin
  with FDebug.FServer do
    Watches.AddWatch(vName);
end;

procedure TLuaDBGDebugWatches.Remove(vName: string);
begin
  with FDebug.FServer do
    Watches.RemoveWatch(vName);
end;

function TLuaDBGDebugWatches.GetValue(vName: string; out vValue: Variant; out vType: string; EvalIt: Boolean): boolean;
var
  aAction: TLuaDBGCustomGet;
begin
  Result := False;
  if dbsRunning in FDebug.GetState then   //there is a connection from mobdebug
  begin
    if EvalIt then
      aAction := TLuaDBGEval.Create
    else
      aAction := TLuaDBGGetWatchInstance.Create;
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

{ TLuaDBGDebugBreakPoints }

function TLuaDBGDebugBreakPoints.GetCount: integer;
begin
  with FDebug.FServer do
    Result := Breakpoints.Count;
end;

function TLuaDBGDebugBreakPoints.GetItems(Index: integer): TDebugBreakpointInfo;
var
  aBP: TLuaDBGBreakpoint;
begin
  with FDebug.FServer do
    aBP := Breakpoints[Index];
  Result.FileName := aBP.FileName;
  Result.Handle := aBP.Handle;
  Result.Line := aBP.Line;
end;

procedure TLuaDBGDebugBreakPoints.Clear;
begin
  with FDebug.FServer do
    Breakpoints.Clear;
end;

procedure TLuaDBGDebugBreakPoints.Toggle(FileName: string; LineNo: integer);
begin
  with FDebug.FServer do
    Breakpoints.Toggle(FileName, LineNo);
end;

function TLuaDBGDebugBreakPoints.IsExists(FileName: string; LineNo: integer): boolean;
begin
  with FDebug.FServer do
    Result := Breakpoints.Find(FileName, LineNo) <> nil;
end;

procedure TLuaDBGDebugBreakPoints.Add(FileName: string; LineNo: integer);
begin
  with FDebug.FServer do
    Breakpoints.Add(FileName, LineNo);
end;

procedure TLuaDBGDebugBreakPoints.Remove(FileName: string; Line: integer);
var
  aBP: TLuaDBGBreakpoint;
begin
  with FDebug.FServer do
    aBP := Breakpoints.Find(FileName, Line);
  if aBP <> nil then
    with FDebug.FServer do
      Breakpoints.Remove(aBP);
end;

procedure TLuaDBGDebugBreakPoints.Remove(Handle: integer);
begin
  with FDebug.FServer do
    Breakpoints.Remove(Handle);
end;

{ TLuaDBGDebug }

function TLuaDBGDebug.CreateBreakPoints: TEditorBreakPoints;
begin
  Result := TLuaDBGDebugBreakPoints.Create;
  (Result as TLuaDBGDebugBreakPoints).FDebug := Self;
end;

function TLuaDBGDebug.CreateWatches: TEditorWatches;
begin
  Result := TLuaDBGDebugWatches.Create;
  (Result as TLuaDBGDebugWatches).FDebug := Self;
end;

constructor TLuaDBGDebug.Create;
begin
  inherited Create;
  FServer := TLuaDBGServer.Create;
  //FServer.FDebug := Self;
end;

destructor TLuaDBGDebug.Destroy;
begin
  FreeAndNil(FServer);
  inherited;
end;

procedure TLuaDBGDebug.Action(AAction: TDebugAction);
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

function TLuaDBGDebug.GetState: TDebugStates;
begin
  Result := [];
  if FServer.Active then
    Result := Result + [dbsActive];
  if FServer.IsRuning then
    Result := Result + [dbsRunning];
end;

procedure TLuaDBGDebug.Start;
begin
  inherited;
  FServer.Start;
end;

procedure TLuaDBGDebug.Stop;
var
  aAction: TLuaDBGDetach;
begin
  inherited;
  if FServer.IsRuning then
  begin
    FServer.Clear;
    aAction := TLuaDBGDetach.Create;
    aAction.CreateEvent;
    FServer.AddAction(aAction);
    FServer.Resume;
    aAction.Event.WaitFor(30000);
    FServer.ExtractAction(aAction);
    aAction.Free;
  end;
  FServer.Stop;
end;

procedure TLuaDBGDebug.Reset;
begin
  FServer.Clear; //no need to any exists actions
  FServer.AddAction(TLuaDBGStop.Create);
  FServer.AddAction(TLuaDBGGetCurrent.Create);
  FServer.Resume;
end;

procedure TLuaDBGDebug.Resume;
begin
  FServer.AddAction(TLuaDBGDetach.Create);
  FServer.AddAction(TLuaDBGGetCurrent.Create);
  FServer.Resume;
end;

procedure TLuaDBGDebug.StepInto;
begin
  FServer.AddAction(TLuaDBGStepInto.Create);
  FServer.AddAction(TLuaDBGGetWatches.Create);
  FServer.AddAction(TLuaDBGGetCurrent.Create);
  FServer.Resume;
end;

procedure TLuaDBGDebug.StepOver;
begin
  FServer.AddAction(TLuaDBGStep.Create);
  FServer.AddAction(TLuaDBGGetWatches.Create);
  FServer.AddAction(TLuaDBGGetCurrent.Create);
  FServer.Resume;
end;

procedure TLuaDBGDebug.StepOut;
begin
  FServer.AddAction(TLuaDBGStepOut.Create);
  FServer.AddAction(TLuaDBGGetWatches.Create);
  FServer.AddAction(TLuaDBGGetCurrent.Create);
  FServer.Resume;
end;

procedure TLuaDBGDebug.Run;
begin
  FServer.AddAction(TLuaDBGRun.Create);
  FServer.AddAction(TLuaDBGGetWatches.Create);
  FServer.AddAction(TLuaDBGGetCurrent.Create);
  FServer.Resume;
end;

function TLuaDBGDebug.GetKey: string;
begin
  Result := FServer.Key;
end;

end.

