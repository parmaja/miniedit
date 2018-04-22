unit ConsoleProcess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, pipes,
  mnStreams;

type
  TmnOnWriteString = procedure(S: String) of object;

  { TmnProcessObject }

  TmnProcessReadStream = (strmOutput, strmError);

  TmnProcessObject = class(TObject)
  protected
    FBuffer: String;
  public
    Process: TProcess;
    Thread: TThread;
    OnWrite: TmnOnWriteString;
    function IsTerminated: Boolean;
    function ReadBuffer(vStream: TInputPipeStream): Integer;
    function ReadStream(vStream: TInputPipeStream): Integer;
    procedure FlushBuffer;
    constructor Create(AProcess: TProcess; AThread: TThread; AOnWrite: TmnOnWriteString);
  end;

  { TmnConsoleThread }

  TmnConsoleThread = class(TThread)
  private
    FOnWriteString: TmnOnWriteString;
  protected
    Buffer: String;
    procedure DoOnWriteString; virtual; //To Sync
    procedure WriteString(S: String);
  public
    Process: TProcess;
    Status: Integer;
    constructor Create(AProcess: TProcess; AOnWriteString: TmnOnWriteString = nil);
    destructor Destroy; override;
    procedure Execute; override;
    procedure Read; virtual;
    property OnWriteString: TmnOnWriteString read FOnWriteString write FOnWriteString;
  end;

implementation

type
  THackThread = class(TThread)
  end;

procedure TmnProcessObject.FlushBuffer;
begin
  if Assigned(OnWrite) then
  begin
    OnWrite(FBuffer);
    FBuffer := '';
  end;
end;

constructor TmnProcessObject.Create(AProcess: TProcess; AThread: TThread; AOnWrite: TmnOnWriteString);
begin
  inherited Create;
  Process := AProcess;
  Thread := AThread;
  OnWrite := AOnWrite;
end;

function TmnProcessObject.IsTerminated: Boolean;
begin
  Result := (Thread <> nil) and THackThread(Thread).Terminated;
end;

function TmnProcessObject.ReadStream(vStream: TInputPipeStream): Integer;
var
  aWrapper: TmnWrapperStream;
  b: Boolean;
begin
  if vStream <> nil then
  begin
    try
      aWrapper := TmnWrapperStream.Create(vStream, False);
      aWrapper.EndOfLine := #13#10;

      while not IsTerminated do
      begin
        b := aWrapper.ReadLine(FBuffer, False);
        if Assigned(OnWrite) then
        begin
          if Thread <> nil then
            Thread.Synchronize(Thread, @FlushBuffer)
          else
            FlushBuffer;
        end;

        if not (Process.Running or b) then
          break;
      end;
      Process.WaitOnExit;
      Result := Process.ExitStatus;
    aWrapper.Free;
  except
    on e: Exception do
    begin
      if Process.Running and IsTerminated then
        Process.Terminate(0);
    end;
  end;
  end;
end;

function TmnProcessObject.ReadBuffer(vStream: TInputPipeStream): Integer;
const
  READ_BYTES = 1024;
var
  C, Count, L: DWORD;
begin
  if (vStream <> nil) then
  begin
    Count := 0;
    C := 0;
    try
      while not IsTerminated do
      begin
        L := Length(FBuffer);
        Setlength(FBuffer, L + READ_BYTES);
        C := vStream.Read(FBuffer[1 + L], READ_BYTES);
        if Assigned(OnWrite) then
        begin
          SetLength(FBuffer, L + C);
          if Length(FBuffer) > 0 then
          begin
            if Thread <> nil then
              Thread.Synchronize(Thread, @FlushBuffer)
            else
              FlushBuffer;
          end;
        end;

        if C > 0 then
          Inc(Count, C)
        else
          Sleep(100);

        if not (Process.Running or (C > 0)) then
          break;
      end;

      Process.WaitOnExit;

      Result := Process.ExitStatus;
    except
      on e: Exception do
      begin
        if Process.Running and IsTerminated then
          Process.Terminate(0);
      end;
    end;
  end;
end;

{ TmnConsoleThread }

procedure TmnConsoleThread.DoOnWriteString;
begin
  if Assigned(FOnWriteString) then
    FOnWriteString(Buffer);
end;

procedure TmnConsoleThread.WriteString(S: String);
begin
  Buffer := S;
  Synchronize(@DoOnWriteString);
  //DoOnWriteString;
  Buffer := '';
end;

constructor TmnConsoleThread.Create(AProcess: TProcess; AOnWriteString: TmnOnWriteString);
begin
  inherited Create(True);
  Process := AProcess;
  FOnWriteString := AOnWriteString;
end;

destructor TmnConsoleThread.Destroy;
begin
  inherited Destroy;
end;

procedure TmnConsoleThread.Read;
var
  T: String;
  aBuffer: array[0..79] of AnsiChar;

  function ReadNow(out C: DWORD): Boolean;
  begin
    if (Process.Output.NumBytesAvailable > 0) then
      C := Process.Output.Read(aBuffer, SizeOf(aBuffer))
    else
      C := 0;
    Result := C > 0;
  end;

var
  C: DWORD;
begin
  aBuffer := '';
  while Process.Running and not Terminated do
  begin
    //    repeat
    if ReadNow(C) then
    begin
      SetString(T, aBuffer, C);
      if T <> '' then
        WriteString(T);
    end;
    //until C = 0;
  end;
  //WriteString('------exit--------');
end;

procedure TmnConsoleThread.Execute;
begin
  Read;
end;

end.
