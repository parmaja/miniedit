unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, process;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    procedure Run(AFile, AParam: string);
    function Read: Integer;
    procedure Flush;
  public
    FBuffer: string;
    FProcess: TProcess;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

function TForm1.Read: Integer;
const
  READ_BYTES = 1024;
var
  C, Count, L: DWORD;
  FirstTime: Boolean;
begin
  FirstTime := True;
  Count := 0;
  C := 0;
    try
      FProcess.Execute;
      while (FirstTime or FProcess.Running or (C > 0)) do
      begin
        L := Length(FBuffer);
        Setlength(FBuffer, L + READ_BYTES);
        C := FProcess.Output.Read(FBuffer[1 + L], READ_BYTES);
        SetLength(FBuffer, L + C);
        if Length(FBuffer) > 0 then
        begin
          Flush;
        end;

        if C > 0 then
          Inc(Count, C)
        else
          Sleep(100);

        FirstTime := False;
      end;

      FProcess.WaitOnExit;

      Result := FProcess.ExitStatus;
    except
      on e : Exception do
      begin
        if FProcess.Running then
          FProcess.Terminate(0);
      end;
    end;
end;

procedure TForm1.Flush;
begin
  Memo1.Lines.Add(FBuffer);
  FBuffer := '';
end;

procedure TForm1.Run(AFile, AParam: string);
var
  aOptions: TProcessOptions;
  Status: Integer;
begin
  FProcess := TProcess.Create(nil);
  FProcess.ConsoleTitle := 'test';
  FProcess.Executable := AFile;//'lua.exe';
  FProcess.Parameters.Text := AParam;//'test.lua';
  FProcess.CurrentDirectory := Application.Location;
  FProcess.InheritHandles := True;

  aOptions := [];

  //aOptions := [poRunSuspended];

  FProcess.Options :=  aOptions + [poUsePipes, poStderrToOutPut];
  FProcess.ShowWindow := swoHIDE;
  FProcess.PipeBufferSize := 40; //80 char in line
  try
    Status := Read;
  finally
    FreeAndNil(FProcess);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Run('lua.exe', 'test.lua');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Run('test_d.exe', '');
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Run('test_pas.exe', '');
end;


end.

