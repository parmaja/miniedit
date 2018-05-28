unit sqlvSessions;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}


{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, mncMetas,
  mncSQL, mncConnections, mncDB;

type
  TsqlvOnNotifySession = procedure of object;

  { TsqlvDB }

  TsqlvDB = class(TObject)
  private
    FConnection: TmncSQLConnection;
    FSession: TmncSQLSession;
    FTables: TmncMetaItems;
    FSequences: TmncMetaItems;
    FProceduers: TmncMetaItems;
    FViews: TmncMetaItems;
    FFunctions: TmncMetaItems;
    FExceptions: TmncMetaItems;
    FDomains: TmncMetaItems;
    FFields: TmncMetaItems;
    FOnDisconnected: TsqlvOnNotifySession;
    FOnConnected: TsqlvOnNotifySession;
    FOnSessionStarted: TsqlvOnNotifySession;
    FOnSessionStoped: TsqlvOnNotifySession;
    FExclusive: Boolean;
    FVacuum: Boolean;
    procedure RunLoginSQL;
    procedure RunLogoutSQL;
    procedure ConnectionAfterConnect(Sender: TObject);
    procedure ConnectionAfterDisconnect(Sender: TObject);
    procedure SessionStarted(Sender: TObject);
    procedure SessionStopped(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadMeta;
    procedure Open(Name:string; vAutoCreate, vExclusive, vVacuum: Boolean);
    procedure Close;
    function IsActive: Boolean;
    procedure Connected;
    procedure Disconnected;
    property OnConnected: TsqlvOnNotifySession read FOnConnected write FOnConnected;
    property OnDisconnected: TsqlvOnNotifySession read FOnDisconnected write FOnDisconnected;
    property OnSessionStarted: TsqlvOnNotifySession read FOnSessionStarted write FOnSessionStarted;
    property OnSessionStoped: TsqlvOnNotifySession read FOnSessionStoped write FOnSessionStoped;
    property Connection: TmncSQLConnection read FConnection;
    property Session: TmncSQLSession read FSession;
    property Tables: TmncMetaItems read FTables;
    property Proceduers: TmncMetaItems read FProceduers;
    property Views: TmncMetaItems read FViews;
    property Sequences: TmncMetaItems read FSequences;
    property Functions: TmncMetaItems read FFunctions;
    property Exceptions: TmncMetaItems read FExceptions;
    property Domains: TmncMetaItems read FDomains;
    property Fields: TmncMetaItems read FFields;
  end;

implementation

uses
  {$ifdef FIREBIRD}
  mncFirebird,
  {$else}
  mncSQLite,
  {$endif}
  sqlvClasses;

{ TsqlvDB }

procedure TsqlvDB.Connected;
begin
  if FVacuum then
    Connection.Execute('vacuum');
  Session.Start;
  LoadMeta;
  RunLoginSQL;
  if Assigned(FOnConnected) then
    FOnConnected;
end;

constructor TsqlvDB.Create;
begin
  inherited;
  {$ifdef FIREBIRD}
  FConnection := Engines.CreateByName('FirebirdSQL') as TmncSQLConnection;
  {$else}
  FConnection := Engines.CreateByName('SQLite') as TmncSQLConnection;
  {$endif}

  FSession := FConnection.CreateSession;
  FConnection.OnConnected :=  @ConnectionAfterConnect;
  FConnection.OnDisconnected :=  @ConnectionAfterDisconnect;

  FTables := TmncMetaItems.Create;
  FProceduers := TmncMetaItems.Create;
  FViews := TmncMetaItems.Create;
  FSequences := TmncMetaItems.Create;
  FExceptions := TmncMetaItems.Create;
  FFunctions := TmncMetaItems.Create;
  FDomains := TmncMetaItems.Create;
  FFields := TmncMetaItems.Create;
end;

destructor TsqlvDB.Destroy;
begin
  FreeAndNil(FTables);
  FreeAndNil(FProceduers);
  FreeAndNil(FViews);
  FreeAndNil(FSequences);
  FreeAndNil(FExceptions);
  FreeAndNil(FFunctions);
  FreeAndNil(FDomains);
  FreeAndNil(FFields);
  FreeAndNil(FSession);
  FreeAndNil(FConnection);
  inherited;
end;

procedure TsqlvDB.Disconnected;
begin
  RunLogoutSQL;
  if Assigned(FOnDisconnected) then
    FOnDisconnected;
end;

procedure TsqlvDB.LoadMeta;
var
  Meta: TmncMeta;
begin
  if sqlvEngine.Setting.CacheMetas then
  begin
    Meta := Session.CreateMeta;
    try
      Meta.Link := Session;
      Meta.EnumObjects(Tables, sokTable, '', [ekSystem, ekSort]);
      Meta.EnumObjects(Views, sokView, '', [ekSort]);
      Meta.EnumObjects(Proceduers, sokProcedure, '', [ekSort]);
      Meta.EnumObjects(Sequences, sokSequence, '', [ekSort]);
      Meta.EnumObjects(Functions, sokFunction, '', [ekSort]);
      Meta.EnumObjects(Exceptions, sokException, '', [ekSort]);
      Meta.EnumObjects(Domains, sokDomain, '', [ekSort]);
      Meta.EnumObjects(Fields, sokField);
    finally
      Meta.Free;
    end;
  end;
end;

procedure TsqlvDB.Open(Name: string; vAutoCreate, vExclusive, vVacuum: Boolean);
begin
  FVacuum := vVacuum;
  FExclusive := vExclusive;
  Connection.Resource := Name;
  Connection.AutoCreate := vAutoCreate;
  Connection.UserName := 'sysdba';//Firebird
  Connection.Password := 'masterkey';
  //DBConnection.Exclusive := FExclusive;//TODO

  Connection.Connect;
  sqlvEngine.AddRecent(Name);
  sqlvEngine.SaveRecents;
end;

procedure TsqlvDB.Close;
begin
  if Session.Active then
    Session.Stop;
  if Connection.Connected then
    Connection.Disconnect;
end;

function TsqlvDB.IsActive: Boolean;
begin
  Result := Connection.Active;
end;

procedure TsqlvDB.RunLoginSQL;
var
  CMD: TmncSQLCommand;
begin
  CMD := Session.CreateCommand;
  try
    if sqlvEngine.Setting.InternalLoginSQL <> '' then
    begin
      CMD.SQL.Text := sqlvEngine.Setting.InternalLoginSQL;
      CMD.Execute;
      CMD.Session.Commit(True);
    end;
    if sqlvEngine.Setting.LoginSQL <> '' then
    begin
      CMD.Close;
      CMD.SQL.Text := sqlvEngine.Setting.LoginSQL;
      CMD.Execute;
      CMD.Session.Commit(True);
    end;
  finally
    CMD.Free;
  end;
end;

procedure TsqlvDB.RunLogoutSQL;
var
  CMD: TmncSQLCommand;
begin
  CMD := Session.CreateCommand;
  try
    if sqlvEngine.Setting.InternalLogoutSQL <> '' then
    begin
      CMD.SQL.Text := sqlvEngine.Setting.InternalLogoutSQL;
      CMD.Execute;
      CMD.Session.Commit(True);
    end;
    if sqlvEngine.Setting.LogoutSQL <> '' then
    begin
      CMD.Close;
      CMD.SQL.Text := sqlvEngine.Setting.LogoutSQL;
      CMD.Execute;
      CMD.Session.Commit(True);
    end;
  finally
    CMD.Free;
  end;
end;

procedure TsqlvDB.ConnectionAfterConnect(Sender: TObject);
begin
  Connected;
end;

procedure TsqlvDB.ConnectionAfterDisconnect(Sender: TObject);
begin
  Disconnected;
end;

procedure TsqlvDB.SessionStarted(Sender: TObject);
begin
  if Assigned(FOnSessionStarted) then
    FOnSessionStarted();
end;

procedure TsqlvDB.SessionStopped(Sender: TObject);
begin
  if Assigned(FOnSessionStoped) then
    FOnSessionStoped();
end;

end.

