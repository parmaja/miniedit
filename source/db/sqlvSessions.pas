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
  SysUtils, Classes, mncMeta,
  mncSQL, mncConnections, mncDB, EditorEngine, EditorClasses;

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
    FExclusive: Boolean;
    FVacuum: Boolean;
    procedure RunLoginSQL;
    procedure RunLogoutSQL;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateMeta: TmncMeta;
    procedure LoadMeta;
    procedure Open(vCreate:Boolean; DatabaseEngine, DatabaseName, UserName, Password, Role: string; vExclusive, vVacuum: Boolean);
    procedure Close;
    function IsActive: Boolean;
    procedure Connected;
    procedure Disconnected;

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
  mncPostgre,
  mncMySQL,
  mncCSV,
  mncFirebird,
  mncSQLite,
  sqlvClasses;

{ TsqlvDB }

procedure TsqlvDB.Connected;
begin
  if FVacuum then
    Connection.Vacuum;
  Session.Start;
  LoadMeta;
  RunLoginSQL;
end;

constructor TsqlvDB.Create;
begin
  inherited;
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

function TsqlvDB.CreateMeta: TmncMeta;
begin
  Result := Engines.CreateMeta(Connection);
  Result.Link := Session;
end;

procedure TsqlvDB.Disconnected;
begin
  RunLogoutSQL;
end;

procedure TsqlvDB.LoadMeta;
var
  AMeta: TmncMeta;
begin
  if DBEngine.Setting.CacheMetas then
  begin
    AMeta := Engines.CreateMeta(FConnection);
    try
      AMeta.Link := Session;
      AMeta.EnumObjects(Tables, sokTable, '', [ekSystem, ekSort]);
      AMeta.EnumObjects(Views, sokView, '', [ekSort]);
      AMeta.EnumObjects(Proceduers, sokProcedure, '', [ekSort]);
      AMeta.EnumObjects(Sequences, sokSequence, '', [ekSort]);
      AMeta.EnumObjects(Functions, sokFunction, '', [ekSort]);
      AMeta.EnumObjects(Exceptions, sokException, '', [ekSort]);
      AMeta.EnumObjects(Domains, sokDomain, '', [ekSort]);
      AMeta.EnumObjects(Fields, sokField);
    finally
      AMeta.Free;
    end;
  end;
end;

procedure TsqlvDB.Open(vCreate: Boolean; DatabaseEngine, DatabaseName, UserName, Password, Role: string; vExclusive, vVacuum: Boolean);
begin
  FConnection := Engines.CreateConnection(DatabaseEngine) as TmncSQLConnection;

  FVacuum := vVacuum;
  FExclusive := vExclusive;

  Connection.Resource := DatabaseName;
  Connection.UserName := UserName;
  Connection.Password := Password;
  Connection.Role := Role;
  //Connection.AutoCreate := vAutoCreate;
  //DBConnection.Exclusive := FExclusive;//TODO

  DBEngine.AddRecent(Engines.ComposeConnectionString(Connection));

  FSession := FConnection.CreateSession;

  Connection.Connect;
  Connected;
  //Engine.SendLog()
  Engine.UpdateState([ecsChanged, ecsState, ecsRefresh, ecsRecents, ecsProject, ecsProjectLoaded]);
end;

procedure TsqlvDB.Close;
begin
  if IsActive then
    Disconnected;
  if (Session <> nil) and Session.Active then
    Session.Stop;
  if (Connection <> nil) and Connection.Connected then
    Connection.Disconnect;
  FreeAndNil(FSession);
  FreeAndNil(FConnection);
end;

function TsqlvDB.IsActive: Boolean;
begin
  Result := (Connection <> nil) and Connection.Active;
end;

procedure TsqlvDB.RunLoginSQL;
var
  CMD: TmncSQLCommand;
begin
  CMD := Session.CreateCommand;
  try
    if DBEngine.Setting.InternalLoginSQL <> '' then
    begin
      CMD.SQL.Text := DBEngine.Setting.InternalLoginSQL;
      CMD.Execute;
      CMD.Session.Commit(True);
    end;
    if DBEngine.Setting.LoginSQL <> '' then
    begin
      CMD.Close;
      CMD.SQL.Text := DBEngine.Setting.LoginSQL;
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
    if DBEngine.Setting.InternalLogoutSQL <> '' then
    begin
      CMD.SQL.Text := DBEngine.Setting.InternalLogoutSQL;
      CMD.Execute;
      CMD.Session.Commit(True);
    end;
    if DBEngine.Setting.LogoutSQL <> '' then
    begin
      CMD.Close;
      CMD.SQL.Text := DBEngine.Setting.LogoutSQL;
      CMD.Execute;
      CMD.Session.Commit(True);
    end;
  finally
    CMD.Free;
  end;
end;

end.

