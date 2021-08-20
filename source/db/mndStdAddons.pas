unit mndStdAddons;
{**
 *  This file is part of the "MiniEdit"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$mode objfpc}{$H+}

{
  Root --------------------------------
    |                 |               |
    |                 |               |
    |                 |               |
  Master             Master           Master
    |----------------
    |       |       |
  Member  Member  Member
    |----------------
    |               |
   Action        Action

   Open Meta = List the groups and open the first (Default) group
   Open Group = List the members of this group in the members list
   Open Member = Make the member as Meta and open it as Meta
}

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms, StdCtrls,
  mnUtils, mncMeta,
  mncCSV, mncPostgre, mncMySQL, mncFirebird, mncSQLite,
  mncPGMeta, mncMySQLMeta, mncFBMeta, mncSQLiteMeta,
  mndEngines;

type

  { TDatabasesAddon }

  TDatabasesAddon = class(TmndAddon)
  private
  public
    constructor Create; override;
    procedure DoExecute(vMetaItems: TmncMetaItems); override;
  end;
         
  { TDatabaseAddon }

  TDatabaseAddon = class(TmndAddon)
  private
  public
    constructor Create; override;
    procedure DoExecute(vMetaItems: TmncMetaItems); override;
  end;

  { TMembersAddon }

  TMembersAddon = class(TmndAddon)
  private
  protected
    function GetCanExecute: Boolean; override;
  public
    constructor Create; override;
    procedure DoExecute(vMetaItems: TmncMetaItems); override;
  end;

  { TTablesAddon }

  TTablesAddon = class(TMembersAddon)
  public
    constructor Create; override;
    procedure EnumMeta(vItems: TmncMetaItems; vMetaItems: TmncMetaItems); override;
  end;

  TTableAddon = class(TmndAddon)
  public
    constructor Create; override;
    procedure DoExecute(vMetaItems: TmncMetaItems); override;
  end;

  { TIndexAddon }

  TIndexAddon = class(TmndAddon)
  public
    constructor Create; override;
    procedure DoExecute(vMetaItems: TmncMetaItems); override;
    procedure EnumMeta(MetaItems: TmncMetaItems; vMetaItems: TmncMetaItems); override;
  end;

  { TProceduresAddon }

  TProceduresAddon = class(TMembersAddon)
  public
    constructor Create; override;
    procedure EnumMeta(MetaItems: TmncMetaItems; vMetaItems: TmncMetaItems); override;
  end;

  { TProcedureAddon }

  TProcedureAddon = class(TmndAddon)
  public
    constructor Create; override;
    procedure DoExecute(vMetaItems: TmncMetaItems); override;
  end;

  { TViewsAddon }

  TViewsAddon = class(TMembersAddon)
  public
    procedure EnumMeta(MetaItems: TmncMetaItems; vMetaItems: TmncMetaItems); override;
    constructor Create; override;
  end;

  { TViewAddon }

  TViewAddon = class(TmndAddon)
  public
    constructor Create; override;
    procedure DoExecute(vMetaItems: TmncMetaItems); override;
  end;

  { TSequencesAddon }

  TSequencesAddon = class(TMembersAddon)
  public
    procedure EnumMeta(MetaItems: TmncMetaItems; vMetaItems: TmncMetaItems); override;
    constructor Create; override;
  end;

  { TDomainsAddon }

  TDomainsAddon = class(TMembersAddon)
  public
    procedure EnumMeta(MetaItems: TmncMetaItems; vMetaItems: TmncMetaItems); override;
    constructor Create; override;
  end;

  { TExceptionsAddon }

  TExceptionsAddon = class(TMembersAddon)
  public
    constructor Create; override;
    procedure EnumMeta(MetaItems: TmncMetaItems; vMetaItems: TmncMetaItems); override;
  end;

  { TFunctionsAddon }

  TFunctionsAddon = class(TMembersAddon)
  public
    constructor Create; override;
    procedure EnumMeta(MetaItems: TmncMetaItems; vMetaItems: TmncMetaItems); override;
  end;

  { TTriggersAddon }

  TTriggersAddon = class(TMembersAddon)
  public
    constructor Create; override;
    procedure EnumMeta(MetaItems: TmncMetaItems; vMetaItems: TmncMetaItems); override;
  end;

  { TTableTriggersAddon }

  TTableTriggersAddon = class(TMembersAddon)
  public
    constructor Create; override;
    procedure EnumMeta(MetaItems: TmncMetaItems; vMetaItems: TmncMetaItems); override;
  end;

  TTriggerAddon = class(TmndAddon)
  public
    constructor Create; override;
    procedure DoExecute(vMetaItems: TmncMetaItems); override;
  end;

  { TIndicesAddon }

  TIndicesAddon = class(TMembersAddon)
  public
    constructor Create; override;
    procedure EnumMeta(MetaItems: TmncMetaItems; vMetaItems: TmncMetaItems); override;
  end;

  { TTableIndicesAddon }

  TTableIndicesAddon = class(TMembersAddon)
  public
    constructor Create; override;
    procedure EnumMeta(MetaItems: TmncMetaItems; vMetaItems: TmncMetaItems); override;
  end;

  { TDropIndexAddon }

  TDropIndexAddon = class(TmndAddon)
  public
    constructor Create; override;
    procedure DoExecute(vMetaItems: TmncMetaItems); override;
  end;

  { TTableFieldsAddon }

  TTableFieldsAddon = class(TMembersAddon)
  public
    constructor Create; override;
    procedure EnumMeta(MetaItems: TmncMetaItems; vMetaItems: TmncMetaItems); override;
  end;

  { TDropTableAddon }

  TDropTableAddon = class(TmndAddon)
  public
    constructor Create; override;
    procedure DoExecute(vMetaItems: TmncMetaItems); override;
  end;

  { TSelectTableAddon }

  TSelectTableAddon = class(TmndAddon)
  public
    constructor Create; override;
    procedure DoExecute(vMetaItems: TmncMetaItems); override;
  end;

  { TInsertTableAddon }

  TInsertTableAddon = class(TmndAddon)
  public
    constructor Create; override;
    procedure DoExecute(vMetaItems: TmncMetaItems); override;
  end;

  { TDropFieldAddon }

  TDropFieldAddon = class(TmndAddon)
  public
    constructor Create; override;
    procedure DoExecute(vMetaItems: TmncMetaItems); override;
  end;

  { TNewFieldAddon }

  TNewFieldAddon = class(TmndAddon)
  public
    constructor Create; override;
    procedure DoExecute(vMetaItems: TmncMetaItems); override;
  end;

  { TEmptyTableAddon }

  TEmptyTableAddon = class(TmndAddon)
  public
    constructor Create; override;
    procedure DoExecute(vMetaItems: TmncMetaItems); override;
  end;

implementation

uses
  Contnrs;

{ TDatabasesAddon }

constructor TDatabasesAddon.Create;
begin
  inherited Create;
  Master := 'Server';
  Name := 'Databases';
  Title := 'Databases';
  Kind := sokDatabase;
  ImageIndex := IMG_DATABASE;
end;

procedure TDatabasesAddon.DoExecute(vMetaItems: TmncMetaItems);
begin
  inherited DoExecute(vMetaItems);
end;

{ TDatabaseAddon }

constructor TDatabaseAddon.Create;
begin
  inherited;
  Master := 'Databases';
  Name := 'Database';
  Title := 'Database';
  Kind := sokDatabase;
  ImageIndex := IMG_DATABASES;
end;

procedure TDatabaseAddon.DoExecute(vMetaItems: TmncMetaItems);
begin
  inherited;
  DBEngine.ShowMeta(Self, True);
end;

{ TTablesAddon }

constructor TTablesAddon.Create;
begin
  inherited;
  Master := 'Database';
  Name := 'Tables';
  Title := 'Tables';
  ItemName := 'Table';
  Kind := sokTable;
  ImageIndex := IMG_TABLE;
end;

procedure TTablesAddon.EnumMeta(vItems: TmncMetaItems; vMetaItems: TmncMetaItems);
var
  aMeta: TmncMeta;
begin
  aMeta := DBEngine.DB.CreateMeta;
  try
    aMeta.EnumTables(vItems);
  finally
    aMeta.Free
  end;
end;

{ TProceduresAddon }

constructor TProceduresAddon.Create;
begin
  inherited;
  Master := 'Database';
  Name := 'Procedures';
  Title := 'Procedures';
  ItemName := 'Procedure';
  Kind := sokProcedure;
  ImageIndex := IMG_PROCEDURE;
end;

procedure TProceduresAddon.EnumMeta(MetaItems: TmncMetaItems; vMetaItems: TmncMetaItems);
var
  aMeta: TmncMeta;
begin
  aMeta := DBEngine.DB.CreateMeta;
  try
    aMeta.EnumProcedures(MetaItems);
  finally
    aMeta.Free
  end;
end;

{ TViewsAddon }

constructor TViewsAddon.Create;
begin
  inherited;
  Master := 'Database';
  Name := 'Views';
  Title := 'Views';
  ItemName := 'View';
  Kind := sokView;
  ImageIndex := IMG_VIEW;
end;

procedure TViewsAddon.EnumMeta(MetaItems: TmncMetaItems; vMetaItems: TmncMetaItems);
var
  aMeta: TmncMeta;
begin
  aMeta := DBEngine.DB.CreateMeta;
  try
    aMeta.EnumViews(MetaItems);
  finally
    aMeta.Free
  end;
end;

{ TSequencesAddon }

constructor TSequencesAddon.Create;
begin
  inherited;
  Master := 'Database';
  Name := 'Sequences';
  Title := 'Sequences';
  ItemName := 'Sequence';
  Kind := sokSequence;
  ImageIndex := IMG_GENERATOR;
end;

procedure TSequencesAddon.EnumMeta(MetaItems: TmncMetaItems; vMetaItems: TmncMetaItems);
var
  aMeta: TmncMeta;
begin
  aMeta := DBEngine.DB.CreateMeta;
  try
    aMeta.EnumSequences(MetaItems);
  finally
    aMeta.Free
  end;
end;

{ TDomainsAddon }

constructor TDomainsAddon.Create;
begin
  inherited;
  Master := 'Database';
  Name := 'Domains';
  Title := 'Domains';
  ItemName := 'Domain';
  Kind := sokDomain;
  ImageIndex := IMG_DOMAIN;
end;

procedure TDomainsAddon.EnumMeta(MetaItems: TmncMetaItems; vMetaItems: TmncMetaItems);
var
  aMeta: TmncMeta;
begin
  aMeta := DBEngine.DB.CreateMeta;
  try
    aMeta.EnumDomains(MetaItems);
  finally
    aMeta.Free
  end;
end;

{ TExceptionsAddon }

constructor TExceptionsAddon.Create;
begin
  inherited;
  Master := 'Database';
  Name := 'Exceptions';
  Title := 'Exceptions';
  ItemName := 'Exception';
  Kind := sokException;
  ImageIndex := IMG_EXCEPTION;
end;

procedure TExceptionsAddon.EnumMeta(MetaItems: TmncMetaItems; vMetaItems: TmncMetaItems);
var
  aMeta: TmncMeta;
begin
  aMeta := DBEngine.DB.CreateMeta;
  try
    aMeta.EnumExceptions(MetaItems);
  finally
    aMeta.Free
  end;
end;

{ TFunctionsAddon }

constructor TFunctionsAddon.Create;
begin
  inherited;
  Master := 'Database';
  Name := 'Functions';
  Title := 'Functions';
  ItemName := 'Function';
  Kind := sokFunction;
  ImageIndex := IMG_FUNCTION;
end;

procedure TFunctionsAddon.EnumMeta(MetaItems: TmncMetaItems; vMetaItems: TmncMetaItems);
var
  aMeta: TmncMeta;
begin
  aMeta := DBEngine.DB.CreateMeta;
  try
    aMeta.EnumFunctions(MetaItems);
  finally
    aMeta.Free
  end;
end;

{ TMembersAddon }

constructor TMembersAddon.Create;
begin
  inherited;
end;

procedure TMembersAddon.DoExecute(vMetaItems: TmncMetaItems);
begin
  inherited;
  //mndGui.ShowMeta(Self, Value);
end;

function TMembersAddon.GetCanExecute: Boolean;
begin
  Result := True;
end;

{ TProcedureAddon }

constructor TProcedureAddon.Create;
begin
  inherited;
  Master := 'Procedure';
  Name := 'ProcedureSource';
  Title := 'Procedure Source';
  ItemName := 'Source';
  Kind := sokProcedure;
  ImageIndex := IMG_PROCEDURE;
end;

procedure TProcedureAddon.DoExecute(vMetaItems: TmncMetaItems);
var
  aMeta: TmncMeta;
  aStrings: TStringList;
begin
  inherited;
  aStrings := TStringList.Create;
  try
    aMeta := DBEngine.DB.CreateMeta;
    try
      //aMeta.ExtractObject(aStrings, sokProcedure, MemberName, [ekAlter]);
    finally
      aMeta.Free;
    end;
    DBEngine.ShowEditor(Self, aStrings);
  finally
    aStrings.Free;
  end;
end;

{ TViewAddon }

constructor TViewAddon.Create;
begin
  inherited;
  Master := 'View';
  Name := 'ViewSource';
  Title := 'View Source';
  ItemName := 'Source';
  Kind := sokView;
  ImageIndex := IMG_VIEW;
end;

procedure TViewAddon.DoExecute(vMetaItems: TmncMetaItems);
var
  aMeta: TmncMeta;
  aStrings: TStringList;
begin
  inherited;
  aStrings := TStringList.Create;
  try
    aMeta := DBEngine.DB.CreateMeta;
    try
      aMeta.GetViewSource(aStrings, vMetaItems.Values['View'], [ekAlter]);
    finally
      aMeta.Free;
    end;
    DBEngine.ShowEditor(Self, aStrings);
  finally
    aStrings.Free;
  end;
end;

{ TTableTriggersAddon }

constructor TTableTriggersAddon.Create;
begin
  inherited;
  Master := 'Table';
  Name := 'Triggers';
  Title := 'Triggers';
  Kind := sokTrigger;
  ImageIndex := IMG_TRIGGER;
end;

procedure TTableTriggersAddon.EnumMeta(MetaItems: TmncMetaItems; vMetaItems: TmncMetaItems);
var
  aMeta: TmncMeta;
begin
  aMeta := DBEngine.DB.CreateMeta;
  try
    aMeta.EnumTriggers(MetaItems, vMetaItems.Values['Table']);
  finally
    aMeta.Free;
  end;
end;

{ TTriggerAddon }

constructor TTriggerAddon.Create;
begin
  inherited;
  Master := 'Triggers';
  Name := 'Trigger';
  Title := 'Trigger';
  Kind := sokTrigger;
  ImageIndex := IMG_TRIGGER;
end;

procedure TTriggerAddon.DoExecute(vMetaItems: TmncMetaItems);
var
  aMeta: TmncMeta;
  aStrings: TStringList;
begin
  inherited;
  aStrings := TStringList.Create;
  try
    aMeta := DBEngine.DB.CreateMeta;
    try
      aMeta.GetTriggerSource(aStrings, vMetaItems.Values['Trigger'], [ekAlter]);
    finally
      aMeta.Free;
    end;
    DBEngine.ShowEditor(Self, aStrings);
  finally
    aStrings.Free;
  end;
end;

{ TTableIndicesAddon }

constructor TTableIndicesAddon.Create;
begin
  inherited;
  Master := 'Table';
  Name := 'Indices';
  Title := 'Indices';
  ItemName := 'Index';
  Kind := sokIndex;
  ImageIndex := IMG_INDEX;
end;

procedure TTableIndicesAddon.EnumMeta(MetaItems: TmncMetaItems; vMetaItems: TmncMetaItems);
var
  aMeta: TmncMeta;
begin
  aMeta := DBEngine.DB.CreateMeta;
  try
    aMeta.EnumIndices(MetaItems, vMetaItems.Values['Table']);
  finally
    aMeta.Free
  end;
end;

{ TDropIndexAddon }

constructor TDropIndexAddon.Create;
begin
  inherited;
  Master := 'Index';
  Name := 'DropIndex';
  Title := 'Drop Index';
  Kind := sokIndex;
  Style := Style + [nsCommand];
  ImageIndex := IMG_INDEX;
end;

procedure TDropIndexAddon.DoExecute(vMetaItems: TmncMetaItems);
begin
  inherited;

end;

{ TTableFieldsAddon }

constructor TTableFieldsAddon.Create;
begin
  inherited;
  Master := 'Table';
  Name := 'Fields';
  Title := 'Fields';
  ItemName := 'Field';
  Kind := sokField;
  ImageIndex := IMG_FIELD;
end;

procedure TTableFieldsAddon.EnumMeta(MetaItems: TmncMetaItems; vMetaItems: TmncMetaItems);
var
  aMeta: TmncMeta;
begin
  aMeta := DBEngine.DB.CreateMeta;
  try
    aMeta.EnumFields(MetaItems, vMetaItems.Values['Table']);
  finally
    aMeta.Free
  end;
end;

{ TNewFieldAddon }

constructor TNewFieldAddon.Create;
begin
  inherited;
  Master := 'Table';
  Name := 'NewField';
  Title := 'New Field';
  Kind := sokField;
  Style := Style + [nsCommand];
  ImageIndex := IMG_FIELD;
end;

procedure TNewFieldAddon.DoExecute(vMetaItems: TmncMetaItems);
var
  aStrings: TStringList;
//  aFieldName: string;
begin
  inherited;
  aStrings := TStringList.Create;
  try
    //aStrings.Text := 'alter table ' + vStack['Table'] + ' drop column ' + Value;
    aStrings.Text := 'alter table ' + vMetaItems.Values['Table'] + ' add ?FieldName ?FieldType';

    DBEngine.ShowEditor(Self, aStrings);
  finally
    aStrings.Free;
  end;

  {if InputQuery('New', 'New Field for ' + MemberName, aFieldName) then
  begin
    aStrings := TStringList.Create;
    aStrings.Text := 'alter table ' + FBQuoteName(MemberName) + ' add ' + FBQuoteName(aFieldName) + ' smallint';
    Open(vSession, aStrings);
    aStrings.Free;
  end;}
end;

{ TEmptyTableAddon }

constructor TEmptyTableAddon.Create;
begin
  inherited;
  Master := 'Table';
  Name := 'EmptyTable';
  Title := 'Empty';
  Kind := sokNone;
  Style := [nsCommand];
  ImageIndex := IMG_COMMAND;
end;

procedure TEmptyTableAddon.DoExecute(vMetaItems: TmncMetaItems);
var
  aStrings: TStringList;
begin
  inherited;
  aStrings := TStringList.Create;
  aStrings.Text := 'delete from ' + vMetaItems.Values['Table'];
  DBEngine.ShowEditor(Self, aStrings);
  aStrings.Free;
end;

{ TDropFieldAddon }

constructor TDropFieldAddon.Create;
begin
  inherited;
  Master := 'Field';
  Name := 'DropField';
  Title := 'Drop Field';
  Kind := sokField;
  Style := Style + [nsCommand];
  ImageIndex := IMG_FIELD;
end;

procedure TDropFieldAddon.DoExecute(vMetaItems: TmncMetaItems);
var
  aStrings: TStringList;
begin
  inherited;
  aStrings := TStringList.Create;
  try
    aStrings.Text := 'alter table ' + vMetaItems.Values['Table'] + ' drop column ' + vMetaItems.Values['Field'];
    DBEngine.ShowEditor(Self, aStrings);
  finally
    aStrings.Free;
  end;
end;

{ TTriggersAddon }

constructor TTriggersAddon.Create;
begin
  inherited Create;
  Master := 'Database';
  Name := 'Triggers';
  Title := 'Triggers';
  ItemName := 'Trigger';
  Kind := sokTrigger;
  ImageIndex := IMG_TRIGGER;
end;

procedure TTriggersAddon.EnumMeta(MetaItems: TmncMetaItems; vMetaItems: TmncMetaItems);
var
  aMeta: TmncMeta;
begin
  aMeta := DBEngine.DB.CreateMeta;
  try
    aMeta.EnumTriggers(MetaItems);
  finally
    aMeta.Free
  end;
end;

{ TTableAddon }

constructor TTableAddon.Create;
begin
  inherited Create;
  Master := 'Tables';
  Name := 'Table';
  Title := 'Table';
  //ItemName := 'Field'; nop it has a child Masters
  Kind := sokTable;
  ImageIndex := IMG_TABLE;
end;

procedure TTableAddon.DoExecute(vMetaItems: TmncMetaItems);
begin
  inherited;
  DBEngine.ShowMeta(Self, True);
end;

{ TIndexAddon }

constructor TIndexAddon.Create;
begin
  inherited Create;
  Master := 'Indices';
  Name := 'Index';
  Title := 'Index';
  Kind := sokIndex;
  ImageIndex := IMG_INDEX;
end;

procedure TIndexAddon.DoExecute(vMetaItems: TmncMetaItems);
begin
  inherited;
  //DBEngine.EnumMembers(Self, Value);
end;

procedure TIndexAddon.EnumMeta(MetaItems: TmncMetaItems; vMetaItems: TmncMetaItems);
var
  aMeta: TmncMeta;
begin
  aMeta := DBEngine.DB.CreateMeta;
  try
    aMeta.GetIndexInfo(MetaItems, vMetaItems.Values['Index']);
  finally
    aMeta.Free
  end;
end;

{ TIndicesAddon }

constructor TIndicesAddon.Create;
begin
  inherited Create;
  Master := 'Database';
  Name := 'Indices';
  Title := 'Indices';
  ItemName := 'Index';
  Kind := sokIndex;
  ImageIndex := IMG_INDEX;
end;

procedure TIndicesAddon.EnumMeta(MetaItems: TmncMetaItems; vMetaItems: TmncMetaItems);
var
  aMeta: TmncMeta;
begin
  aMeta := DBEngine.DB.CreateMeta;
  try
    aMeta.EnumIndices(MetaItems);
  finally
    aMeta.Free
  end;
end;

{ TDropTableAddon }

constructor TDropTableAddon.Create;
begin
  inherited Create;
  Master := 'Table';
  Name := 'DropTable';
  Title := 'Drop';
  Kind := sokTable;
  Style := Style + [nsCommand];
  ImageIndex := IMG_TABLE;
end;

procedure TDropTableAddon.DoExecute(vMetaItems: TmncMetaItems);
begin
  inherited;
  DBEngine.ShowEditor(Self, 'drop table ' + vMetaItems.Values['Table']);
end;

{ TSelectTableAddon }

constructor TSelectTableAddon.Create;
begin
  inherited Create;
  Master := 'Table';
  Name := 'SelectTable';
  Title := 'Select';
  Kind := sokTable;
  Style := Style + [nsCommand];
  ImageIndex := IMG_TABLE;
end;

procedure TSelectTableAddon.DoExecute(vMetaItems: TmncMetaItems);
begin
  inherited;
  DumpMetaItems(vMetaItems);
  DBEngine.ShowEditor(Self, 'select * from ' + vMetaItems.Values['Table']);
end;

{ TInsertTableAddon }

constructor TInsertTableAddon.Create;
begin
  inherited Create;
  Master := 'Table';
  Name := 'InsertTable';
  Title := 'Insert';
  Kind := sokTable;
  Style := Style + [nsCommand];
  ImageIndex := IMG_TABLE;
end;

procedure TInsertTableAddon.DoExecute(vMetaItems: TmncMetaItems);
var
  aMeta: TmncMeta;
  aItems: TmncMetaItems;
  s1, s2: string;
  i: Integer;
begin
  inherited;
  aItems := TmncMetaItems.Create;
  try
    aMeta := DBEngine.DB.CreateMeta;
    try
      aMeta.EnumFields(aItems, vMetaItems.Values['Table']);
    finally
      aMeta.Free
    end;
    s1 := '';
    s2 := '';
    for i := 0 to aItems.Count - 1 do
    begin
      if i > 0 then
      begin
        s1 := s1 + ' ,';
        s2 := s2 + ' ,';
      end;
      s1 := s1+ aItems[i].Name;
      s2 := s2 + '?' + aItems[i].Name;
    end;
  finally
    aItems.Free;
  end;
  DBEngine.ShowEditor(Self, 'insert into ' + vMetaItems.Values['Table'] + '(' + s1 + ') values (' + s2 +')');
end;

{ TExportSQLAddon }

initialization
  DBEngine.RegisterViewer([TDatabaseAddon]);
  DBEngine.RegisterViewer([TTablesAddon, TTableAddon, TTableFieldsAddon]);
  DBEngine.RegisterViewer([TSelectTableAddon, TInsertTableAddon, TEmptyTableAddon, TDropTableAddon]);
  DBEngine.RegisterViewer([TIndicesAddon, TTableIndicesAddon, TIndexAddon, TDropIndexAddon]);
  DBEngine.RegisterViewer([TDropFieldAddon, TNewFieldAddon]);
  DBEngine.RegisterViewer([TViewsAddon, TViewAddon]);
  DBEngine.RegisterViewer([TTriggersAddon, TTriggerAddon, TTableTriggersAddon]);
  //DBEngine.RegisterViewer([TDomainsAddon, TExceptionsAddon, TFunctionsAddon]);
  //DBEngine.RegisterViewer([TProceduresAddon, TProcedureAddon]);
  //DBEngine.RegisterViewer([TSequencesAddon]);
end.
