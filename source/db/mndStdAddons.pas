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
    procedure DoExecute(vMetaItem: TmncMetaItem); override;
  end;
         
  { TDatabaseAddon }

  TDatabaseAddon = class(TmndAddon)
  private
  public
    constructor Create; override;
    procedure DoExecute(vMetaItem: TmncMetaItem); override;
  end;

  { TMembersAddon }

  TMembersAddon = class(TmndAddon)
  private
  protected
    function GetCanExecute: Boolean; override;
  public
    constructor Create; override;
    procedure DoExecute(vMetaItem: TmncMetaItem); override;
  end;

  { TTablesAddon }

  TTablesAddon = class(TMembersAddon)
  public
    constructor Create; override;
    procedure EnumMeta(vItems: TmncMetaItems; vMetaItem: TmncMetaItem); override;
  end;

  { TTableAddon }

  TTableAddon = class(TmndAddon)
  public
    constructor Create; override;
    procedure DoExecute(vMetaItem: TmncMetaItem); override;
    procedure EnumMeta(vItems: TmncMetaItems; vMetaItem: TmncMetaItem); override;
  end;

  { TIndexAddon }

  TIndexAddon = class(TmndAddon)
  public
    constructor Create; override;
    procedure DoExecute(vMetaItem: TmncMetaItem); override;
    procedure EnumMeta(vItems: TmncMetaItems; vMetaItem: TmncMetaItem); override;
  end;

  { TProceduresAddon }

  TProceduresAddon = class(TMembersAddon)
  public
    constructor Create; override;
    procedure EnumMeta(vItems: TmncMetaItems; vMetaItem: TmncMetaItem); override;
  end;

  { TProcedureAddon }

  TProcedureAddon = class(TmndAddon)
  public
    constructor Create; override;
    procedure DoExecute(vMetaItem: TmncMetaItem); override;
  end;

  { TViewAddon }

  TViewAddon = class(TMembersAddon)
  public
    constructor Create; override;
    procedure EnumMeta(vItems: TmncMetaItems; vMetaItem: TmncMetaItem); override;
  end;

  { TViewAddon }

  TViewSourceAddon = class(TmndAddon)
  public
    constructor Create; override;
    procedure DoExecute(vMetaItem: TmncMetaItem); override;
  end;

  { TSequencesAddon }

  TSequencesAddon = class(TMembersAddon)
  public
    procedure EnumMeta(vItems: TmncMetaItems; vMetaItem: TmncMetaItem); override;
    constructor Create; override;
  end;

  { TDomainsAddon }

  TDomainsAddon = class(TMembersAddon)
  public
    procedure EnumMeta(vItems: TmncMetaItems; vMetaItem: TmncMetaItem); override;
    constructor Create; override;
  end;

  { TExceptionsAddon }

  TExceptionsAddon = class(TMembersAddon)
  public
    constructor Create; override;
    procedure EnumMeta(vItems: TmncMetaItems; vMetaItem: TmncMetaItem); override;
  end;

  { TFunctionsAddon }

  TFunctionsAddon = class(TMembersAddon)
  public
    constructor Create; override;
    procedure EnumMeta(vItems: TmncMetaItems; vMetaItem: TmncMetaItem); override;
  end;

  { TTriggersAddon }

  TTriggersAddon = class(TMembersAddon)
  public
    constructor Create; override;
    procedure EnumMeta(vItems: TmncMetaItems; vMetaItem: TmncMetaItem); override;
  end;

  { TTableTriggersAddon }

  TTableTriggersAddon = class(TMembersAddon)
  public
    constructor Create; override;
    procedure EnumMeta(vItems: TmncMetaItems; vMetaItem: TmncMetaItem); override;
  end;

  TTriggerAddon = class(TmndAddon)
  public
    constructor Create; override;
    procedure DoExecute(vMetaItem: TmncMetaItem); override;
  end;

  { TIndicesAddon }

  TIndicesAddon = class(TMembersAddon)
  public
    constructor Create; override;
    procedure EnumMeta(vItems: TmncMetaItems; vMetaItem: TmncMetaItem); override;
  end;

  { TTableIndexAddon }

  TTableIndexAddon = class(TMembersAddon)
  public
    constructor Create; override;
    procedure EnumMeta(vItems: TmncMetaItems; vMetaItem: TmncMetaItem); override;
  end;

  { TDropIndexAddon }

  TDropIndexAddon = class(TmndAddon)
  public
    constructor Create; override;
    procedure DoExecute(vMetaItem: TmncMetaItem); override;
  end;

  { TTableFieldsAddon }

  TTableFieldsAddon = class(TMembersAddon)
  public
    constructor Create; override;
    procedure EnumMeta(vItems: TmncMetaItems; vMetaItem: TmncMetaItem); override;
  end;

  { TTableFieldAddon }

  TTableFieldAddon = class(TMembersAddon)
  public
    constructor Create; override;
    procedure EnumMeta(vItems: TmncMetaItems; vMetaItem: TmncMetaItem); override;
  end;

  { TDropTableAddon }

  TDropTableAddon = class(TmndAddon)
  public
    constructor Create; override;
    procedure DoExecute(vMetaItem: TmncMetaItem); override;
  end;

  { TSelectTableAddon }

  TSelectTableAddon = class(TmndAddon)
  public
    constructor Create; override;
    procedure DoExecute(vMetaItem: TmncMetaItem); override;
  end;

  { TInsertTableAddon }

  TInsertTableAddon = class(TmndAddon)
  public
    constructor Create; override;
    procedure DoExecute(vMetaItem: TmncMetaItem); override;
  end;

  { TDropFieldAddon }

  TDropFieldAddon = class(TmndAddon)
  public
    constructor Create; override;
    procedure DoExecute(vMetaItem: TmncMetaItem); override;
  end;

  { TNewFieldAddon }

  TNewFieldAddon = class(TmndAddon)
  public
    constructor Create; override;
    procedure DoExecute(vMetaItem: TmncMetaItem); override;
  end;

  { TEmptyTableAddon }

  TEmptyTableAddon = class(TmndAddon)
  public
    constructor Create; override;
    procedure DoExecute(vMetaItem: TmncMetaItem); override;
  end;

implementation

uses
  Contnrs;

{ TTableFieldAddon }

constructor TTableFieldAddon.Create;
begin
  inherited;
  Master := 'Table';
  Name := 'Field';
  Title := 'Field';
  ItemName := 'FieldProperies';
  Kind := sokField;
  ImageIndex := IMG_FIELD;
end;

procedure TTableFieldAddon.EnumMeta(vItems: TmncMetaItems; vMetaItem: TmncMetaItem);
var
  aMeta: TmncMeta;
begin
  aMeta := DBEngine.DB.CreateMeta;
  try
    aMeta.EnumFields(vItems, vMetaItem.Name);
  finally
    aMeta.Free
  end;
end;

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

procedure TDatabasesAddon.DoExecute(vMetaItem: TmncMetaItem);
begin
  inherited;
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

procedure TDatabaseAddon.DoExecute(vMetaItem: TmncMetaItem);
begin
  inherited;
  DBEngine.ShowMeta(Self, vMetaItem, True);
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

procedure TTablesAddon.EnumMeta(vItems: TmncMetaItems; vMetaItem: TmncMetaItem);
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

procedure TProceduresAddon.EnumMeta(vItems: TmncMetaItems; vMetaItem: TmncMetaItem);
var
  aMeta: TmncMeta;
begin
  aMeta := DBEngine.DB.CreateMeta;
  try
    aMeta.EnumProcedures(vItems);
  finally
    aMeta.Free
  end;
end;

{ TViewAddon }

constructor TViewAddon.Create;
begin
  inherited;
  Master := 'Database';
  Name := 'View';
  Title := 'View';
  ItemName := 'ViewSource';
  Kind := sokView;
  ImageIndex := IMG_VIEW;
end;

procedure TViewAddon.EnumMeta(vItems: TmncMetaItems; vMetaItem: TmncMetaItem);
var
  aMeta: TmncMeta;
begin
  aMeta := DBEngine.DB.CreateMeta;
  try
    aMeta.EnumViews(vItems);
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

procedure TSequencesAddon.EnumMeta(vItems: TmncMetaItems; vMetaItem: TmncMetaItem);
var
  aMeta: TmncMeta;
begin
  aMeta := DBEngine.DB.CreateMeta;
  try
    aMeta.EnumSequences(vItems);
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

procedure TDomainsAddon.EnumMeta(vItems: TmncMetaItems; vMetaItem: TmncMetaItem);
var
  aMeta: TmncMeta;
begin
  aMeta := DBEngine.DB.CreateMeta;
  try
    aMeta.EnumDomains(vItems);
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

procedure TExceptionsAddon.EnumMeta(vItems: TmncMetaItems; vMetaItem: TmncMetaItem);
var
  aMeta: TmncMeta;
begin
  aMeta := DBEngine.DB.CreateMeta;
  try
    aMeta.EnumExceptions(vItems);
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

procedure TFunctionsAddon.EnumMeta(vItems: TmncMetaItems; vMetaItem: TmncMetaItem);
var
  aMeta: TmncMeta;
begin
  aMeta := DBEngine.DB.CreateMeta;
  try
    aMeta.EnumFunctions(vItems);
  finally
    aMeta.Free
  end;
end;

{ TMembersAddon }

constructor TMembersAddon.Create;
begin
  inherited;
end;

procedure TMembersAddon.DoExecute(vMetaItem: TmncMetaItem);
begin
  inherited;
  DBEngine.ShowMeta(Self, vMetaItem, True);
end;

function TMembersAddon.GetCanExecute: Boolean;
begin
  Result := True;
end;

{ TProcedureAddon }

constructor TProcedureAddon.Create;
begin
  inherited;
  Master := 'Database';
  Name := 'ProcedureSource';
  Title := 'Procedure Source';
  ItemName := 'Source';
  Kind := sokProcedure;
  ImageIndex := IMG_PROCEDURE;
end;

procedure TProcedureAddon.DoExecute(vMetaItem: TmncMetaItem);
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

{ TViewSourceAddon }

constructor TViewSourceAddon.Create;
begin
  inherited;
  Master := 'View';
  Name := 'ViewSource';
  Title := 'View Source';
  ItemName := '';
  Kind := sokView;
  ImageIndex := IMG_VIEW;
end;

procedure TViewSourceAddon.DoExecute(vMetaItem: TmncMetaItem);
var
  aMeta: TmncMeta;
  aStrings: TStringList;
begin
  inherited;
  aStrings := TStringList.Create;
  try
    aMeta := DBEngine.DB.CreateMeta;
    try
      aMeta.GetViewSource(aStrings, vMetaItem.Value, [ekAlter]);
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

procedure TTableTriggersAddon.EnumMeta(vItems: TmncMetaItems; vMetaItem: TmncMetaItem);
var
  aMeta: TmncMeta;
begin
  aMeta := DBEngine.DB.CreateMeta;
  try
    aMeta.EnumTriggers(vItems, {MetaItem.Values['Table']} ''); //TODO
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

procedure TTriggerAddon.DoExecute(vMetaItem: TmncMetaItem);
var
  aMeta: TmncMeta;
  aStrings: TStringList;
begin
  inherited;
  aStrings := TStringList.Create;
  try
    aMeta := DBEngine.DB.CreateMeta;
    try
      aMeta.GetTriggerSource(aStrings, vMetaItem.Values['Trigger'], [ekAlter]);
    finally
      aMeta.Free;
    end;
    DBEngine.ShowEditor(Self, aStrings);
  finally
    aStrings.Free;
  end;
end;

{ TTableIndexAddon }

constructor TTableIndexAddon.Create;
begin
  inherited;
  Master := 'Table';
  Name := 'Indices';
  Title := 'Indices';
  ItemName := 'Index';
  Kind := sokIndex;
  ImageIndex := IMG_INDEX;
end;

procedure TTableIndexAddon.EnumMeta(vItems: TmncMetaItems; vMetaItem: TmncMetaItem);
var
  aMeta: TmncMeta;
begin
  aMeta := DBEngine.DB.CreateMeta;
  try
    aMeta.EnumIndices(vItems, vMetaItem.Name);
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

procedure TDropIndexAddon.DoExecute(vMetaItem: TmncMetaItem);
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

procedure TTableFieldsAddon.EnumMeta(vItems: TmncMetaItems; vMetaItem: TmncMetaItem);
var
  aMeta: TmncMeta;
begin
  aMeta := DBEngine.DB.CreateMeta;
  try
    aMeta.EnumFields(vItems, vMetaItem.Values['Table']);
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

procedure TNewFieldAddon.DoExecute(vMetaItem: TmncMetaItem);
var
  aStrings: TStringList;
//  aFieldName: string;
begin
  inherited;
  aStrings := TStringList.Create;
  try
    //aStrings.Text := 'alter table ' + vStack['Table'] + ' drop column ' + Value;
    aStrings.Text := 'alter table ' + vMetaItem.Value + ' add ?FieldName ?FieldType';

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

procedure TEmptyTableAddon.DoExecute(vMetaItem: TmncMetaItem);
var
  aStrings: TStringList;
begin
  inherited;
  aStrings := TStringList.Create;
  aStrings.Text := 'delete from ' + vMetaItem.Value;
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

procedure TDropFieldAddon.DoExecute(vMetaItem: TmncMetaItem);
var
  aStrings: TStringList;
begin
  inherited;
  aStrings := TStringList.Create;
  try
    aStrings.Text := 'alter table ' + vMetaItem.Values['Table'] + ' drop column ' + vMetaItem.Value;
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

procedure TTriggersAddon.EnumMeta(vItems: TmncMetaItems; vMetaItem: TmncMetaItem);
var
  aMeta: TmncMeta;
begin
  aMeta := DBEngine.DB.CreateMeta;
  try
    aMeta.EnumTriggers(vItems);
  finally
    aMeta.Free
  end;
end;

{ TTableAddon }

constructor TTableAddon.Create;
begin
  inherited Create;
  Master := 'Database';
  Name := 'Table';
  Title := 'Table';
  //ItemName := 'Field'; nop it has a child Masters
  DefaultAddon := 'Field';
  Kind := sokTable;
  ImageIndex := IMG_TABLE;
end;

procedure TTableAddon.DoExecute(vMetaItem: TmncMetaItem);
begin
  inherited;
  DBEngine.ShowMeta(Self, vMetaItem, True);
end;

procedure TTableAddon.EnumMeta(vItems: TmncMetaItems; vMetaItem: TmncMetaItem);
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

{ TIndexAddon }

constructor TIndexAddon.Create;
begin
  inherited Create;
  Master := 'Database';
  Name := 'Index';
  Title := 'Index';
  Kind := sokIndex;
  ImageIndex := IMG_INDEX;
end;

procedure TIndexAddon.DoExecute(vMetaItem: TmncMetaItem);
begin
  inherited;
  //DBEngine.EnumMembers(Self, Value);
end;

procedure TIndexAddon.EnumMeta(vItems: TmncMetaItems; vMetaItem: TmncMetaItem);
var
  aMeta: TmncMeta;
begin
  aMeta := DBEngine.DB.CreateMeta;
  try
    aMeta.GetIndexInfo(vItems, vMetaItem.Values['Index']);
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

procedure TIndicesAddon.EnumMeta(vItems: TmncMetaItems; vMetaItem: TmncMetaItem);
var
  aMeta: TmncMeta;
begin
  aMeta := DBEngine.DB.CreateMeta;
  try
    aMeta.EnumIndices(vItems);
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

procedure TDropTableAddon.DoExecute(vMetaItem: TmncMetaItem);
begin
  inherited;
  DBEngine.ShowEditor(Self, 'drop table ' + vMetaItem.Values['Table']);
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

procedure TSelectTableAddon.DoExecute(vMetaItem: TmncMetaItem);
begin
  inherited;
  DumpMetaItem(vMetaItem);
  DBEngine.ShowEditor(Self, 'select * from ' + vMetaItem.Values['Table']);
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

procedure TInsertTableAddon.DoExecute(vMetaItem: TmncMetaItem);
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
      aMeta.EnumFields(aItems, vMetaItem.Values['Table']);
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
  DBEngine.ShowEditor(Self, 'insert into ' + vMetaItem.Values['Table'] + '(' + s1 + ') values (' + s2 +')');
end;

{ TExportSQLAddon }

initialization
  DBEngine.RegisterAddon([TDatabaseAddon]);
  DBEngine.RegisterAddon([TTableAddon, TIndexAddon, TViewAddon]);
  DBEngine.RegisterAddon([TTableFieldAddon, TTableIndexAddon]);
  DBEngine.RegisterAddon([TSelectTableAddon, TInsertTableAddon, TEmptyTableAddon, TDropTableAddon]);
  DBEngine.RegisterAddon([TDropIndexAddon]);
  DBEngine.RegisterAddon([TDropFieldAddon, TNewFieldAddon]);
  DBEngine.RegisterAddon([TTriggerAddon, TTableTriggersAddon]);
  DBEngine.RegisterAddon([TViewSourceAddon]);
  //DBEngine.RegisterAddon([TDomainsAddon, TExceptionsAddon, TFunctionsAddon]);
  //DBEngine.RegisterAddon([TProceduresAddon, TProcedureAddon]);
  //DBEngine.RegisterAddon([TSequencesAddon]);
end.
