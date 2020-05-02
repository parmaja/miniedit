unit sqlvStdClasses;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$mode objfpc}{$H+}

{
  Schema --------------------------
     |                 |               |
     |                 |               |
     |                 |               |
   Group             Group           Group
     |---------------
     |       |       |
   Member  Member  Member

   Open Schema = List the groups and open the first (Default) group
   Open Group = List the members of this group in the members list
   Open Member = Make the member as Schema and open it as Schema
}

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms, StdCtrls,
  mncMeta, mnUtils,
  sqlvSessions, sqlvClasses;

type

  { TsqlvDatabase }

  TsqlvDatabase = class(TsqlvAddon)
  private
  public
    constructor Create; override;
    procedure DoExecute(vAttributes: TsqlvAttributes); override;
  end;

  { TsqlvMembers }

  TsqlvMembers = class(TsqlvAddon)
  private
  protected
    function GetCanExecute: Boolean; override;
  public
    constructor Create; override;
    procedure DoExecute(vAttributes: TsqlvAttributes); override;
  end;

  { TsqlvTables }

  TsqlvTables = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure EnumMeta(vItems: TmncMetaItems; vAttributes: TsqlvAttributes); override;
  end;

  TsqlvTable = class(TsqlvAddon)
  public
    constructor Create; override;
    procedure DoExecute(vAttributes: TsqlvAttributes); override;
  end;

  { TsqlvIndex }

  TsqlvIndex = class(TsqlvAddon)
  public
    constructor Create; override;
    procedure DoExecute(vAttributes: TsqlvAttributes); override;
    procedure EnumHeader(Header: TStringList); override;
    procedure EnumMeta(MetaItems: TmncMetaItems; vAttributes: TsqlvAttributes); override;
  end;

  { TsqlvProcedures }

  TsqlvProcedures = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure EnumMeta(MetaItems: TmncMetaItems; vAttributes: TsqlvAttributes); override;
  end;

  { TsqlvProcedure }

  TsqlvProcedure = class(TsqlvAddon)
  public
    constructor Create; override;
    procedure DoExecute(vAttributes: TsqlvAttributes); override;
  end;

  { TsqlvViews }

  TsqlvViews = class(TsqlvMembers)
  public
    procedure EnumMeta(MetaItems: TmncMetaItems; vAttributes: TsqlvAttributes); override;
    constructor Create; override;
  end;

  { TsqlvView }

  TsqlvView = class(TsqlvAddon)
  public
    constructor Create; override;
    procedure DoExecute(vAttributes: TsqlvAttributes); override;
  end;

  { TsqlvSequences }

  TsqlvSequences = class(TsqlvMembers)
  public
    procedure EnumMeta(MetaItems: TmncMetaItems; vAttributes: TsqlvAttributes); override;
    constructor Create; override;
  end;

  { TsqlvDomains }

  TsqlvDomains = class(TsqlvMembers)
  public
    procedure EnumMeta(MetaItems: TmncMetaItems; vAttributes: TsqlvAttributes); override;
    constructor Create; override;
  end;

  { TsqlvExceptions }

  TsqlvExceptions = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure EnumMeta(MetaItems: TmncMetaItems; vAttributes: TsqlvAttributes); override;
  end;

  { TsqlvFunctions }

  TsqlvFunctions = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure EnumMeta(MetaItems: TmncMetaItems; vAttributes: TsqlvAttributes); override;
  end;

  { TsqlvTriggers }

  TsqlvTriggers = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure EnumMeta(MetaItems: TmncMetaItems; vAttributes: TsqlvAttributes); override;
  end;

  { TsqlvTableTriggers }

  TsqlvTableTriggers = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure EnumMeta(MetaItems: TmncMetaItems; vAttributes: TsqlvAttributes); override;
  end;

  TsqlvTrigger = class(TsqlvAddon)
  public
    constructor Create; override;
    procedure DoExecute(vAttributes: TsqlvAttributes); override;
  end;

  { TsqlvIndices }

  TsqlvIndices = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure EnumHeader(Header: TStringList); override;
    procedure EnumMeta(MetaItems: TmncMetaItems; vAttributes: TsqlvAttributes); override;
  end;

  { TsqlvTableIndices }

  TsqlvTableIndices = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure EnumHeader(Header: TStringList); override;
    procedure EnumMeta(MetaItems: TmncMetaItems; vAttributes: TsqlvAttributes); override;
  end;

  { TsqlvDropIndex }

  TsqlvDropIndex = class(TsqlvAddon)
  public
    constructor Create; override;
    procedure DoExecute(vAttributes: TsqlvAttributes); override;
  end;

  { TsqlvTableFields }

  TsqlvTableFields = class(TsqlvMembers)
  public
    constructor Create; override;
    procedure EnumHeader(Header: TStringList); override;
    procedure EnumMeta(MetaItems: TmncMetaItems; vAttributes: TsqlvAttributes); override;
  end;

  { TsqlvDropTable }

  TsqlvDropTable = class(TsqlvAddon)
  public
    constructor Create; override;
    procedure DoExecute(vAttributes: TsqlvAttributes); override;
  end;

  { TsqlvSelectTable }

  TsqlvSelectTable = class(TsqlvAddon)
  public
    constructor Create; override;
    procedure DoExecute(vAttributes: TsqlvAttributes); override;
  end;

  { TsqlvInsertTable }

  TsqlvInsertTable = class(TsqlvAddon)
  public
    constructor Create; override;
    procedure DoExecute(vAttributes: TsqlvAttributes); override;
  end;

  { TsqlvDropField }

  TsqlvDropField = class(TsqlvAddon)
  public
    constructor Create; override;
    procedure DoExecute(vAttributes: TsqlvAttributes); override;
  end;

  { TsqlvNewField }

  TsqlvNewField = class(TsqlvAddon)
  public
    constructor Create; override;
    procedure DoExecute(vAttributes: TsqlvAttributes); override;
  end;

  { TsqlvEmptyTable }

  TsqlvEmptyTable = class(TsqlvAddon)
  public
    constructor Create; override;
    procedure DoExecute(vAttributes: TsqlvAttributes); override;
  end;

implementation

uses
  Contnrs;

{ TsqlvDatabase }

constructor TsqlvDatabase.Create;
begin
  inherited;
  Group := 'Databases';
  Name := 'Database';
  Title := 'Database';
  Kind := sokDatabase;
  ImageIndex := IMG_DATABASE;
end;

procedure TsqlvDatabase.DoExecute(vAttributes: TsqlvAttributes);
begin
  inherited;
  DBEngine.ShowMeta(Self, True);
end;

{ TsqlvTables }

constructor TsqlvTables.Create;
begin
  inherited;
  Group := 'Database';
  Name := 'Tables';
  Title := 'Tables';
  ItemName := 'Table';
  Kind := sokTable;
  ImageIndex := IMG_TABLE;
end;

procedure TsqlvTables.EnumMeta(vItems: TmncMetaItems; vAttributes: TsqlvAttributes);
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

{ TsqlvProcedures }

constructor TsqlvProcedures.Create;
begin
  inherited;
  Group := 'Database';
  Name := 'Procedures';
  Title := 'Procedures';
  ItemName := 'Procedure';
  Kind := sokProcedure;
  ImageIndex := IMG_PROCEDURE;
end;

procedure TsqlvProcedures.EnumMeta(MetaItems: TmncMetaItems; vAttributes: TsqlvAttributes);
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

{ TsqlvViews }

constructor TsqlvViews.Create;
begin
  inherited;
  Group := 'Database';
  Name := 'Views';
  Title := 'Views';
  ItemName := 'View';
  Kind := sokView;
  ImageIndex := IMG_VIEW;
end;

procedure TsqlvViews.EnumMeta(MetaItems: TmncMetaItems; vAttributes: TsqlvAttributes);
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

{ TsqlvSequences }

constructor TsqlvSequences.Create;
begin
  inherited;
  Group := 'Database';
  Name := 'Sequences';
  Title := 'Sequences';
  ItemName := 'Sequence';
  Kind := sokSequence;
  ImageIndex := IMG_GENERATOR;
end;

procedure TsqlvSequences.EnumMeta(MetaItems: TmncMetaItems; vAttributes: TsqlvAttributes);
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

{ TsqlvDomains }

constructor TsqlvDomains.Create;
begin
  inherited;
  Group := 'Database';
  Name := 'Domains';
  Title := 'Domains';
  ItemName := 'Domain';
  Kind := sokDomain;
  ImageIndex := IMG_DOMAIN;
end;

procedure TsqlvDomains.EnumMeta(MetaItems: TmncMetaItems; vAttributes: TsqlvAttributes);
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

{ TsqlvExceptions }

constructor TsqlvExceptions.Create;
begin
  inherited;
  Group := 'Database';
  Name := 'Exceptions';
  Title := 'Exceptions';
  ItemName := 'Exception';
  Kind := sokException;
  ImageIndex := IMG_EXCEPTION;
end;

procedure TsqlvExceptions.EnumMeta(MetaItems: TmncMetaItems; vAttributes: TsqlvAttributes);
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

{ TsqlvFunctions }

constructor TsqlvFunctions.Create;
begin
  inherited;
  Group := 'Database';
  Name := 'Functions';
  Title := 'Functions';
  ItemName := 'Function';
  Kind := sokFunction;
  ImageIndex := IMG_FUNCTION;
end;

procedure TsqlvFunctions.EnumMeta(MetaItems: TmncMetaItems; vAttributes: TsqlvAttributes);
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

{ TsqlvMembers }

constructor TsqlvMembers.Create;
begin
  inherited;
end;

procedure TsqlvMembers.DoExecute(vAttributes: TsqlvAttributes);
begin
  inherited;
  //sqlvGui.ShowMeta(Self, Value);
end;

function TsqlvMembers.GetCanExecute: Boolean;
begin
  Result := True;
end;

{ TsqlvProcedure }

constructor TsqlvProcedure.Create;
begin
  inherited;
  Group := 'Procedure';
  Name := 'ProcedureSource';
  Title := 'Procedure Source';
  ItemName := 'Source';
  Kind := sokProcedure;
  ImageIndex := IMG_PROCEDURE;
end;

procedure TsqlvProcedure.DoExecute(vAttributes: TsqlvAttributes);
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
    DBEngine.LoadEditor(Self, aStrings);
  finally
    aStrings.Free;
  end;
end;

{ TsqlvView }

constructor TsqlvView.Create;
begin
  inherited;
  Group := 'View';
  Name := 'ViewSource';
  Title := 'View Source';
  ItemName := 'Source';
  Kind := sokView;
  ImageIndex := IMG_VIEW;
end;

procedure TsqlvView.DoExecute(vAttributes: TsqlvAttributes);
var
  aMeta: TmncMeta;
  aStrings: TStringList;
begin
  inherited;
  aStrings := TStringList.Create;
  try
    aMeta := DBEngine.DB.CreateMeta;
    try
      aMeta.GetViewSource(aStrings, vAttributes['View'], [ekAlter]);
    finally
      aMeta.Free;
    end;
    DBEngine.LoadEditor(Self, aStrings);
  finally
    aStrings.Free;
  end;
end;

{ TsqlvTableTriggers }

constructor TsqlvTableTriggers.Create;
begin
  inherited;
  Group := 'Table';
  Name := 'Triggers';
  Title := 'Triggers';
  Kind := sokTrigger;
  ImageIndex := IMG_TRIGGER;
end;

procedure TsqlvTableTriggers.EnumMeta(MetaItems: TmncMetaItems; vAttributes: TsqlvAttributes);
var
  aMeta: TmncMeta;
begin
  aMeta := DBEngine.DB.CreateMeta;
  try
    aMeta.EnumTriggers(MetaItems, vAttributes['Table']);
  finally
    aMeta.Free;
  end;
end;

{ TsqlvTrigger }

constructor TsqlvTrigger.Create;
begin
  inherited;
  Group := 'Triggers';
  Name := 'Trigger';
  Title := 'Trigger';
  Kind := sokTrigger;
  ImageIndex := IMG_TRIGGER;
end;

procedure TsqlvTrigger.DoExecute(vAttributes: TsqlvAttributes);
var
  aMeta: TmncMeta;
  aStrings: TStringList;
begin
  inherited;
  aStrings := TStringList.Create;
  try
    aMeta := DBEngine.DB.CreateMeta;
    try
      aMeta.GetTriggerSource(aStrings, vAttributes['Trigger'], [ekAlter]);
    finally
      aMeta.Free;
    end;
    DBEngine.LoadEditor(Self, aStrings);
  finally
    aStrings.Free;
  end;
end;

{ TsqlvTableIndices }

constructor TsqlvTableIndices.Create;
begin
  inherited;
  Group := 'Table';
  Name := 'Indices';
  Title := 'Indices';
  ItemName := 'Index';
  Kind := sokIndex;
  ImageIndex := IMG_INDEX;
end;

procedure TsqlvTableIndices.EnumHeader(Header: TStringList);
begin
  inherited EnumHeader(Header);
  Header.Add('Unique');
end;

procedure TsqlvTableIndices.EnumMeta(MetaItems: TmncMetaItems; vAttributes: TsqlvAttributes);
var
  aMeta: TmncMeta;
begin
  aMeta := DBEngine.DB.CreateMeta;
  try
    aMeta.EnumIndices(MetaItems, vAttributes['Table']);
  finally
    aMeta.Free
  end;
end;

{ TsqlvDropIndex }

constructor TsqlvDropIndex.Create;
begin
  inherited;
  Group := 'Index';
  Name := 'DropIndex';
  Title := 'Drop Index';
  Kind := sokIndex;
  Style := Style + [nsCommand];
  ImageIndex := IMG_INDEX;
end;

procedure TsqlvDropIndex.DoExecute(vAttributes: TsqlvAttributes);
begin
  inherited;

end;

{ TsqlvTableFields }

constructor TsqlvTableFields.Create;
begin
  inherited;
  Group := 'Table';
  Name := 'Fields';
  Title := 'Fields';
  ItemName := 'Field';
  Kind := sokField;
  ImageIndex := IMG_FIELD;
end;

procedure TsqlvTableFields.EnumHeader(Header: TStringList);
begin
  inherited;
  Header.Add('Type');
  Header.Add('PK');//primiry key
  Header.Add('NN');//not null
  Header.Add('Default');
end;

procedure TsqlvTableFields.EnumMeta(MetaItems: TmncMetaItems; vAttributes: TsqlvAttributes);
var
  aMeta: TmncMeta;
begin
  aMeta := DBEngine.DB.CreateMeta;
  try
    aMeta.EnumFields(MetaItems, vAttributes['Table']);
  finally
    aMeta.Free
  end;
end;

{ TsqlvNewField }

constructor TsqlvNewField.Create;
begin
  inherited;
  Group := 'Table';
  Name := 'NewField';
  Title := 'New Field';
  Kind := sokField;
  Style := Style + [nsCommand];
  ImageIndex := IMG_FIELD;
end;

procedure TsqlvNewField.DoExecute(vAttributes: TsqlvAttributes);
var
  aStrings: TStringList;
//  aFieldName: string;
begin
  inherited;
  aStrings := TStringList.Create;
  try
    //aStrings.Text := 'alter table ' + vStack['Table'] + ' drop column ' + Value;
    aStrings.Text := 'alter table ' + vAttributes['Table'] + ' add ?FieldName ?FieldType';

    DBEngine.LoadEditor(Self, aStrings);
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

{ TsqlvEmptyTable }

constructor TsqlvEmptyTable.Create;
begin
  inherited;
  Group := 'Table';
  Name := 'EmptyTable';
  Title := 'Empty';
  Kind := sokNone;
  Style := [nsCommand];
  ImageIndex := IMG_COMMAND;
end;

procedure TsqlvEmptyTable.DoExecute(vAttributes: TsqlvAttributes);
var
  aStrings: TStringList;
begin
  inherited;
  aStrings := TStringList.Create;
  aStrings.Text := 'delete from ' + vAttributes['Table'];
  DBEngine.LoadEditor(Self, aStrings);
  aStrings.Free;
end;

{ TsqlvDropField }

constructor TsqlvDropField.Create;
begin
  inherited;
  Group := 'Field';
  Name := 'DropField';
  Title := 'Drop Field';
  Kind := sokField;
  Style := Style + [nsCommand];
  ImageIndex := IMG_FIELD;
end;

procedure TsqlvDropField.DoExecute(vAttributes: TsqlvAttributes);
var
  aStrings: TStringList;
begin
  inherited;
  aStrings := TStringList.Create;
  try
    aStrings.Text := 'alter table ' + vAttributes['Table'] + ' drop column ' + vAttributes['Field'];
    DBEngine.LoadEditor(Self, aStrings);
  finally
    aStrings.Free;
  end;
end;

{ TsqlvTriggers }

constructor TsqlvTriggers.Create;
begin
  inherited Create;
  Group := 'Database';
  Name := 'Triggers';
  Title := 'Triggers';
  ItemName := 'Trigger';
  Kind := sokTrigger;
  ImageIndex := IMG_TRIGGER;
end;

procedure TsqlvTriggers.EnumMeta(MetaItems: TmncMetaItems; vAttributes: TsqlvAttributes);
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

{ TsqlvTable }

constructor TsqlvTable.Create;
begin
  inherited Create;
  Group := 'Tables';
  Name := 'Table';
  Title := 'Table';
  //ItemName := 'Field'; nop it has a child groups
  Kind := sokTable;
  ImageIndex := IMG_TABLE;
end;

procedure TsqlvTable.DoExecute(vAttributes: TsqlvAttributes);
begin
  inherited;
  DBEngine.ShowMeta(Self, True);
end;

{ TsqlvIndex }

constructor TsqlvIndex.Create;
begin
  inherited Create;
  Group := 'Indices';
  Name := 'Index';
  Title := 'Index';
  Kind := sokIndex;
  ImageIndex := IMG_INDEX;
end;

procedure TsqlvIndex.DoExecute(vAttributes: TsqlvAttributes);
begin
  inherited;
  //DBEngine.EnumMembers(Self, Value);
end;

procedure TsqlvIndex.EnumHeader(Header: TStringList);
begin
  inherited EnumHeader(Header);
  Header.Add('Value');
end;

procedure TsqlvIndex.EnumMeta(MetaItems: TmncMetaItems; vAttributes: TsqlvAttributes);
var
  aMeta: TmncMeta;
begin
  aMeta := DBEngine.DB.CreateMeta;
  try
    aMeta.GetIndexInfo(MetaItems, vAttributes['Index']);
  finally
    aMeta.Free
  end;
end;

{ TsqlvIndices }

constructor TsqlvIndices.Create;
begin
  inherited Create;
  Group := 'Database';
  Name := 'Indices';
  Title := 'Indices';
  ItemName := 'Index';
  Kind := sokIndex;
  ImageIndex := IMG_INDEX;
end;

procedure TsqlvIndices.EnumHeader(Header: TStringList);
begin
  inherited;
end;

procedure TsqlvIndices.EnumMeta(MetaItems: TmncMetaItems; vAttributes: TsqlvAttributes);
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

{ TsqlvDropTable }

constructor TsqlvDropTable.Create;
begin
  inherited Create;
  Group := 'Table';
  Name := 'DropTable';
  Title := 'Drop';
  Kind := sokTable;
  Style := Style + [nsCommand];
  ImageIndex := IMG_TABLE;
end;

procedure TsqlvDropTable.DoExecute(vAttributes: TsqlvAttributes);
begin
  inherited;
  DBEngine.LoadEditor(Self, 'drop table ' + vAttributes['Table']);
end;

{ TsqlvSelectTable }

constructor TsqlvSelectTable.Create;
begin
  inherited Create;
  Group := 'Table';
  Name := 'SelectTable';
  Title := 'Select';
  Kind := sokTable;
  Style := Style + [nsCommand];
  ImageIndex := IMG_TABLE;
end;

procedure TsqlvSelectTable.DoExecute(vAttributes: TsqlvAttributes);
begin
  inherited;
  DBEngine.LoadEditor(Self, 'select * from ' + vAttributes['Table']);
end;

{ TsqlvInsertTable }

constructor TsqlvInsertTable.Create;
begin
  inherited Create;
  Group := 'Table';
  Name := 'InsertTable';
  Title := 'Insert';
  Kind := sokTable;
  Style := Style + [nsCommand];
  ImageIndex := IMG_TABLE;
end;

procedure TsqlvInsertTable.DoExecute(vAttributes: TsqlvAttributes);
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
      aMeta.EnumFields(aItems, vAttributes['Table']);
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
  DBEngine.LoadEditor(Self, 'insert into ' + vAttributes['Table'] + '(' + s1 + ') values (' + s2 +')');
end;

{ TsqlvExportSQL }

initialization
  DBEngine.RegisterViewer([TsqlvDatabase]);
  DBEngine.RegisterViewer([TsqlvTables, TsqlvTable, TsqlvTableFields]);
  DBEngine.RegisterViewer([TsqlvSelectTable, TsqlvInsertTable, TsqlvEmptyTable, TsqlvDropTable]);
  DBEngine.RegisterViewer([TsqlvIndices, TsqlvTableIndices, TsqlvIndex, TsqlvDropIndex]);
  DBEngine.RegisterViewer([TsqlvDropField, TsqlvNewField]);
  DBEngine.RegisterViewer([TsqlvViews, TsqlvView]);
  DBEngine.RegisterViewer([TsqlvTriggers, TsqlvTrigger, TsqlvTableTriggers]);
  //DBEngine.RegisterViewer([TsqlvDomains, TsqlvExceptions, TsqlvFunctions]);
  //DBEngine.RegisterViewer([TsqlvProcedures, TsqlvProcedure]);
  //DBEngine.RegisterViewer([TsqlvSequences]);
end.
