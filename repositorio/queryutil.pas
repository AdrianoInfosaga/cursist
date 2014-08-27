{ Unit queryutil.pas
  =================================================
  *funções e procedures uteis para manipulação SQL*
  =================================================
  Detalhes:
  - class tQueryUtil:
  -       CreateQuery(sql) -implementa operação SQL com tabelas
  -       BuscaUmDadoSql...(sql) obtenção de dados nas tabelas
  -       ExecSql(sql) - update, insert
  -       manipula transações
  -       pesquisa metadados (tabelas, campos)
  - DateToSql(dt) - retorna data como string sql
  Observações:
  - Sqlite3
  ==================================
  Adriano Alexandre Adami - 2014
  http://contribuicoes.wordpress.com
}
unit queryutil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  {adicionadas} ZConnection, ZDataset;

type
  {tQueryUtil}
  tQueryUtil = class
  private
         FInTransaction:boolean;
         FdbConn: TZConnection;
         function BuscaUmDadoSql(sql: string; defaultvalue: variant): variant;
         procedure SetdbConn(AValue: TZConnection);
  public
        constructor create (Connection: TZConnection);
        Property dbConn: TZConnection read FdbConn write SetdbConn;
        Property InTransaction:boolean read FInTransaction write fInTransaction;
        function BuscaUmDadoSqlAsDateTime(sql: string): tdatetime;
        function BuscaUmDadoSqlAsFloat(sql: string): Extended;
        function BuscaUmDadoSqlAsInteger(sql: string): integer;
        function BuscaUmDadoSqlAsString(sql: string): string;
        function QueryCreate(sql: string): TZQuery;
        procedure QueryDestroy(var QueryObject: TZQuery);
        function campoexiste(nometabela,nomecampo:string):boolean;
        Procedure ExecSql( stt: string );
        function ListaNomesTabelas: tZquery;
        procedure begintransaction;
        procedure commit;
        procedure rollback;
        procedure commitretain;
  end;

  Function DateToSql( dt: tDatetime ):string;

implementation

uses uteis;

function DateToSql(dt: tDatetime): string;
{ retorna string com a data no formato sql }
var y,m,d: word;
begin
     decodedate( dt, y,m,d);
     result := qstr( strzero(y,4)+'-'+strzero(m,2)+'-'+strzero(d,2) );
end;

{ tQueryUtil }

procedure tQueryUtil.SetdbConn(AValue: TZConnection);
begin
  if FdbConn=AValue then Exit;
     FdbConn:=AValue;
end;

constructor tQueryUtil.create(Connection: TZConnection);
begin
     dbConn := Connection;
     InTransaction:=False;
end;

function tQueryUtil.BuscaUmDadoSql(sql: string; defaultvalue: variant): variant;
{executa query e retorna o variant obtido}
var qry: tZQuery;
begin
     try
       qry:=QueryCreate(sql);
       if qry.Fields[0].Value<>null then
          result:=qry.Fields[0].Value
       else
          result:=defaultvalue;
     finally
       QueryDestroy(qry)
     end;
end;

function tQueryUtil.BuscaUmDadoSqlAsInteger(sql: string): integer;
{executa query e retorna o valor obtido}
begin
     result:=buscaumdadosql( sql, 0 );
end;

function tQueryUtil.BuscaUmDadoSqlAsFloat(sql: string): Extended;
{executa query e retorna o valor obtido}
begin
     result:=buscaumdadosql( sql, 0.0 );
end;

function tQueryUtil.BuscaUmDadoSqlAsDateTime(sql: string): tdatetime;
{executa query e retorna o valor obtido}
begin
     result:=buscaumdadosql( sql, 0.0 );
end;

function tQueryUtil.BuscaUmDadoSqlAsString(sql: string): string;
{executa query e retorna o valor obtido}
begin
     result:=buscaumdadosql( sql, '' );
end;

function tQueryUtil.QueryCreate(sql: string): TZQuery;
{configura e retorna objeto TZQuery}
begin
     QueryCreate:=tZQuery.Create(dbConn.Owner);
     QueryCreate.Connection:=dbConn;
     QueryCreate.SQL.text:=sql;
     QueryCreate.open;
End;

procedure tQueryUtil.QueryDestroy(var QueryObject: TZQuery);
begin
  QueryObject.Close;
  FreeAndNil(QueryObject);
end;

function tQueryUtil.campoexiste(nometabela, nomecampo: string): boolean;
{ testa se determinado campo existe em determinada tabela }
var qry: tzQuery;
begin
     campoexiste:=false;
     try
       qry:=querycreate('PRAGMA table_info('+nometabela+')');
       while not qry.eof do
       begin
         if qry.fieldbyname('name').asstring=nomecampo then
         begin
           campoexiste:=true;
           break;
         end;
         qry.next;
       end;
     finally
       querydestroy(qry);
     end;
end;

procedure tQueryUtil.ExecSql(stt: string);
var qry: tZquery;
begin
     try
        qry:=tZQuery.Create(nil);
        qry.Connection:=dbConn;
        qry.SQL.Text:=stt;
        qry.Prepare;
        qry.ExecSQL;
     finally
       FreeAndNil(qry);
     end;
end;

function tQueryUtil.ListaNomesTabelas: tZquery;
begin
     ListaNomesTabelas := QueryCreate('select name from sqlite_master where type='+qstr('table')+' order by name');
end;

procedure tQueryUtil.begintransaction;
begin
     if not InTransaction then
        ExecSql('BEGIN TRANSACTION');
     InTransaction := true;
end;

procedure tQueryUtil.commit;
begin
     if InTransaction then
        ExecSql('COMMIT TRANSACTION');
     inTransaction:=False;
end;

procedure tQueryUtil.commitretain;
begin
     commit;
     begintransaction;
end;


procedure tQueryUtil.rollback;
begin
     if InTransaction then
        ExecSql('ROLLBACK TRANSACTION');
     InTransaction:=False;
end;

end.

