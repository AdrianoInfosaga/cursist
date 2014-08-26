{ Unit queryutil.pas
  =================================================
  *funções e procedures uteis para manipulação SQL*
  =================================================
  Detalhes:
  - class tQueryUtil:
  - Apresenta tela tipo splash para login
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
         FdbConn: TZConnection;
         procedure SetdbConn(AValue: TZConnection);
  public
        constructor create (Connection: TZConnection);
        Property dbConn: TZConnection read FdbConn write SetdbConn;
        function BuscaUmDadoSql(sql: string): variant;
        function BuscaUmDadoSqlAsDateTime(sql: string): tdatetime;
        function BuscaUmDadoSqlAsFloat(sql: string): Extended;
        function BuscaUmDadoSqlAsInteger(sql: string): integer;
        function BuscaUmDadoSqlAsString(sql: string): string;
        function QueryCreate(sql: string; open: boolean): TZQuery;
        procedure QueryDestroy(QueryObject: TZQuery);
  end;

implementation

{ tQueryUtil }

procedure tQueryUtil.SetdbConn(AValue: TZConnection);
begin
  if FdbConn=AValue then Exit;
     FdbConn:=AValue;
end;

constructor tQueryUtil.create(Connection: TZConnection);
begin
     dbConn := Connection;
end;

function tQueryUtil.BuscaUmDadoSql(sql: string): variant;
{executa query e retorna o valor obtido}
var qry: tZQuery;
begin
     try
       qry:=QueryCreate(sql,true);
       result := qry.Fields[0].AsVariant;
     finally
       qry.close;
       FreeAndNil(qry);
     end;
end;

function tQueryUtil.BuscaUmDadoSqlAsInteger(sql: string): integer;
{executa query e retorna o valor obtido}
begin
     result := BuscaUmDadoSql(sql);
end;

function tQueryUtil.BuscaUmDadoSqlAsFloat(sql: string): Extended;
{executa query e retorna o valor obtido}
begin
     result := BuscaUmDadoSql(sql);
end;

function tQueryUtil.BuscaUmDadoSqlAsDateTime(sql: string): tdatetime;
{executa query e retorna o valor obtido}
begin
     result := BuscaUmDadoSql(sql);
end;

function tQueryUtil.BuscaUmDadoSqlAsString(sql: string): string;
{executa query e retorna o valor obtido}
begin
     result := BuscaUmDadoSql(sql);
end;

function tQueryUtil.QueryCreate(sql: string; open:boolean): TZQuery;
{configura e retorna objeto TZQuery}
begin
     QueryCreate:=tZQuery.Create(dbConn.Owner);
     QueryCreate.Connection:=dbConn;
     QueryCreate.SQL.text:=sql;
     if open then
        QueryCreate.open;
End;

Procedure tQueryUtil.QueryDestroy(QueryObject: TZQuery);
begin
  QueryObject.Close;
  FreeAndNil(QueryObject);
end;

end.

