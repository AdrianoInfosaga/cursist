{ Unit uteis.pas
  ================================================
  *funções e procedures para finalidades diversas*
  ================================================
  Adriano Alexandre Adami - 2014
  http://contribuicoes.wordpress.com
}

unit uteis;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

//criado com basemainform
Function HasTextaValidDate( DtStr: string ) : boolean; // testa se string possui data válida
Function ExtrairNumeros(msg:string):string; // Extrai digitos de uma string
Function LastDay( dtRef: tDate ) : Integer ; // retorna ultimo dia do mês
//criado com dbinicio
Function InsereBarraDiretorio(lPath:string):string; // Insere barra ao final do caminho informado

implementation

Function HasTextaValidDate( DtStr: string ) : boolean;
{ testa se string possui uma data valida }
var
  dd: Integer;
  mm: Integer;
  yy: Integer;
  ultdia: Integer;
  DateStr: String;
begin
     result := false ;
     DateStr := ExtrairNumeros( DtStr ) ;
     if length(DateStr)=8 then
     begin
          dd := strtointdef( copy ( datestr , 1 , 2 ) , 0 );
          mm := strtointdef( copy ( datestr , 3 , 2 ) , 0 );
          yy := strtointdef( copy ( datestr , 5 , 4 ) , 0 );
          if (dd>0) and (mm>0) and (yy>1900) then
          begin
               if (mm<13) then // mes válido
               begin
                    ultdia := LastDay ( encodedate ( yy,mm,1 ) );
                    if dd <= ultdia then
                       result := true;
               end;
          end;
     end;
end;

function ExtrairNumeros(msg: string): string;
{ extrai digitos de uma string }
var x,y:integer;
    num:string;
begin
     num:='';
     x := length(msg);
     for y:=1 to x do
     begin
          if pos(msg[y],'1234567890')<>0 then
             num := num + msg[y];
     end;
     result := num;
end;

Function LastDay( dtRef: tDate ) : Integer ;
{ retorna último dia do mês }
var
  ano: word;
  mes: word;
  dia: word;
begin
     decodedate( dtref, ano, mes, dia);
     mes := mes + 1;
     if mes = 13 then
     begin
          mes := 1;
          ano := ano + 1;
     end;
     dia := 1;
     DecodeDate ( EncodeDate ( ano, mes, dia )-1 , ano, mes, dia);
     result := dia ;
end;

Function InsereBarraDiretorio(lPath:string):string;
{Insere barra ao final do caminho informado}
begin
     if lPath[length(lPath)]='\' then
        Result:=lPath
     Else
         Result := lPath + '\';
end;

end.

