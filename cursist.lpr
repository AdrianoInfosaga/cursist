program cursist;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uteis, queryutil, dbinicio, basemainform, zcomponent, rxnew
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  SplashStart;
  //Application.CreateForm(TFrmBaseMain, FrmBaseMain);
  //Application.CreateForm(TInicioDB, InicioDB);
  Application.Run;
end.

