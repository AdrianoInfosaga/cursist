{ Unit dbinicio.pas
  ===========================================================
  *inicialização da base de dados e autenticação de usuário*
  ===========================================================
  Comportamentos:
  - Lê localização da base de dados no arquivo config.ini
  - Conecta a base de dados
  - Apresenta tela tipo splash para login
  - Guarda dados relativos a instituição
  - Autentica o usuário
  - Guarda dados relativos ao usuário
  Detalhes:
  - SQLite-3
  ==================================
  Adriano Alexandre Adami - 2014
  http://contribuicoes.wordpress.com
}
unit dbinicio;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, basemainform, ZConnection,
  {adicionadas} windows, IniFiles, queryutil, ZCompatibility, ZDbcIntfs, ZDataset, db;

type

  { tUsuario }
  tUsuario = record
    nome: string[20];
    perfil: string[1];
  end;

  { tInstituicao }
  tInstituicao = record
     nome:string[40];
     rua:string[40];
     numero:string[5];
     compl:string[10];
     bairro:string[20];
     cidade:string[30];
     uf:string[2];
     cep:string[8];
     //logo:tPicture;
  end;

  { TInicioDB }

  TInicioDB = class(TFrmBaseMain)
    btCancel: TButton;
    btOK: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    edNomeSist: TLabel;
    edVer: TLabel;
    MainDB: TZConnection;
    Painel1: TPanel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure btOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    fDbUtil: tQueryUtil;
    vPastaDB: string;
    vPastaSistema: string;
    vUsuario: tUsuario;
    vInstituicao: tInstituicao;
    procedure IniciaDB(sqlDB: tZConnection);
    procedure LerDadosInstituicao;
    procedure ReadIniFile;
    function VerificaLogin(nm, psw: string): boolean;
  public
        Property PastaDB: string read vPastaDB write vPastaDB;
        Property PastaSistema: string read vPastaSistema write vPastaSistema;
        Property Query: tQueryUtil read fDbUtil write fDbUtil;
        Property Usuario: tUsuario read vUsuario write vUsuario;
        Property Instituicao: tInstituicao read vInstituicao write vInstituicao;
  end;

var
  InicioDB: TInicioDB;

function SplashStart: Boolean;

implementation

uses uteis;

{$R *.lfm}

function SplashStart: Boolean;
{ inicialização db e autenticação usuário }
begin
     SplashStart := False;
     try
        if assigned(InicioDB) then
        begin
             InicioDB.Close;
             FreeAndNil(InicioDB);
        end;
        InicioDB:=TInicioDB.Create(Nil);

        with InicioDB do
        begin

             ReadIniFile;
             IniciaDB( MainDB );
             LerDadosInstituicao;

             ShowModal;
             SplashStart := ModalResult=mrOK;

        end;
     Except
           SplashStart:=False;
     end;
end;

{ TInicioDB }

procedure TInicioDB.FormCreate(Sender: TObject);
begin
  inherited;
  Edit1.Clear;
  Edit2.Clear;
  self.Width:=490;
  self.Height:=280;
  PastaDB := '';
  PastaSistema := '';
  Query:=tQueryUtil.Create(MainDB);
  with usuario do
  begin
       nome := '';
       perfil:='';
  end;
  with instituicao do
  begin
       nome:='';
       rua:='';
       numero:='';
       compl:='';
       bairro:='';
       cidade:='';
       uf:='';
       cep:='';
       //logo:=tPicture.Create;
  end;
  {self.onkeydown:=@FormKeyDown}
end;

procedure TInicioDB.btOKClick(Sender: TObject);
var flg : boolean;
begin
     flg := VerificaLogin ( Trim(Edit1.Text), Trim(Edit2.Text) );
     if flg then
        modalresult := mrOk
     else
     begin
          Showmessage('Login ou senha inválida!');
          Edit1.clear;
          Edit2.Clear;
          Edit1.setfocus;
     end;
end;

procedure TInicioDB.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
     inherited;
     if GetKeyState( VK_CONTROL ) and 128 > 0 then // control pressionado
     begin
          if key = VK_RETURN then
             btOk.Click;
     end
end;

procedure TInicioDB.ReadIniFile;
{ lê ou cria caso não exista o arquivo config.ini }
var ini: tIniFile;
    ininame: string ;
begin
     PastaDB:=InsereBarraDiretorio(ExtractFilePath( application.exename ));
     PastaSistema:=PastaDB;
     ininame := PastaDB + 'config.ini';
     ini := tIniFile.Create ( ininame );
     try
        PastaDB := InsereBarraDiretorio(ini.ReadString('geral','PastaDB',PastaDB));
        if not fileexists( ininame ) then
        begin
             ini.WriteString('geral','PastaDB',PastaDB);
        end
        Else
        begin
             PastaDB := ini.ReadString('geral','PastaDB',PastaDB);
        end;
        PastaDB := PastaDB + 'db\';
     Finally
            FreeAndNil(ini);
     end;
end;

procedure TInicioDB.IniciaDB(sqlDB: tZConnection);
{inicializar a base de dados}
begin
     if sqlDB.Connected then
     begin
          if sqlDB.InTransaction then
             sqlDB.Rollback;
          sqlDB.Connected:=False;
     end;
     sqlDB.ControlsCodePage:=cGET_ACP; // GET_AnsiCodePage ( lc_ctype=win1252 )
     sqlDB.ClientCodePage:='WIN1252';
     sqlDB.Database:=PastaDB+'cursodb.sqlite3';
     sqlDB.HostName:='';
     sqlDB.LibraryLocation:=PastaSistema+'sqlite3.dll';
     sqlDB.LoginPrompt:=false;
     sqlDB.Properties.Clear;
     sqlDB.Properties.Add('controls_cp=GET_ACP');
     sqlDB.Properties.Add('lc_ctype=win1252'); // codepage
     sqlDB.Properties.Add('AutoEncodeStrings=false');
     sqlDB.Protocol:='sqlite-3';
     sqlDB.TransactIsolationLevel:=tiReadCommitted;
     try
        sqlDB.Connected:=true;
     Except
           Raise Exception.Create('Não foi possível conectar a '+sqlDB.Database+' em '+sqlDb.HostName);
     end;
end;

function TInicioDB.VerificaLogin( nm, psw: string ):boolean;
{autenticação e leitura de dados do usuário}
var Qry: tZQuery;
begin
     result := false;
     if (nm<>'') and (psw<>'') then
     begin
          try
             Qry:=Query.QueryCreate('select * from appl_usuarios where nome='+quotedstr(nm)+' and senha='+quotedstr(psw),true);
             if Qry.RecordCount<>0 then
             begin
                  result := true;
                  with usuario do
                  begin
                       nome := Qry.FieldByName('nome').AsString;
                       perfil:= Qry.FieldByName('perfil').AsString;
                  end;
             end;
          finally
                 Query.QueryDestroy(Qry);
          end;
     end;
end;

Procedure TInicioDB.LerDadosInstituicao;
{Lê e armazena os dados da instituição}
var Qry: tZQuery;
    BlobStream: tStream;
    Jpg: tJpegImage; // O formato da logo será JPG
begin
     try
        Qry:=Query.QueryCreate('Select * from usr_instituicao',true);
        with instituicao do
        begin
             nome:=Qry.FieldByName('nome').asstring;
             rua:=Qry.FieldByName('rua').asstring;
             numero:=Qry.FieldByName('numero').asstring;
             compl:=Qry.FieldByName('compl').asstring;
             bairro:=Qry.FieldByName('bairro').asstring;
             cidade:=Qry.FieldByName('cidade').asstring;
             uf:=Qry.FieldByName('uf').asstring;
             cep:=Qry.FieldByName('cep').asstring;
             {logo.clear;
             if not Qry.FieldByName('LOGO').isNull then
             begin
                  BlobStream:=Qry.CreateBlobStream(Qry.FieldByName('LOGO'),bmRead);
                  try
                     Jpg:= TJPEGImage.Create;
                     Jpg.LoadFromStream(BlobStream);
                     logo.Assign(Jpg);
                  finally
                     BlobStream.Free;
                     Jpg.Free;
                  end;
             end;}
        end;
     finally
            Query.QueryDestroy(Qry);
     end;
end;

end.

