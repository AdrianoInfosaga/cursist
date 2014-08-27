{ Unit dbinicio.pas
  ===========================================================
  *inicialização da base de dados e autenticação de usuário*
  ===========================================================
  Comportamentos:
  - Lê localização da base de dados no arquivo config.ini
  - Conecta a base de dados
  - Apresenta tela tipo splash para login
  - Le para a memória logo e dados da instituição
  - Autentica o usuário
  - Le para a memória dados de login e perfil do usuário
  - Insere registros para sequencias dos campos id / cod de todas as tabelas
  - Insere usuários MASTER(adiministrador) e ROOT(manutenção)
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
    login: string[20];
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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    fDbUtil: tQueryUtil;
    vPastaDB: string;
    vPastaSistema: string;
    vUsuario: tUsuario;
    vInstituicao: tInstituicao;
    vLogo: tPicture;
    procedure AdicionaSequenciaTabelasAuto;
    procedure AdicionaUsuariosDefault;
    function GetIdSequencia(nomesequencia: string): integer;
    function GetNextSequence(nomesequencia: string ): integer;
    procedure InsereSequencia(nometabela, nomecampo: string; id: integer);
    procedure IniciaDB(sqlDB: tZConnection);
    procedure LerDadosInstituicao;
    procedure ReadIniFile;
    procedure setlogo(AValue: tPicture);
    function VerificaLogin(nm, psw: string): boolean;
  public
        Property PastaDB: string read vPastaDB write vPastaDB;
        Property PastaSistema: string read vPastaSistema write vPastaSistema;
        Property Query: tQueryUtil read fDbUtil write fDbUtil;
        Property Usuario: tUsuario read vUsuario write vUsuario;
        Property Instituicao: tInstituicao read vInstituicao write vInstituicao;
        Property Logo: tPicture read vLogo write setlogo;
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
  DefinirBoxAvisos;
  PastaDB := '';
  PastaSistema := '';
  Query:=tQueryUtil.Create(MainDB);
  with usuario do
  begin
       login := '';
       perfil:='';
  end;
  logo := tPicture.Create;
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

procedure TInicioDB.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  inherited;
  FreeAndNil(vLogo);
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

procedure TInicioDB.setlogo(AValue: tPicture);
begin
  if vLogo=AValue then Exit;
  vLogo:=AValue;
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
     //sqlDB.TransactIsolationLevel:=tiReadCommitted;
     try
        sqlDB.Connected:=true;
     Except
           Raise Exception.Create('Não foi possível conectar a '+sqlDB.Database+' em '+sqlDb.HostName);
     end;
     AdicionaSequenciaTabelasAuto;
     AdicionaUsuariosDefault;
     //AdicionaOutrosDados;
end;

function TInicioDB.GetIdSequencia(nomesequencia: string): integer;
{ retorna o id da sequencia ou 0 caso não exista }
begin
     result := Query.BuscaUmdadoSqlasInteger('select id from appl_sequencias where nometabela='+qstr(nomesequencia));
end;

Function TInicioDB.GetNextSequence(nomesequencia: string):integer;
{ incrementa sequência e retorna valor, retorna msg erro caso sequência não exista }
var
  id_seq, pos: Integer;
begin
     pos := 0;
     id_seq := GetIdSequencia(nomesequencia);
     if id_seq<>0 then // le e incrementa sequencia existente
     begin
         pos := query.buscaumdadosqlasinteger( 'select posicao from appl_sequencias where id='+inttostr(id_seq));
         query.execsql( 'update appl_sequencias set posicao=posicao+1, dtmodificacao='+datetosql(date)+' where id='+inttostr(id_seq));
         inc(pos);
     end
     Else
        GeraException('Sequência '+nomesequencia+' não encontrada!');
     result := pos;
end;

procedure TInicioDB.InsereSequencia(nometabela,nomecampo:string; id:integer);
{ insere registro da nova sequencia }
var pos: integer;
begin
     pos:=query.buscaumdadosqlasinteger('select max('+nomecampo+') from '+nometabela);
     query.execsql( 'insert into appl_sequencias (id, nometabela, posicao, dtcadastro) '+
                    'values('+inttostr(id)+','+qstr(nometabela+':'+nomecampo)+','+inttostr(pos)+','+datetosql(date)+')');
end;

Procedure TInicioDB.AdicionaSequenciaTabelasAuto;
{ gerar automaticamente as sequencias para as tabelas de dados }
var tabelas: tZquery;
    nometabela: String;
    id_seq: Integer;
///
   Procedure SequenciasParaCampo( tabela, campo:string );
   begin
        if query.campoexiste( tabela,campo ) then
        begin
             if GetIdSequencia(nometabela+':'+campo)=0 then //testa se sequencia tabela:id existe
             begin
                  id_seq:=GetNextSequence('appl_sequencias:id'); //obtem posicao nova sequencia
                  inseresequencia(nometabela, campo, id_seq);
             end;
        end;
   end;
///
begin
     // testar sequencia appl_sequencias:id
     try
        screen.Cursor:=crSqlWait;
        query.begintransaction;
        if GetIdSequencia('appl_sequencias:id')=0 then
        begin
             inseresequencia('appl_sequencias','id',1); // insere sequencia appl_sequencias:id
             GetNextSequence('appl_sequencias:id'); // incrementa a posicao da appl_sequencias:id
        end;
        try
           tabelas:=query.listanomestabelas;
           while not tabelas.eof do
           begin
                nometabela:=tabelas.FieldByName('name').asstring;
                sequenciasparacampo( nometabela , 'id' );
                sequenciasparacampo( nometabela , 'cod' );
                tabelas.next;
           end;
        finally
            query.querydestroy(tabelas);
            screen.Cursor:=crDefault;
        end;
        query.commit;
     Except
           query.rollback;
     end;
end;

Procedure TInicioDB.AdicionaUsuariosDefault;
{ adicionar usuários default inicialização do sistema: MASTER / ROOT }
///
   procedure adicionausuariosistema(nm,psw,prfl:string);
   var id: integer;
   begin
        if query.buscaumdadosqlasinteger('select count(*) from appl_usuarios where login='+qstr(nm)+' and tiporegistro='+qstr('S'))=0 then
        begin
             id := getnextsequence('appl_usuarios:id');
             query.ExecSql('insert into appl_usuarios (id,login,senha,perfil,dtcadastro,tiporegistro) '+
                           'values('+inttostr(id)+','+qstr(nm)+','+qstr(psw)+','+qstr(prfl)+','+datetosql(date)+','+qstr('S')+')');
        end;
   end;
///
begin
  try
     try
        screen.cursor:=crSqlWait;
        query.begintransaction;
        adicionausuariosistema('MASTER','MASTER','A');
        adicionausuariosistema('ROOT','54321','M');
        query.commit;
     finally
         screen.cursor:=crDefault;
     end;
  except
      query.rollback;
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
             Qry:=Query.QueryCreate('select * from appl_usuarios where login='+quotedstr(nm)+' and senha='+quotedstr(psw));
             if Qry.RecordCount<>0 then
             begin
                  result := true;
                  with usuario do
                  begin
                       login := Qry.FieldByName('login').AsString;
                       perfil:= Qry.FieldByName('perfil').AsString;
                  end;
             end;
          finally
                 Query.QueryDestroy(Qry);
          end;
     end;
end;

procedure TInicioDB.LerDadosInstituicao;
{Lê e armazena os dados da instituição}
var Qry: tZQuery;
    BlobStream: tStream;
    Jpg: tJpegImage; // formato da logo: JPG
begin
     try
        Qry:=Query.QueryCreate('Select * from usr_instituicao');
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
        end;
        logo.clear;
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
        end;
     finally
            Query.QueryDestroy(Qry);
     end;
end;

end.

