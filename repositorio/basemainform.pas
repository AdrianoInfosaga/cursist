{ Unit basemainform.pas
  =====================
  *controles básicos de formulário*
  ==================================
  comportamentos:
  - self.KeyPreview:=True;
  - self.Position:=poDesktopCenter;
  - self.DoubleBuffered:=true;
  - implementa alteração de cor do campo de edição ativo
  - implementa melhorias na consistencia de campos data
  - atribui a RxDbGrid.options os valores dgConfirmDelete e dgRowHighlight
  - Public procedure GeraException - janela para msg erro / levanta exceção
  - implementa propriedade LastActiveControl
  - implementa formkeydown - uso da tecla ESC para sair / fechar formulário
  - padroniza o formato para datas
  ==================================
  Adriano Alexandre Adami - 2014
  http://contribuicoes.wordpress.com
}

unit basemainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  {adicionadas} stdCtrls,  Grids, rxdbgrid, DBGrids, dbdateedit,
  editbtn, windows;

type

  { OnEventObject }
  OnEventObject = class
  private
    f_cmp : tcomponent;
    f_ent : TNotifyEvent ;
    f_ext : TNotifyEvent ;
  Public
        constructor create( Cmp: tComponent );
        Property Component: tComponent read f_cmp write f_cmp;
        Property OnEnter: tNotifyEvent Read f_ent Write f_ent;
        Property OnExit: tNotifyEvent Read f_ext Write f_ext;
  end;


  { TFrmBaseMain }
  TFrmBaseMain = class(TForm)
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    fLastActiveControl: tWinControl; // componente com foco anterior ao atual
    OnEventList: tList;
    procedure DefaultDateOnExit(Sender: TObject);
    procedure DefaultOnExit(Sender: TObject);
    function GetEventObject(cmp: tcomponent): onEventObject;
    procedure DefaultOnEnter(sender: tObject);
    procedure DefineEnterExit(wCntrl: tWinControl);
    procedure InicializaComponentes;
    procedure OnEventListDestroy;
  public
    procedure GeraException(msg: string);
  published
    Property LastActiveControl: tWinControl Read fLastActiveControl Write fLastActiveControl;
  end;

var
  FrmBaseMain: TFrmBaseMain;

implementation

Uses Uteis;

{$R *.lfm}

{ OnEventObject }

constructor OnEventObject.create(cmp: tComponent);
begin
     Component := cmp;
     if assigned ( twincontrol(cmp).onEnter ) then
        OnEnter := twincontrol(cmp).onEnter;
     if assigned ( twincontrol(cmp).onExit ) then
        onExit := twincontrol(cmp).onExit;
end;

{ TFrmBaseMain }

procedure TFrmBaseMain.FormCreate(Sender: TObject);
begin
  inherited;
  shortdateformat:='dd/mm/yyyy';
  self.KeyPreview:=True;
  self.Position:=poDesktopCenter;
  self.DoubleBuffered:=true;
  OnEventList:=tList.Create; // armazenar onEventObject
  InicializaComponentes;
end;

procedure TFrmBaseMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
     inherited;
     if Key=VK_ESCAPE then
     begin
          self.close;
     end;
end;

procedure TFrmBaseMain.InicializaComponentes;
{ definir comportamento padrão de componentes:
 - Componentes de edição: altera cor ao entrar e sair,
 - Melhora a consistencia de datas,
 - Define os conjunto de atributos RxDbGrid.options }
var qtd, x: integer;
begin
     qtd := self.ComponentCount;
     for x := 0 to qtd - 1 do
     begin
          if self.Components[x].InheritsFrom(tCustomEdit) then
             DefineEnterExit( tWinControl(self.Components[x]) )
          Else
          if self.Components[x].InheritsFrom(tComboBox) then
             DefineEnterExit( tWinControl(self.Components[x]) )
          Else
          if self.Components[x].InheritsFrom(tCustomGrid) then
          BEGIN
               DefineEnterExit( tWinControl(self.Components[x]) );
               if self.Components[x].InheritsFrom(tRxDbGrid) then
                  tRxDbGrid(self.Components[x]).Options := [dgEditing,dgTitles,dgIndicator,dgColumnResize,dgColumnMove,dgColLines,dgRowLines,dgAlwaysShowSelection,dgConfirmDelete,dgCancelOnExit,dgRowHighlight];
          end;
     end;
end;

procedure TFrmBaseMain.GeraException(msg: string);
begin
     if trim(msg)<>'' then // só eleva exceção se existir uma msg de erro válida.
     begin
          try
             showmessage( msg );
             abort;
          except
                raise;
          end;
     end;
end;

procedure TFrmBaseMain.DefineEnterExit(wCntrl: tWinControl);
{ definir eventos onenter / onexit }
///
   Procedure AddEventList ;
   { guardar evento onenter / onexit do usuário }
   var obj: OnEventObject;
   begin
        obj := OnEventObject.create( wCntrl );
        OnEventList.Add(obj);
   end;
///
begin
     if (wCntrl.TabStop) then
     begin
          if assigned ( wCntrl.onEnter ) or assigned ( wCntrl.OnExit ) then
             AddEventList;
          wCntrl.onEnter:=@DefaultOnEnter;
          if (wCntrl.ClassType=tDBDateEdit) or (wCntrl.ClassType=tDateEdit) then
             wCntrl.onExit:=@DefaultDateOnExit
          Else
             wCntrl.onExit:=@DefaultOnExit;
     end;
end;

procedure TFrmBaseMain.DefaultOnEnter(sender: tObject);
{ comportamento: altera cor do componente }
var Obj : OnEventObject;
begin
     tWinControl(sender).Color:=$00DFFFFE; //muda cor do componente
     Obj := GetEventObject( tComponent(sender) );
     if assigned( Obj ) then
        if assigned (Obj.OnEnter) then
           Obj.OnEnter( sender ) ;
end;

function TFrmBaseMain.GetEventObject(cmp: tcomponent): onEventObject;
{busca o OnEventObject para o componente}
var mx,x: integer;
    obj : OnEventObject ;
begin
     result:=Nil;
     mx := OnEventList.Count;
     for x := 0 to mx - 1 do
     begin
          obj := OnEventObject(OnEventList[x]); // onchangeobject ( pointer )
          if obj.component = cmp then
          begin
             result := obj;
             break;
          end;
     end;
end;

procedure TFrmBaseMain.DefaultOnExit(Sender: TObject);
{comportamento:
-altera cor do componente,
-salva lastactivecontrol}
var Obj: OnEventObject;
begin
     tWinControl(sender).Color:=clWhite; //muda cor do componente
     Obj := GetEventObject( tComponent(sender) );
     if assigned( Obj ) then
        if assigned( Obj.OnExit ) then
           Obj.OnExit( sender ) ;
     self.LastActiveControl:=tWinControl(sender); //salva informação de foco
end;

procedure TFrmBaseMain.DefaultDateOnExit(Sender: TObject);
{comportamento:
-implementa melhorias na consistencia de datas,
-altera cor do componente,
-salva lastactivecontrol}
var dt: string;
begin
     try
        dt := ExtrairNumeros (tDateEdit( sender ).Text);
        if dt<>'' then // data não esta vazia
        begin
             if Not HasTextaValidDate( dt ) then
                GeraException ('Data Inválida!');
        end;
        DefaultOnExit(Sender); // Executa procedimento default onexit;
     except
           self.ActiveControl:=tWinControl(sender); // mantém foco no componente
           raise;
     end;
end;

procedure TFrmBaseMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
     inherited;
     OnEventListDestroy;
end;

procedure TFrmBaseMain.OnEventListDestroy;
{destroi OnEventList e objetos relacionados}
var mx, x: integer;
    Obj : OnEventObject ;
begin
     mx := OnEventList.Count;
     for x := 0 to mx-1 do
     begin
          Obj := OnEventObject ( OnEventList[x] ) ;
          FreeAndnil( Obj );
          OnEventList[X]:=Nil;
     end;
     OnEventList.Clear;
     FreeAndNil(OnEventList);
end;

end.

