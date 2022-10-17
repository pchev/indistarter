unit indigui_connect;

{$mode objfpc}{$H+}

interface

uses pu_indigui, u_utils, XMLConf, LazFileUtils, LCLType,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Menus;

type

  { Tf_connect }

  Tf_connect = class(TForm)
    BtnConnect: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MainMenu1: TMainMenu;
    MenuHelp: TMenuItem;
    ServerHost: TComboBox;
    ServerPort: TComboBox;
    Timer1: TTimer;
    procedure BtnConnectClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure MenuHelpClick(Sender: TObject);
    procedure ServerHostKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ServerPortKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Timer1Timer(Sender: TObject);
  private
    autoconnect: boolean;
    config: TXMLConfig;
    MaxMRU: integer;
    ConfigDir,configfile: string;
    procedure LoadConfig;
    procedure SaveConfig;
    procedure AddMRU(cb: TComboBox; value: string);
  public
    procedure Init;
  end;

var
  f_connect: Tf_connect;

implementation

{$R *.lfm}
{$i revision.inc}

{ Tf_connect }

procedure Tf_connect.FormCreate(Sender: TObject);
begin
  MaxMRU:=10;
  config:=TXMLConfig.Create(self);
  ConfigDir:=GetAppConfigDirUTF8(false,true);
  configfile:='indigui.conf';
  config.Filename:=slash(ConfigDir)+configfile;
  LoadConfig;
  autoconnect:=true;
  if Application.HasOption('h', 'host') then begin
    ServerHost.Text:=Application.GetOptionValue('h', 'host');
  end;
  if Application.HasOption('p', 'port') then begin
    ServerPort.Text:=Application.GetOptionValue('p', 'port');
  end;
  if Application.HasOption('n', 'noconnect') then begin
    autoconnect:=false;
  end;
end;

procedure Tf_connect.LoadConfig;
var i,n: integer;
begin
  ServerHost.Clear;
  n:=config.GetValue('/Host/Num',0);
  for i:=1 to n do begin
    ServerHost.Items.Add(config.GetValue('/Host/Host'+inttostr(i),''));
  end;
  if ServerHost.Items.Count=0 then
    ServerHost.Items.Add('localhost');
  ServerHost.ItemIndex:=0;
  ServerPort.Clear;
  n:=config.GetValue('/Port/Num',0);
  for i:=1 to n do begin
    ServerPort.Items.Add(config.GetValue('/Port/Port'+inttostr(i),''));
  end;
  if ServerPort.Items.Count=0 then
    ServerPort.Items.Add('7624');
  ServerPort.ItemIndex:=0;
end;

procedure Tf_connect.SaveConfig;
var i,n: integer;
begin
  n:=ServerHost.Items.Count;
  config.SetValue('/Host/Num',n);
  for i:=0 to n-1 do begin
    config.SetValue('/Host/Host'+inttostr(i+1),ServerHost.Items[i]);
  end;
  n:=ServerPort.Items.Count;
  config.SetValue('/Port/Num',n);
  for i:=0 to n-1 do begin
    config.SetValue('/Port/Port'+inttostr(i+1),ServerPort.Items[i]);
  end;
  config.Flush;
end;

procedure Tf_connect.Init;
begin
   Caption:='INDI Client '+starter_version+'-'+RevisionStr;
   if autoconnect then timer1.Enabled:=true;
end;

procedure Tf_connect.Timer1Timer(Sender: TObject);
begin
  timer1.Enabled:=false;
  BtnConnectClick(Sender);
end;

procedure Tf_connect.AddMRU(cb: TComboBox; value: string);
var i: integer;
begin
  i := cb.Items.IndexOf(value);
  if (i < 0) and (cb.Items.Count >= MaxMRU) then
    i := MaxMRU - 1;
  if i >= 0 then
    cb.Items.Delete(i);
  cb.Items.Insert(0, value);
  cb.ItemIndex := 0;
end;

procedure Tf_connect.BtnConnectClick(Sender: TObject);
var h,p: string;
begin
  h:=ServerHost.Text;
  if trim(h)='' then exit;
  AddMRU(ServerHost,h);
  p:=ServerPort.Text;
  if trim(p)='' then exit;
  AddMRU(ServerPort,p);
  f_indigui:=Tf_indigui.Create(self);
  f_indigui.Caption:='INDI Client '+starter_version+'-'+RevisionStr;
  f_indigui.IndiServer:=h;
  f_indigui.IndiPort:=p;
  hide;
  f_indigui.ShowModal;
  if not f_indigui.ConnectedServer then
    show
  else begin
    if f_indigui.DisconnectedServer then ShowMessage('Disconnected from server');
    close;
  end;
end;

procedure Tf_connect.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveConfig;
end;

procedure Tf_connect.MenuHelpClick(Sender: TObject);
var aboutmsg,cdate: string;
begin
cdate:={$I %DATE%};
cdate:=copy(cdate,1,4);
aboutmsg:='INDI Client '+starter_version+'-'+RevisionStr+crlf;
aboutmsg:=aboutmsg+'A simple INDI client GUI'+crlf;
aboutmsg:=aboutmsg+crlf;
aboutmsg:=aboutmsg+'Options:'+crlf;
aboutmsg:=aboutmsg+'-h --host hostname :'+tab+'INDI server host name'+crlf;
aboutmsg:=aboutmsg+'-p --port port :    '+tab+tab+'INDI server port number'+crlf;
aboutmsg:=aboutmsg+'-n --noconnect :    '+tab+tab+'Do not try to auto-connect to localhost'+crlf;
aboutmsg:=aboutmsg+crlf;
aboutmsg:=aboutmsg+'Copyright (C) '+cdate+' Patrick Chevalley'+crlf;
aboutmsg:=aboutmsg+'http://www.ap-i.net'+crlf;
aboutmsg:=aboutmsg+crlf;
aboutmsg:=aboutmsg+'This program is free software; you can redistribute it and/or'+crlf;
aboutmsg:=aboutmsg+'modify it under the terms of the GNU General Public License'+crlf;
aboutmsg:=aboutmsg+'as published by the Free Software Foundation; either version 3'+crlf;
aboutmsg:=aboutmsg+'of the License, or (at your option) any later version.'+crlf;
ShowMessage(aboutmsg);
end;

procedure Tf_connect.ServerHostKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key=VK_RETURN then BtnConnectClick(Sender);
end;

procedure Tf_connect.ServerPortKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key=VK_RETURN then BtnConnectClick(Sender);
end;


end.

